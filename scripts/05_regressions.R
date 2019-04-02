# This script generates the regression models, as well as rootograms to assess goodness-of-fit

## TODO:  
## - estimate extraction
##      - what we have now works; but it's hard to read, especially for grade_diff (which has two different control sets)
##          - logistic and bias-reduced logistic can be put on the same scale
##          - likewise w/ Poisson and hurdle count
##      - shade background for "substantial," "moderate," and "negligible" effect regions:  https://stackoverflow.com/questions/9968975/make-the-background-of-a-graph-different-colours-in-different-regions
##          - can a dataframe be used to set different regions for different model types? 

library(tidyverse)

# library(lme4)
library(brglm2)
# library(MASS)
library(pscl)
library(broom)
library(yardstick)
## See "Relevant level" section of ?yardstick::roc_auc
options('yardstick.event_first' = FALSE)

library(googlesheets)

library(tictoc)

## Rootograms
# source('../R/rootogram.R')
# source('../R/rootogram_binom.R')
source('../R/rootogram_count.R')

library(cowplot)
theme_set(theme_bw())

## Function for extracting estimates for a process and properly combining demographic interactions
source('../R/extract_estimates.R')

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
insecure_data_folder = '../data_insecure/'
plots_folder = '../plots/'

test_share = .25 ## fraction of observations in test set

plot_filters = quos(ci.high < 3, is.finite(se.comb)) ## used to filter plot-breaking values before generating effects estimates plots


## Load data ----
## Instructor demographics
faculty_file = 'faculty.Rds'
if (!file.exists(str_c(insecure_data_folder, faculty_file))) {
    ## NB requires Google authentication
    # gs_auth()
    sheet = gs_key('1EKE_2xHC1Ju38qfPTpSoKdiuw1-VZnKHGyJCyzsmPUo')
    faculty_df = gs_read(sheet) %>%
        mutate(instructor.log_total = log10(n)) %>%
        dplyr::select(-n, -notes) %>%
        ## Reconcile demographic variables w/ student versions
        mutate(race = fct_relevel(race, 'White'), 
               gender = fct_relevel(gender, 'M'))
    write_rds(faculty_df, str_c(insecure_data_folder, faculty_file))
} else {
    faculty_df = read_rds(str_c(insecure_data_folder, faculty_file))
}

## Course division
course_division_file = 'course_division.Rds'
if (!file.exists(str_c(insecure_data_folder, course_division_file))) {
    ## NB requires Google authentication
    # gs_auth()
    sheet = gs_key('1tzLYJzW4qADjq48J9WfPax_-cIUTeIJOMbA3TZc215g')
    course_division_df = gs_read(sheet) %>%
        dplyr::select(title, division)
    write_rds(course_division_df, str_c(insecure_data_folder, 
                                        course_division_file))
} else {
    course_division_df = read_rds(str_c(insecure_data_folder, 
                                        course_division_file))
}

dataf = read_rds(str_c(data_folder, '03_analysis_df.Rds')) %>%
    left_join(faculty_df, by = 'instructor', 
              suffix = c('', '.instructor')) %>%
    left_join(course_division_df, by = 'title') %>% 
    rename(course_division = division) %>% 
    filter(!is.na(race.instructor), 
           !is.na(course_division))

## Regression formulas and model types ----
model_types = tribble(
    ~ outcome, ~ model_type,
    'ever_phil', 'lm',
    'ever_phil', 'logistic',
    'ever_phil', 'bias-reduced logistic',
    'n_later_phil', 'Poisson',
    'n_later_phil', 'hurdle'
)

covar_groups = tribble(
    ~ covar, ~ covar_group, 
    'admission_type', 'student background',
    'course_division', 'curriculum',
    'first_gen', 'student background',
    'low_income', 'student background', 
    'undeclared.student', 'student background',
    'dmg', 'grade gap',
    'grade_diff', 'grade gap',
    'gender.instructor', 'instructor effects',
    'race.instructor', 'instructor effects',
    'current_phil_share', 'peer effects',
    'n_students', 'peer effects',
    'poc_share', 'peer effects',
    'women_share', 'peer effects'
)

reg_form = read_rds(str_c(insecure_data_folder, '04_reg_form.Rds')) %>% 
    left_join(model_types) %>% 
    left_join(covar_groups, by = c('focal_var' = 'covar')) %>% 
    mutate(model_idx = as.character(row_number()))


## Training and testing sets ----
set.seed(2019-02-14)
test_rows = dataf %>%
    mutate(row_idx = row_number()) %>%
    group_by(ever_phil, demographic) %>%
    sample_frac(size = test_share) %>%
    pull(row_idx)

train_df = dataf[-test_rows,]
test_df = dataf[test_rows,]


## Fit models ----
construct_expr = function(model_type, 
                          reg_form, 
                          data_arg = 'data = train_df') {
    ## To add a new model type:  
    ## 1. Add name/label to model_types
    ## 2. Add case to fn
    ## 3. If needed, add case to other_args
    model_types = c('lm', 
                    'logistic', 
                    'bias-reduced logistic', 
                    'Poisson',
                    'hurdle')
    if (!all(model_type %in% model_types)) {
        stop(paste('unknown model type'))
    }
    
    fn = case_when(model_type == 'lm' ~ 'lm', 
                   model_type == 'logistic' ~ 'glm',
                   model_type == 'bias-reduced logistic' ~ 'glm',
                   model_type == 'Poisson' ~ 'glm',
                   model_type == 'hurdle' ~ 'hurdle')
    other_args = case_when(model_type == 'logistic' ~ 'family = binomial',
                           model_type == 'bias-reduced logistic' ~ 'family = binomial, method = "brglmFit", type = "AS_mean"',
                           model_type == 'Poisson' ~ 'family = poisson',
                           model_type == 'hurdle' ~ "dist = 'negbin', trace = 1", 
                           TRUE ~ '')
    inner = ifelse(other_args == '', 
                   str_c(reg_form, data_arg, sep = ', '), 
                   str_c(reg_form, data_arg, other_args, sep = ', '))
    
    comb = str_c(fn, '(', inner, ')')
    # expr = as.expression(comb)
    return(comb)
}

# construct_expr('logistic', 'monkey + zoo')

## lm, logistic, bias-reduced logistic, Poisson, and hurdle:  ~80 sec
## NB brglmFit warnings are due to separation w/ instructor demographics
tic()
models = reg_form %>% 
    # slice(10:12) %>%
    mutate(expr = map2(model_type, reg_form, construct_expr)) %>% 
    mutate(model = map(expr, ~eval(parse(text = .))))
toc()



## ROC AUC for ever_phil models ----
## ~ 63 sec
tic()
roc_df = models %>% 
    filter(outcome == 'ever_phil') %>% 
    mutate(data = map(model, augment),
           roc_auc = map(data, 
                         ~roc_auc(., .fitted, 
                                  truth = as.factor(ever_phil))), 
           roc_curve = map(data, 
                           ~roc_curve(., .fitted, 
                                      truth = as.factor(ever_phil))))
toc()

## ROC curves indicate that the three different specifications perform very similarly, and substantially better than a coin flip
roc_df %>% 
    unnest(roc_curve) %>% 
    ggplot(aes(1 - specificity, sensitivity, 
               group = model_idx, 
               color = model_type)) +
    geom_line() +
    stat_function(fun = identity, inherit.aes = FALSE, 
                  color = 'black') +
    facet_wrap(~ focal_var) +
    ggtitle('ROC for ever_phil models', 
            subtitle = Sys.time())

ggsave(str_c(plots_folder, '05_roc.png'), 
       height = 4*3, width = 5*3)

## AUC values indicate that the two logistic specifications are almost identical, and tend to perform slightly better than the linear probability model.  However, all models have moderate-good AUC between .75 and .85.  
roc_df %>% 
    unnest(roc_auc) %>% 
    ggplot(aes(focal_var, .estimate, 
               color = model_type)) +
    geom_point(position = position_dodge(.25)) +
    coord_flip() +
    ggtitle('ROC AUC for ever_phil models', 
            subtitle = Sys.time())

ggsave(str_c(plots_folder, '05_auc.png'), 
       height = 6, width = 6)

## Threshold-sensitivity curves
## These curves show how sensitivity or recall (skill at correctly predicting that a student will major) changes as we vary the prediction threshold
## Across all models, moderately high recall requires a very low threshold <.1
## This includes bias-reduced logistic models, which are supposed to avoid this problem
roc_df %>% 
    unnest(roc_curve) %>% 
    filter(is.finite(.threshold)) %>% 
    mutate(.threshold = ifelse(model_type != 'lm', 
                               boot::inv.logit(.threshold), 
                               .threshold)) %>% 
    ggplot(aes(.threshold, sensitivity, 
               group = model_idx, 
               color = model_type)) +
    geom_line() +
    geom_vline(xintercept = .5) +
    facet_wrap(~ focal_var, scales = 'free') +
    ggtitle('Threshold-sensitivity curves for ever_phil models', 
            subtitle = Sys.time())

ggsave(str_c(plots_folder, '05_thresh_sens.png'), 
       height = 4*3, width = 5*3)



## Rootograms ----
## Because the ever_phil models require such low thresholds, their rootograms (at the "natural" threshold of .5) all predict 100% FALSE
## So we'll only construct them for n_later_phil models
rootogram_df = models %>% 
    filter(outcome == 'n_later_phil') %>% 
    group_by(focal_var) %>% 
    summarize(rootogram = list(rootogram_count(model, 
                                               model_names = model_type,
                                               n_later_phil, 
                                               new_data = test_df))) %>% 
    rowwise() %>% 
    mutate(rootogram = list(rootogram + 
                                scale_color_brewer(name = 'model', 
                                                   palette = 'Set1') +
                                theme_minimal() +
                                ggtitle(focal_var)))

# rootogram_count(models$model[43:44], 
#                 model_names = models$model_type[43:44],
#                 n_later_phil, 
#                 new_data = test_df) +
#     scale_color_brewer(name = 'model', palette = 'Set1') +
#     theme_minimal()

rootogram_legend = get_legend(rootogram_df$rootogram[[1]])
rootogram_title = ggplot() +
    ggtitle('Rootograms for n_later_phil models', 
            subtitle = Sys.time())

rootogram_df %>%
    pull(rootogram) %>% 
    # flatten() %>% 
    map(~ . %+% theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .) %>% 
    plot_grid(rootogram_legend, 
              ncol = 2, 
              rel_widths = c(4, .25)) %>% 
    ## Very hacky way of adding a title to a cowplot composite:  
    ## <https://stackoverflow.com/questions/50973713/ggplot2-creating-themed-title-subtitle-with-cowplot#50975628>
    plot_grid(rootogram_title, ., 
              ncol = 1, rel_heights = c(.25, 4))

ggsave(str_c(plots_folder, '05_rootograms.png'), 
       scale = 1.5, 
       height = 4*2, width = 4.25*3)



## Extract estimates ----
# extract_estimates(models$model[[1]], models$focal_var[[1]])
# extract_estimates(models$model[[20]], models$focal_var[[20]])
# extract_estimates(models$model[[5]], models$focal_var[[5]])

estimates = map2_dfr(models$model, models$focal_var, 
                     extract_estimates, .id = 'model_idx') %>% 
    ## Model metadata
    left_join(reg_form) %>% 
    ## Split hurdle models into 2 types
    mutate(model_type = ifelse(!is.na(hurdle_component),
                               paste(model_type, hurdle_component),
                               model_type)) %>%
    ## Stabilize model type order for ggplot
    mutate(model_type = fct_inorder(model_type)) %>% 
    ## Backtransform non-linear models
    mutate_if(is.numeric,
              ~ ifelse(model_type != 'lm', 
                       exp(.) - 1, 
                       .))

estimates_plot = function(data) {
    ggplot(data = data, 
           aes(x = term, y = estimate.comb,
               ymin = ci.low, ymax = ci.high,
               color = race, shape = gender, 
               group = model_idx)) +
        geom_pointrange(position = position_dodge(width = 1)) +
        geom_hline(yintercept = 0,
                   # data = reg_form,
                   linetype = 'dashed') +
        coord_flip() +
        facet_wrap(process ~ model_type,
                   dir = 'v',
                   scales = 'free_x', 
                   drop = FALSE,
                   nrow = n_distinct(estimates$model_type)
        ) +
        xlab('') +
        ylab('estimated effect') +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_color_brewer(palette = 'Set1') +
        theme_bw() +
        theme(legend.position = 'bottom')
}

# estimates %>% 
#     filter(covar_group == 'instructor effects') %>% 
#     filter() %>% 
#     estimates_plot()

estimates_plots = estimates %>% 
    filter(!!!plot_filters) %>% 
    nest(-covar_group) %>% 
    mutate(plot = map(data, estimates_plot), 
           plot = map2(plot, covar_group, ~ .x + ggtitle(str_to_title(.y))))

## Output ----
write_rds(estimates, str_c(insecure_data_folder, '05_estimates.Rds'))
write_rds(estimates_plots, str_c(insecure_data_folder, '05_estimates_plots.Rds'))

estimates_plots %>% 
    # slice(1) %>%
    mutate(path = str_c(plots_folder, 
                        '05_',
                        str_replace_all(covar_group, ' ', '_'), 
                        '.png')) %>% 
    # {walk(.$plot, ~print(.))}
    {walk2(.$plot, .$path,
          ~ ggsave(filename = .y, plot = .x,
                   width = 5, height = 5,
                   scale = 2))}

