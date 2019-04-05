# This script generates the regression models, as well as ROC plots and rootograms to assess goodness-of-fit

## TODO:  
## - estimate extraction
##      - nice process/covar labels
##      - think through effect size thresholds, plotting cutoffs
##          - unconditional prob. of majoring is 6%
##      - legend width (curriculum, grade gap)
##      - curriculum drops logistic and br logistic models?  maybe try a drop = FALSE somewhere

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
## cowplot makes its own theme the default -_-
theme_set(theme_bw())

## Function for extracting estimates for a process and properly combining demographic interactions
source('../R/extract_estimates.R')
source('../R/estimates_plot.R')

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
insecure_data_folder = '../data_insecure/'
plots_folder = '../plots/'

test_share = .25 ## fraction of observations in test set

plot_filters = quos(ci.high < 4, is.finite(se.comb)) ## used to filter plot-breaking values before generating effects estimates plots


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
           !is.na(course_division)) %>% 
    ## Rescale _share variables as deciles
    mutate_at(vars(contains('_share')), ~ .*10)

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
    # slice(1:6, 85:88) %>% 
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

## ~125 sec
## NB brglmFit warnings are due to separation w/ instructor demographics
tic()
models = reg_form %>% 
    # slice(10:12) %>%
    mutate(expr = map2(model_type, reg_form, construct_expr)) %>% 
    mutate(model = map(expr, ~eval(parse(text = .))))
toc()



## ROC AUC for ever_phil models ----
## ~ 140 sec
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

## ROC curves indicate that the various specifications perform very similarly, and all are substantially better than a coin flip.  
## When there is a difference, interaction models tend to do better than full models, and logistic models do better than linear models.  
## In every case, logistic and br logistic models have basically identical curves. 
roc_df %>% 
    unnest(roc_curve) %>% 
    ggplot(aes(1 - specificity, sensitivity, 
               group = model_idx, 
               color = model_type, 
               linetype = formula)) +
    geom_line() +
    stat_function(fun = identity, inherit.aes = FALSE, 
                  color = 'black') +
    facet_wrap(~ focal_var) +
    ggtitle('ROC for ever_phil models', 
            subtitle = Sys.time())

ggsave(str_c(plots_folder, '05_roc.png'), 
       height = 4*3, width = 5*3)

## AUC values indicate that the two logistic specifications are almost identical, and tend to perform slightly better than the linear probability model.  Interaction models consistently have higher AICs than full models; for some variables full model AUCs are around .70-.73.  Otherwise models have moderate-good AUCs between .75 and .85.  
roc_df %>% 
    unnest(roc_auc) %>% 
    ggplot(aes(focal_var, .estimate, 
               color = model_type, 
               shape = formula)) +
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
               color = model_type, 
               linetype = formula)) +
    geom_line() +
    geom_vline(xintercept = .5) +
    facet_wrap(~ focal_var, scales = 'free') +
    ggtitle('Threshold-sensitivity curves for ever_phil models', 
            subtitle = Sys.time())

ggsave(str_c(plots_folder, '05_thresh_sens.png'), 
       height = 4*3, width = 5*3)



## Rootograms ----
## Because the ever_phil models require such low thresholds, their rootograms (at the "natural" threshold of .5) all predict 100% FALSE
## So we'll only construct rootograms for n_later_phil models

rootogram_df = models %>% 
    filter(outcome == 'n_later_phil') %>% 
    group_by(focal_var) %>% 
    summarize(rootogram = list(rootogram_count(model, 
                                               model_names = interaction(model_type, formula),
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
              rel_widths = c(4, .5)) %>% 
    ## Very hacky way of adding a title to a cowplot composite:  
    ## <https://stackoverflow.com/questions/50973713/ggplot2-creating-themed-title-subtitle-with-cowplot#50975628>
    plot_grid(rootogram_title, ., 
              ncol = 1, rel_heights = c(.25, 4))

ggsave(str_c(plots_folder, '05_rootograms.png'), 
       scale = 1.5, 
       height = 4*2, width = 4.25*3)



## Extract estimates ----
# extract_estimates(models$model[[7]], 
#                   models$focal_var[[7]],
#                   models$formula[[7]] == 'interaction')
# extract_estimates(models$model[[20]], models$focal_var[[20]])
# extract_estimates(models$model[[5]], models$focal_var[[5]], TRUE)

estimates = models %>% 
    transmute(model, focal_var, formula == 'interaction') %>% 
    pmap_dfr(~ extract_estimates(..1, ..2, ..3), 
             .id = 'model_idx') %>% 
    ## Model metadata
    left_join(reg_form) %>% 
    ## Split hurdle models into 2 types
    mutate(model_type = ifelse(!is.na(hurdle_component),
                               paste(model_type, hurdle_component),
                               model_type)) %>%
    ## Group models to reduce facets in plots
    mutate(model_group = case_when(model_type == 'lm' ~ 'linear', 
                                   model_type == 'logistic' ~ 'logistic', 
                                   model_type == 'bias-reduced logistic' ~ 'logistic', 
                                   model_type == 'Poisson' ~ 'count', 
                                   model_type == 'hurdle count' ~ 'count', 
                                   model_type == 'hurdle zero' ~ 'logistic')) %>% 
    ## Stabilize model type and group orders for ggplot
    mutate(model_type = fct_inorder(model_type), 
           model_group = fct_inorder(model_group)) %>% 
    ## Backtransform non-linear models
    mutate_if(is.numeric,
              ~ ifelse(model_type != 'lm', 
                       exp(.) - 1, 
                       .))

# estimates_plot(estimates)
# estimates %>%
#     filter(covar_group == 'instructor effects') %>%
#     filter(!!!plot_filters) %>%
#     estimates_plot()

estimates_plots = estimates %>% 
    filter(!!!plot_filters) %>% 
    nest(-covar_group) %>% 
    mutate(plot = map(data, estimates_plot), 
           plot = map2(plot, covar_group, ~ .x + 
                           ggtitle(str_to_title(.y), 
                                   subtitle = Sys.time())))

## Output ----
write_rds(estimates, str_c(insecure_data_folder, '05_estimates.Rds'))
write_rds(estimates_plots, str_c(insecure_data_folder, '05_estimates_plots.Rds'))

estimates_plots %>% 
    # slice(1) %>%
    mutate(n_processes = map_int(data, ~n_distinct(.$process)), 
           n_model_groups = map_int(data, ~n_distinct(.$model_group)), 
           width = 3*pmax(n_processes, 1.5),
           height = 3*n_model_groups + 3*1/3) %>% 
    mutate(path = str_c(plots_folder, 
                        '05_',
                        str_replace_all(covar_group, ' ', '_'), 
                        '.png')) %>% 
    # {walk(.$plot, ~print(.))}
                        {pwalk(list(.$plot, .$path, .$width, .$height),
                               ~ ggsave(filename = ..2, plot = ..1,
                                        width = ..3, height = ..4,
                                        scale = 1))}

