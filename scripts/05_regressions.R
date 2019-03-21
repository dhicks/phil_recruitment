# This script generates the regression models, as well as rootograms to assess goodness-of-fit

## TODO:  
## - course division
##     - import Google Sheet
##     - rerun models
## - rootogram stuff
##     - yardstick: AUC calculations; sensitivity rates? 
##     - perusable display of rootograms
##     - pass sets of models to `rootogram` (eg, all count models for a given focal_var) and get back a smaller number of rootograms
##     - pass list of model names to use in legend

library(tidyverse)

# library(lme4)
library(brglm2)
# library(MASS)
library(pscl)
library(broom)

library(googlesheets)

library(tictoc)

## Rootograms
source('../R/rootogram.R')
source('../R/rootogram_binom.R')
source('../R/rootogram_count.R')

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
    sheet_df = gs_read(sheet) %>%
        mutate(instructor.log_total = log10(n)) %>%
        dplyr::select(-n, -notes) %>%
        ## Reconcile demographic variables w/ student versions
        mutate(race = fct_relevel(race, 'White'), 
               gender = fct_relevel(gender, 'M'))
    write_rds(sheet_df, str_c(insecure_data_folder, faculty_file))
} else {
    sheet_df = read_rds(str_c(insecure_data_folder, faculty_file))
}

dataf = read_rds(str_c(data_folder, '03_analysis_df.Rds')) %>%
    left_join(sheet_df, by = 'instructor', 
              suffix = c('', '.instructor')) %>%
    filter(!is.na(race.instructor))

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
    'first_gen', 'student background',
    'low_income', 'student background', 
    'dmg', 'grade gap',
    'grade_diff', 'grade gap',
    'gender.instructor', 'instructor effects',
    'race.instructor', 'instructor effects',
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
construct_expr = function(model_type, reg_form, 
                          data_arg = 'data = train_df') {
    ## To add a new model type:  
    ## 1. Add to vector model_types
    ## 2. Add case to fn
    ## 3. Add case to other_args
    model_types = c('lm', 'bias-reduced logistic', 'hurdle', 'logistic', 'Poisson')
    if (!all(model_type %in% model_types)) {
        stop(paste('unknown model type'))
    }
    
    fn = case_when(model_type == 'lm' ~ 'lm', 
                   model_type == 'logistic' ~ 'glm',
                   model_type == 'bias-reduced logistic' ~ 'glm',
                   model_type == 'Poisson' ~ 'glm',
                   model_type == 'hurdle' ~ 'hurdle')
    other_args = case_when(model_type == 'lm' ~ '', 
                           model_type == 'logistic' ~ 'family = binomial',
                           model_type == 'bias-reduced logistic' ~ 'family = binomial, method = "brglmFit"',
                           model_type == 'Poisson' ~ 'family = poisson',
                           model_type == 'hurdle' ~ "dist = 'negbin', trace = 1")
    inner = ifelse(other_args == '', 
                   str_c(reg_form, data_arg, sep = ', '), 
                   str_c(reg_form, data_arg, other_args, sep = ', '))
    
    comb = str_c(fn, '(', inner, ')')
    # expr = as.expression(comb)
    return(comb)
}

# construct_expr('logistic', 'monkey + zoo')

## lm, logistic, bias-reduced logistic, Poisson, and hurdle:  ~55 sec
## NB brglmFit warnings are due to separation w/ instructor demographics
tic()
models = reg_form %>% 
    # slice(10:12) %>%
    mutate(expr = map2(model_type, reg_form, construct_expr)) %>% 
    mutate(model = map(expr, ~eval(parse(text = .))))
toc()


## Rootograms ---
rootogram_df =  models %>% 
    mutate(rootogram = map2(outcome, model, 
                            ~ rootogram(.x, .y, #threshold = .2,
                                        new_data = train_df))) %>% 
    rowwise() %>% 
    mutate(rootogram = list(rootogram + 
                                ggtitle(str_c(focal_var, ': ', model_type)))) %>% 
    select(-model)

rootogram('ever_phil', models$model[[3]], threshold = .5,
          new_data = train_df)
# rootogram_df$rootogram[[19]]

# library(cowplot)
# models %>% 
#     filter(outcome == 'ever_phil') %>% 
#     split(.$focal_var) %>% 
#     map(pull, rootogram) %>% 
#     flatten() %>% 
#     plot_grid(plotlist = ., 
#               labels = names(.))



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
               color = race, shape = gender)) +
        geom_pointrange(position = position_dodge(width = .25)) +
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

