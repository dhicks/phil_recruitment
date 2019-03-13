# This script generates the regression models, as well as rootograms to assess goodness-of-fit

## TODO:  
## - additional model types (logistic, Poisson)
## - error detection in construct_expr
## - extract estimates

library(tidyverse)

library(lme4)
# library(MASS)
library(pscl)
library(broom)

library(googlesheets)

library(tictoc)

## Rootograms
source('../R/rootogram_binom.R')
source('../R/rootogram_count.R')

## Function for extracting estimates for a process and properly combining demographic interactions
source('../R/extract_estimates.R')

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
insecure_data_folder = '../data_insecure/'
plots_folder = '../plots/'

test_share = .25 ## fraction of observations in test set


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
    'n_later_phil', 'hurdle'
)

reg_form = read_rds(str_c(insecure_data_folder, '04_reg_form.Rds')) %>% 
    left_join(model_types)


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
construct_expr = function(model_type, reg_form) {
    fn = case_when(model_type == 'lm' ~ 'lm', 
                   model_type == 'hurdle' ~ 'hurdle', 
                   TRUE ~ 'error')
    data_arg = 'data = train_df'
    other_args = case_when(model_type == 'lm' ~ '', 
                           model_type == 'hurdle' ~ "dist = 'negbin', trace = 1", 
                           TRUE ~ 'error')
    inner = ifelse(other_args == '', 
                   str_c(reg_form, data_arg, sep = ', '), 
                   str_c(reg_form, data_arg, other_args, sep = ', '))
    
    comb = str_c(fn, '(', inner, ')')
    # expr = as.expression(comb)
    return(comb)
}

## w/ just lm and hurdle, ~20 sec
tic()
models = reg_form %>% 
    # slice(18:19) %>% 
    mutate(expr = map2(model_type, reg_form, construct_expr)) %>% 
    mutate(model = map(expr, ~eval(parse(text = .))))
toc()


## Rootograms ---
rootogram = function(outcome, ## character
                     model,
                     threshold = .5, ...) {
    if (outcome == 'ever_phil') {
        rootogram_binom(model, 
                        response = outcome, 
                        quoted = TRUE,
                        threshold = threshold, ...)
    } else if (outcome == 'n_later_phil') {
        rootogram_count(model, 
                        response = outcome, 
                        quoted = TRUE,
                        ...)
    } else {
        stop('outcome not recognized')
    }
}

models =  models %>% 
    mutate(rootogram = map2(outcome, model, 
                            ~ rootogram(.x, .y, threshold = .2,
                                          new_data = train_df)))

# models$rootogram[[1]]
# rootogram('ever_phil', models$model[[1]], threshold = .2, 
#           new_data = train_df)

# library(cowplot)
# models %>% 
#     filter(outcome == 'ever_phil') %>% 
#     split(.$focal_var) %>% 
#     map(pull, rootogram) %>% 
#     flatten() %>% 
#     plot_grid(plotlist = ., 
#               labels = names(.))


# ## ever_phil models ----
# ever_form = formula(ever_phil ~ 1 + 
#                         demographic +
#                         ## ***Processes we are examining***
#                         ## Class effects
#                         low_income*demographic +
#                         first_gen*demographic +
#                         ## Student background effects
#                         admission_type*demographic +
#                         undeclared.student*demographic +
#                         ## Grade gap effect; Thompson 2017, §2.2.1
#                         grade_diff*demographic +
#                         ## Difference in mean GPA
#                         dmg*demographic +
#                         ## Peer effects
#                         women_share*demographic +
#                         poc_share*demographic +
#                         current_phil_share*demographic +
#                         n_students*demographic +
#                         ## Instructor demographic effects; Thompson 2017, §2.3.1
#                         gender.instructor*demographic +
#                         race.instructor*demographic +
#                         ## ***Controls***
#                         other_major.student +
#                         year + quarter + 
#                         other_major_share + 
#                         mean_grade +
#                         instructor.log_total)
# 
# model_lm = lm(ever_form, data = train_df)
# 
# model_logit = glm(ever_form, 
#                   family = binomial,
#                   data = train_df)
# 
# 
# ## ever_phil rootograms ----
# threshold = .3
# rootogram_binom(model_lm, NULL, ever_phil, threshold) +
#     facet_wrap(~ demographic, scales = 'free') +
#     theme_minimal() +
#     ggtitle(str_c('linear probability: ', threshold, '; training data'),
#             subtitle = Sys.time())
# 
# rootogram_binom(model_lm, test_df, ever_phil, threshold) +
#     facet_wrap(~ demographic, scales = 'free') +
#     theme_minimal() +
#     ggtitle(str_c('linear probability: ', threshold, '; testing data'),
#             subtitle = Sys.time())
# 
# rootogram_binom(model_logit, NULL, ever_phil, threshold, 
#                 delogit = TRUE) +
#     facet_wrap(~ demographic, scales = 'free') +
#     theme_minimal() +
#     ggtitle(str_c('logistic regression: ', threshold, '; training data'), 
#             subtitle = Sys.time())
# 
# rootogram_binom(model_logit, test_df, ever_phil, threshold, 
#                 delogit = TRUE) +
#     facet_wrap(~ demographic, scales = 'free') +
#     theme_minimal() +
#     ggtitle(str_c('logistic regression: ', threshold, '; testing data'), 
#             subtitle = Sys.time())
# 
# 
# ## n_later_phil models ----
# later_form = formula(n_later_phil ~ 1 + 
#                          demographic +
#                          ## ***Processes we are examining***
#                          ## Student background
#                          low_income*demographic +
#                          first_gen*demographic +
#                          admission_type*demographic +
#                          undeclared.student*demographic +
#                          ## Grade gap effect
#                          grade_diff*demographic +
#                          ## Difference in mean grade gap
#                          dmg*demographic +
#                          ## Peer effects
#                          women_share*demographic +
#                          poc_share*demographic +
#                          current_phil_share*demographic +
#                          n_students*demographic +
#                          ## Student-instructor demographics
#                          gender.instructor*demographic +
#                          race.instructor*demographic +
#                          ## ***Controls***
#                          ever_phil + ## NB
#                          other_major.student +
#                          year + quarter +
#                          other_major_share +
#                          mean_grade +
#                          instructor.log_total
# )
# 
# model_pois = glm(later_form, 
#                  family = poisson, 
#                  data = train_df)
# model_nb = MASS::glm.nb(later_form, 
#                         trace = 1, 
#                         data = train_df)
# 
# ## A zero-inflated model represents the data as having "true" and "false" zeroes; in our data this only makes sense if records for a single student were somehow split over two+ different student IDs
# model_zinb = zeroinfl(later_form,
#                       dist = 'negbin',
#                       trace = 1,
#                       data = train_df)
# 
# ## A hurdle model is theoretically appropriate.  But it's not much better than the ordinary negative binomial
# model_hurdle = hurdle(later_form,
#                       dist = 'negbin',
#                       trace = 1,
#                       data = train_df)
# 
# 
# ## n_later_phil rootograms ----
# ## All models overpredict at 1, underpredict in the 2-5 range; 
# ## maybe this is due to missing minors? 
# ## Overdisperson is a clear issue for Poisson
# ## The hurdle does better in the teens, but the plain NB handles the tail better
# rootogram_count(list(#'Poisson' = model_pois, 
#                        # 'n.b.' = model_nb, 
#                        # 'zinb' = model_zinb,
#                        'hurdle n.b.' = model_hurdle), 
#                   new_data = test_df,
#                   response = n_later_phil) +
#     # scale_y_sqrt(breaks = c(1, 10, 100, 1000, 2000, 3000)) +
#     geom_vline(xintercept = c(5-1, 13-1), 
#                color = 'grey50', linetype = 'dashed') +
#     facet_wrap(~ demographic, scales = 'free') +
#     scale_color_brewer(palette = 'Set1') +
#     theme_minimal()
# 
# 
# ## Extract estimates ----
# ## TODO:  one giant grid of plots is overwhelming; need to break these into blocks by groups of models and groups of processes
# 
# model_list = list('linear probability' = model_lm,
#                   'logit' = model_logit,
#                   'Poisson' = model_pois,
#                   'negative binomial' = model_nb,
#                   'hurdle n.b.' = model_hurdle
# )
# n_model_plots = ifelse('hurdle n.b.' %in% names(model_list), 
#                        length(model_list) + 1, 
#                        length(model_list))
# process_list = list(#'low_income', 
#     # 'first_gen',
#     # 'admission_type',
#     # 'undeclared.student',
#     'grade_diff',
#     'dmg'
#     # 'women_share',
#     # 'poc_share',
#     # 'current_phil_share',
#     # 'n_students',
#     # 'gender.instructor'
#     # 'race.instructor'
# )
# 
# estimates = cross_df(list(model = model_list, process = process_list)) %>% 
#     mutate(model_name = rep.int(names(model_list), 
#                                 length(process_list))) %>% 
#     mutate(estimates = map2(model, process, extract_estimates)) %>% 
#     unnest(estimates) %>% 
#     rename(model = model_name) %>% 
#     mutate(model = fct_inorder(model), 
#            process = fct_inorder(process1)) %>% 
#     ## Back-transform estimates
#     mutate_if(is.numeric,
#               ~ ifelse(model == 'linear probability', 
#                        ., 
#                        exp(.) - 1))
# 
# ggplot(estimates, 
#        aes(x = term, y = estimate.comb, 
#            ymin = ci.low, ymax = ci.high, 
#            color = race, shape = gender)) +
#     geom_pointrange(position = position_dodge(width = .25)) +
#     geom_hline(yintercept = 0, linetype = 'dashed') +
#     coord_flip() +
#     facet_wrap(~ model + #hurdle_component + 
#                    process, scales = 'free_x', 
#                nrow = n_model_plots) +
#     xlab('') +
#     # ylab('estimated effect') +
#     scale_y_continuous(labels = scales::percent_format()) +
#     scale_color_brewer(palette = 'Set1') +
#     theme_bw() +
#     theme(legend.position = 'bottom') +
#     ggtitle('Preliminary effects estimates', 
#             subtitle = Sys.time())
# 
# ggsave(str_c(plots_folder, '04_estimates.png'), 
#        width = 5*(length(process_list) + 0),
#        height = 3*(n_model_plots + 1), 
#        scale = 1/2)
