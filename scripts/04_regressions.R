# This script generates the regression models, as well as rootograms to assess goodness-of-fit
library(tidyverse)
library(lme4)
# library(MASS)
library(pscl)
library(broom)

library(googlesheets)

library(tictoc)

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


## Training and testing sets ----
set.seed(2019-02-14)
test_rows = dataf %>%
    mutate(row_idx = row_number()) %>%
    group_by(ever_phil, demographic) %>%
    sample_frac(size = test_share) %>%
    pull(row_idx)

train_df = dataf[-test_rows,]
test_df = dataf[test_rows,]


## ever_phil models ----
ever_form = formula(ever_phil ~ 1 + 
                        demographic +
                        ## ***Processes we are examining***
                        ## Class effects
                        low_income*demographic +
                        first_gen*demographic +
                        ## Student background effects
                        admission_type*demographic +
                        undeclared.student*demographic +
                        ## Grade gap effect; Thompson 2017, §2.2.1
                        grade_diff*demographic +
                        ## Difference in mean GPA
                        dmg*demographic +
                        ## Peer effects
                        women_share*demographic +
                        poc_share*demographic +
                        current_phil_share*demographic +
                        n_students*demographic +
                        ## Instructor demographic effects; Thompson 2017, §2.3.1
                        gender.instructor*demographic +
                        race.instructor*demographic +
                        ## ***Controls***
                        other_major.student +
                        year + quarter + 
                        other_major_share + 
                        mean_grade +
                        instructor.log_total)

model_lm = lm(ever_form, data = train_df)

model_logit = glm(ever_form, 
                  family = binomial,
                  data = train_df)

## Construct a standing rootogoram for a binary response model
## [@KleiberVisualizingCountData2016] 
rootogram_binom = function(model, 
                           new_data = NULL, ## if not provided, uses fitting data
                           response, ## observed values of y
                           threshold, ## for discretizing fitted values
                           delogit = FALSE, ## do fitted values need to be de-logit-ed? 
                           sqrt_scale = TRUE ## on the y axis
) {
    response_var = enquo(response)
    
    augmented_df = augment(model, newdata = new_data)
    if (delogit) {
        augmented_df = mutate(augmented_df, 
                              .fitted = boot::inv.logit(.fitted))
    }
    augmented_df = mutate(augmented_df, 
                          .predicted = (.fitted > threshold) + 1)
    
    plot = ggplot(augmented_df) +
        stat_count(aes(!!response_var, color = 'observed', group = 1L), 
                   geom = 'col', fill = NA) +
        stat_count(aes(.predicted, color = 'fitted', group = 1L), 
                   geom = 'line')
    if (sqrt_scale) {
        plot = plot + scale_y_sqrt()
    }
    return(plot)
}

threshold = .3
rootogram_binom(model_lm, NULL, ever_phil, threshold) +
    theme_minimal() +
    ggtitle(str_c('linear probability: ', threshold, '; training data'),
            subtitle = Sys.time())
rootogram_binom(model_lm, test_df, ever_phil, threshold) +
    theme_minimal() +
    ggtitle(str_c('linear probability: ', threshold, '; testing data'),
            subtitle = Sys.time())
rootogram_binom(model_logit, NULL, ever_phil, threshold, 
                delogit = TRUE) +
    theme_minimal() +
    ggtitle(str_c('logistic regression: ', threshold, '; training data'), 
            subtitle = Sys.time())
rootogram_binom(model_logit, test_df, ever_phil, threshold, 
                delogit = TRUE) +
    theme_minimal() +
    ggtitle(str_c('logistic regression: ', threshold, '; testing data'), 
            subtitle = Sys.time())


## n_later_phil models ----
later_form = formula(n_later_phil ~ 1 + 
                         demographic +
                         ## ***Processes we are examining***
                         ## Student background
                         low_income*demographic +
                         first_gen*demographic +
                         admission_type*demographic +
                         undeclared.student*demographic +
                         ## Grade gap effect
                         grade_diff*demographic +
                         ## Difference in mean grade gap
                         dmg*demographic +
                         ## Peer effects
                         women_share*demographic +
                         poc_share*demographic +
                         current_phil_share*demographic +
                         n_students*demographic +
                         ## Student-instructor demographics
                         gender.instructor*demographic +
                         race.instructor*demographic +
                         ## ***Controls***
                         ever_phil + ## NB
                         other_major.student +
                         year + quarter +
                         other_major_share +
                         mean_grade +
                         instructor.log_total
)

model_pois = glm(later_form, 
                 family = poisson, 
                 data = train_df)
model_nb = MASS::glm.nb(later_form, 
                        trace = 1, 
                        data = train_df)

## A zero-inflated model represents the data as having "true" and "false" zeroes; in our data this only makes sense if records for a single student were somehow split over two+ different student IDs
model_zinb = zeroinfl(later_form,
                      dist = 'negbin',
                      trace = 1,
                      data = train_df)

## A hurdle model is theoretically appropriate.  But it's not much better than the ordinary negative binomial
model_hurdle = hurdle(later_form,
                      dist = 'negbin',
                      trace = 1,
                      data = train_df)


## Rootogram for Poisson and NB models
rootogram_count = function(model_list,
                           new_data = NULL, 
                           response, 
                           sqrt_scale = TRUE) {
    ## Wrapper to simplify passing a single model
    if (!inherits(model_list, 'list')) {
        model_list = list(model_list)
    }
    
    ## glm and MASS models have broom tidiers; pscl models don't
    augment_model = function(model, new_data) {
        if (inherits(model, 'lm')) {
            augmented_df = augment(model, newdata = new_data, 
                                   type.predict = 'response')  
        } else if (inherits(model, 'hurdle') | inherits(model, 'zeroinfl')) {
            augmented_df = mutate(new_data, 
                                  .fitted = predict(model, 
                                                    newdata = new_data, 
                                                    type = 'response'))
        } else {
            stop('Model class not recognized')
        }
        return(augmented_df)
    }
    
    response_var = enquo(response)
    if (is.null(new_data)) {
        new_data = model_list[[1]]$model
    }
    
    augmented = map(model_list, augment_model, new_data)
    
    ## Plot of observed values
    obs_plot = ggplot(new_data) +
        stat_count(aes(!!response_var, color = 'observed', group = 1L), 
                   geom = 'col', fill = NA)
    
    ## Construct line and point layers for each model
    construct_prediction_layer = function(model, name) {
        list(stat_bin(data = model, 
                      aes(.fitted, color = as.character(name), group = 1L), 
                      binwidth = 1, 
                      geom = 'line'),
             stat_bin(data = model, 
                      aes(.fitted, color = as.character(name), group = 1L), 
                      binwidth = 1, 
                      geom = 'point'))
    }
    prediction_layers = augmented %>% 
        imap(construct_prediction_layer) %>% 
        flatten()
    ## `+.gg()` understands a list of layers
    plot = obs_plot + prediction_layers
    
    ## Conventionally a sqrt transform allows comparison in the tails
    if (sqrt_scale) {
        plot = plot + scale_y_sqrt()
    }
    return(plot)
}


# rootogram_count(model_pois, 
#                 NULL, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Poisson, training data',
#             subtitle = Sys.time())
# rootogram_count(model_pois, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Poisson, testing data',
#             subtitle = Sys.time())
# 
# 
# rootogram_count(model_nb, 
#                 NULL,
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Negative binomial, training data', 
#             subtitle = Sys.time())
# rootogram_count(model_nb, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Negative binomial, testing data', 
#             subtitle = Sys.time())
# 
# 
# rootogram_count(model_hurdle, 
#                 NULL,
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Hurdle negative binomial, training data', 
#             subtitle = Sys.time())
# rootogram_count(model_hurdle, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Hurdle negative binomial, testing data', 
#             subtitle = Sys.time())



## All models overpredict at 1, underpredict in the 2-5 range; 
## maybe this is due to missing minors? 
## Overdisperson is a clear issue for Poisson
## The hurdle does better in the teens, but the plain NB handles the tail better
rootogram_count(list('Poisson' = model_pois, 
                       'n.b.' = model_nb, 
                       'zinb' = model_zinb,
                       'hurdle n.b.' = model_hurdle), 
                  new_data = test_df,
                  response = n_later_phil) +
    scale_y_sqrt(breaks = c(1, 10, 100, 1000, 2000, 3000)) +
    scale_color_brewer(palette = 'Set1') +
    theme_minimal()


