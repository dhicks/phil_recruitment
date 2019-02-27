# This script generates the regression models, as well as rootograms to assess goodness-of-fit
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


## ever_phil rootograms ----
threshold = .3
rootogram_binom(model_lm, NULL, ever_phil, threshold) +
    facet_wrap(~ demographic, scales = 'free') +
    theme_minimal() +
    ggtitle(str_c('linear probability: ', threshold, '; training data'),
            subtitle = Sys.time())

rootogram_binom(model_lm, test_df, ever_phil, threshold) +
    facet_wrap(~ demographic, scales = 'free') +
    theme_minimal() +
    ggtitle(str_c('linear probability: ', threshold, '; testing data'),
            subtitle = Sys.time())

rootogram_binom(model_logit, NULL, ever_phil, threshold, 
                delogit = TRUE) +
    facet_wrap(~ demographic, scales = 'free') +
    theme_minimal() +
    ggtitle(str_c('logistic regression: ', threshold, '; training data'), 
            subtitle = Sys.time())

rootogram_binom(model_logit, test_df, ever_phil, threshold, 
                delogit = TRUE) +
    facet_wrap(~ demographic, scales = 'free') +
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


## n_later_phil rootograms ----
## All models overpredict at 1, underpredict in the 2-5 range; 
## maybe this is due to missing minors? 
## Overdisperson is a clear issue for Poisson
## The hurdle does better in the teens, but the plain NB handles the tail better
rootogram_count(list(#'Poisson' = model_pois, 
                       # 'n.b.' = model_nb, 
                       # 'zinb' = model_zinb,
                       'hurdle n.b.' = model_hurdle), 
                  new_data = test_df,
                  response = n_later_phil) +
    # scale_y_sqrt(breaks = c(1, 10, 100, 1000, 2000, 3000)) +
    geom_vline(xintercept = c(5-1, 13-1), 
               color = 'grey50', linetype = 'dashed') +
    facet_wrap(~ demographic, scales = 'free') +
    scale_color_brewer(palette = 'Set1') +
    theme_minimal()


