library(tidyverse)

library(skimr)
library(tictoc)
library(assertthat)

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'

## Load data ----
profile_df = read_rds(str_c(data_folder, '01_profile.Rds')) %>%
    mutate(n_first_phil = map_int(first_phil_course, nrow), 
           n_later_phil = n_phil - n_first_phil) %>%
    mutate_at(vars(ethnicity, ethnicity_code), fct_explicit_na) %>%
    mutate_at(vars(admission_type, gender, major_at_first_phil, race), as_factor) %>%
    mutate(multiple_phil = n_later_phil >= 1)
crs_df = read_rds(str_c(data_folder, '01_crs.Rds')) %>%
    mutate_at(vars(major), fct_explicit_na) %>%
    mutate_at(vars(instructor), as_factor)
major_term = read_rds(str_c(data_folder, '01_major_long.Rds'))


## skimr ----
skim(profile_df)

## 220 NAs for major at first philosophy
## This is because major data is NA at or prior to this, eg, 
## transfers who took first philosophy course in YR2SPRING
# profile_df %>%
#     filter(is.na(major_at_first_phil)) %>%
#     select(id, first_phil_course) %>%
#     unnest() %>%
#     left_join(major_term)

## Gender
## 2 students w/ gender == N
## That's so few it'll produce bad estimates, and they won't be missed from other demographics
count(profile_df, gender)
profile_df %>%
    filter(gender == 'N') %>%
    unnest()

## 275 students have the philosophy major at the term of their first philosophy course
## **assuming these are majors at beginning of term** they should probably be dropped
count(profile_df, phil_at_first_phil) %>%
    mutate(frac = n / sum(n))
profile_df %>%
    filter(phil_at_first_phil) %>%
    skim()

## All student-CRS combinations
skim(crs_df)

## Only first philosophy courses
profile_df %>%
    unnest() %>%
    select(id, course_id) %>%
    left_join(crs_df) %>%
    skim()

## How many majors in each term? 
major_term %>%
    filter(current_phil) %>%
    # count(term) %>%
    ggplot(aes(term, group = 1L)) + 
    stat_count(geom = 'line')



## Analysis df ----
## Only first philosophy courses, students who aren't already majors, students w/ binary gender ID
## This is probably the dataset we'll use to build the models
analysis_df = profile_df %>%
    filter(!phil_at_first_phil, gender != 'N') %>% 
    unnest() %>%
    left_join(crs_df, by = c('id', 'course_id'), 
              suffix = c('', '.class'))

count(analysis_df, n_first_phil)    
skim(analysis_df)

## Year seems suspiciously flat 
## It seems like we might be missing data from 2005 and 2006
count(analysis_df, year)
## Not due to the filtering on profiles
profile_df %>%
    unnest() %>%
    left_join(crs_df, by = c('id', 'course_id'), 
              suffix = c('', '.class')) %>%
    count(year)
## Or only looking at first philosophy courses
count(crs_df, year)
## Even 2007 looks low
ggplot(crs_df, aes(year)) + 
    geom_bar()

## Probably what happened is the registrar's office included only students who matriculated in Fall 2005 or later
## (I forget whether this was in line w/ the data request)
major_term %>%
    select(id, admit_term) %>%
    filter(!duplicated(.)) %>%
    separate(admit_term, into = c('year', 'qtr'), 4, convert = TRUE) %>%
    # count(year)
    ggplot(aes(year)) +
    geom_bar()

## Missing data are an issue insofar as there are time trends in course-level variables
## - All variables show positive trends
## - But the ends are in line with the trends seen across the middle of the study period
## - Except women_share and n_students; but the changes in trends mostly happen in the middle of the study period
## Qualitatively similar btwn analysis_df and crs_df
crs_covars = crs_df %>% #analysis_df %>%
    select(course_id, year, term, 
           n_students, mean_grade, mean_cum_gpa, 
           women_share, poc_share, 
           first_gen_share, low_income_share) %>%
    gather(key = covariate, value = value, 
           -course_id, -year, -term)
ggplot(crs_covars, aes(year, value)) +
    # geom_point() +
    geom_smooth(color = 'red', method = 'lm', alpha = .25, 
                data = filter(crs_covars, year >= 2007, 
                              year < 2015)) +
    stat_summary(geom = 'line') +
    facet_wrap(~ covariate, scales = 'free')

## Instructors
## 74 distinct instructors
count(analysis_df, instructor) %>%
    arrange(desc(n)) %>%
    write_csv(str_c(data_folder, '02_instructors.csv'))

analysis_df %>%
    select(instructor, matches('share')) %>%
    gather(key = var, value = value, -instructor) %>%
    ggplot(aes(instructor, value)) + 
    stat_summary() +
    # geom_violin(draw_quantiles = .5) +
    # geom_point() +
    coord_flip() +
    facet_wrap(~ var)


## Correlations ----
cors_long = analysis_df %>%
    select_if(funs(is.factor(.)|is.numeric(.))) %>%
    mutate_if(is.factor, as.integer) %>%
    cor(use = 'pairwise.complete') %>%
    as_tibble(rownames = 'Var1') %>%
    gather(key = 'Var2', value = 'cor', -Var1) 

cors_long %>%
    filter(Var1 > Var2) %>%
    filter(abs(cor) > .8)

ggplot(cors_long, aes(Var1, Var2, fill = cor)) +
    geom_tile() +
    scale_fill_gradient2(low = 'red', high = 'blue', limits = c(-1, 1))




## Rough cut regressions ----
profile_df %>%
    filter(!phil_at_first_phil) %>%
    mutate(gender = fct_relevel(gender, 'M'), 
           race = fct_relevel(race, 'White')) %>%
    glm(ever_phil ~ gender + race + first_gen + low_income + admission_type, 
       data = ., 
       family = binomial)

profile_df %>%
    filter(!phil_at_first_phil) %>%
    mutate(gender = fct_relevel(gender, 'M'), 
           race = fct_relevel(race, 'White')) %>%
    unnest() %>%
    left_join(crs_df, by = c('id', 'course_id'), 
              suffix = c('', '.class')) %>%
    mutate(grade_diff = term_cum_gpa - grade) %>%
    glm(ever_phil ~ gender + race + first_gen + low_income + admission_type + 
            grade + grade_diff +
            n_students + mean_grade + mean_cum_gpa + 
            women_share + poc_share + first_gen_share + low_income_share, 
        data = ., 
        family = binomial) %>%
    summary()
