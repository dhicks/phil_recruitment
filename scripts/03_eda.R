## TODO: try this for effects plots:  <https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html>

library(tidyverse)
library(lubridate)

library(skimr)
library(tictoc)
library(assertthat)

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
insecure_data_folder = '../data_insecure/'
plots_folder = '../plots/'

theme_set(theme_minimal())

## Load data ----
profile_df = read_rds(str_c(data_folder, '01_profile.Rds')) %>%
    mutate(n_first_phil = map_int(first_phil_course, nrow), 
           n_later_phil = n_phil - n_first_phil) %>%
    mutate_at(vars(ethnicity, ethnicity_code), fct_explicit_na) %>%
    mutate_at(vars(admission_type, gender, 
                   major_at_first_phil, race), 
              as_factor) %>%
    mutate(multiple_phil = n_later_phil >= 1, 
           humdev_at_first_phil = str_detect(major_at_first_phil, 
                                             'Human Development'))
crs_df = read_rds(str_c(data_folder, '01_crs.Rds')) %>%
    mutate_at(vars(major), fct_explicit_na) %>%
    mutate_at(vars(instructor), as_factor) %>%
    mutate(term_posix = parse_date_time2(term, 'Ym'))
major_term = read_rds(str_c(data_folder, '01_major_long.Rds'))

## University-wide trends
trends = read_rds(str_c(insecure_data_folder, '02_trends.Rds'))
## Reconcile trends data w/ profile_df
trends_gender = trends$gender %>%
    mutate(gender = fct_recode(gender, 
                               'F' = 'Female', 
                               'M' = 'Male',
                               NULL = 'Unknown'), 
           term_posix = parse_date_time2(term, 'Ym'))
trends_race = trends$race %>%
    mutate(race = fct_recode(race, 
                             'Asian' = 'Asian-PI', 
                             'Black' = 'African American', 
                             'Hispanic' = 'Chicano-Latino', 
                             'Other' = 'Other/Unknown', 
                             'Indigenous' = 'Native American',
                             'White' = 'White'), 
           term_posix = parse_date_time2(term, 'Ym'))

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
## Because we don't know when they graduate, this isn't useful
# major_term %>%
#     filter(str_detect(major, 'Philosophy')) %>%
#     # count(term) %>%
#     ggplot(aes(term, group = 1L)) +
#     stat_count(geom = 'line')

## Intersection of demographics
count(profile_df, gender, race, first_gen, low_income, admission_type) #%>% View('race')
count(profile_df, gender, poc, first_gen, low_income, admission_type) #%>% View('poc')

## More manageable if gender and race are the only demographic factors
count(profile_clean, gender, race) %>% 
    arrange(desc(n))

## Principal components doesn't help
demo_pc = profile_df %>%
    select(gender, poc, first_gen, low_income, admission_type) %>%
    mutate_all(as.integer) %>%
    prcomp()
summary(demo_pc)
plot(demo_pc)

varimax(demo_pc$rotation)
varimax(demo_pc$rotation[,1:4])
varimax(demo_pc$rotation[,1:3])

## Which instructors have missing grades? 
crs_df %>%
    filter(is.na(grade)) %>%
    count(instructor) %>%
    arrange(d)


## Some plots ----
## Demographics
## All students
profile_df %>%
    select(admission_type, gender, race, first_gen, low_income) %>%
    gather(key = variable, value = value) %>%
    count(variable, value) %>%
    group_by(variable) %>%
    mutate(share = n / sum(n)) %>%
    ggplot(aes(value, share)) +
    geom_col(aes(fill = variable), show.legend = FALSE) +
    geom_text(aes(label = n), nudge_y = .05) +
    facet_wrap(~ variable, scales = 'free')
# ggsave(str_c(plots_folder, '03_demographics.png'), height = 3, width = 6, scale = 2)

## Majors
profile_df %>%
    filter(ever_phil) %>%
    select(admission_type, gender, race, first_gen, low_income) %>%
    gather(key = variable, value = value) %>%
    count(variable, value) %>%
    group_by(variable) %>%
    mutate(share = n / sum(n)) %>%
    ggplot(aes(value, share)) +
    geom_col(aes(fill = variable), show.legend = FALSE) +
    geom_text(aes(label = n), nudge_y = .05) +
    facet_wrap(~ variable, scales = 'free')

## Combine these two
demo_long = profile_df %>%
    select(ever_phil, admission_type, gender, race, first_gen, low_income) %>%
    gather(key = variable, value = value, -ever_phil) %>%
    count(ever_phil, variable, value)
demo_all = demo_long %>%
    group_by(variable, value) %>%
    summarize(n = sum(n)) %>%
    mutate(share = n / sum(n), 
           students = 'all') %>%
    ungroup()
demo_majors = demo_long %>%
    filter(ever_phil) %>%
    group_by(variable, value) %>%
    summarize(n = sum(n)) %>%
    mutate(share = n / sum(n), 
           students = 'majors') %>%
    ungroup()

bind_rows(demo_all, demo_majors) %>%
    ggplot(aes(value, share, color = students, group = students)) +
    # geom_col(position = 'dodge') +
    geom_segment(aes(xend = value, yend = 0), size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = n), nudge_y = .05, nudge_x = .25, show.legend = FALSE) +
    facet_wrap(~ variable, scales = 'free')
ggsave(str_c(plots_folder, '03_demographics.png'), height = 3, width = 6, scale = 2)

    


## Fraction of 1. philosophy students that ever major, by term
profile_df %>%
    unnest() %>%
    left_join(crs_df, by = c('id', 'course_id')) %>%
    select(id, ever_phil, term, term_posix, quarter) %>%
    filter(!duplicated(.)) %>%
    group_by(term, term_posix, quarter) %>%
    summarize(ever_phil_share = mean(ever_phil)) %>%
    ggplot(aes(term_posix, ever_phil_share)) +
    geom_line() +
    geom_point(aes(color = as.factor(quarter)))
ggsave(str_c(plots_folder, '03_descriptive_share.png'), height = 3, width = 6)


## Comparison of philosophy to campus-wide demographic trends
## At course level
crs_df %>%
    select(term, course_id, women_share, poc_share) %>%
    filter(!duplicated(.)) %>%
    mutate(term_posix = parse_date_time2(term, 'Ym')) %>%
    ggplot(aes(term_posix, women_share)) +
    # geom_jitter(alpha = .01) +
    stat_summary(geom = 'line', aes(color = 'philosophy')) +
    geom_line(data = filter(trends_gender, 
                            gender == 'F', 
                            year >= 2005, year <= 2015),
              aes(y = frac, color = 'campus-wide')) +
    theme_minimal()
ggsave(str_c(plots_folder, '03_women_trends.png'), height = 3, width = 6)

crs_df %>%
    select(term, course_id, women_share, poc_share) %>%
    filter(!duplicated(.)) %>%
    mutate(term_posix = parse_date_time2(term, 'Ym')) %>%
    ggplot(aes(term_posix, poc_share)) +
    # geom_point(aes(color = 'philosophy'), alpha = .05) +
    stat_summary(geom = 'line', aes(color = 'philosophy')) +
    stat_summary(data = filter(trends_race, 
                               ! race %in% c('White', 'Other'), 
                               year >= 2005, year <= 2015), 
                 aes(y = frac, color = 'campus-wide'), 
                 fun.y = sum, 
                 geom = 'line') +
    theme_minimal()
ggsave(str_c(plots_folder, '03_poc_trends.png'), height = 3, width = 6)


## Analysis df ----
## Only first philosophy courses, students who aren't already majors, students w/ binary gender ID
## This is probably the dataset we'll use to build the models
analysis_df = profile_df %>%
    filter(!phil_at_first_phil, gender != 'N') %>% 
    mutate(gender = fct_relevel(gender, 'M'), 
           race = fct_relevel(race, 'White')) %>%
    unnest() %>%
    left_join(crs_df, by = c('id', 'course_id'), 
              suffix = c('', '.class')) %>%
    filter(!is.na(grade))

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
crs_covars = analysis_df %>%
    select(course_id, year, term, 
           n_students, mean_grade, mean_cum_gpa, 
           women_share, poc_share, 
           first_gen_share, low_income_share, 
           ever_phil, multiple_phil, n_later_phil
           ) %>%
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
    write_csv(str_c(data_folder, '03_instructors.csv'))

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
    filter(abs(cor) > .8) %>%
    arrange(desc(abs(cor)))

ggplot(cors_long, aes(Var1, Var2, fill = cor)) +
    geom_tile() +
    scale_fill_gradient2(low = 'red', high = 'blue', limits = c(-1, 1))




## Rough cut regressions ----
library(lme4)
library(broom)
glmer(ever_phil ~ gender + race + first_gen + low_income + admission_type + (1|year), 
       data = analysis_df, 
       family = binomial) %>%
    summary()


model = analysis_df %>%
    mutate(grade_diff = term_cum_gpa - grade) %>%
    mutate_at(vars(matches('share')), funs(.*10)) %>%
    glm(ever_phil ~ gender*women_share + race*poc_share + first_gen*first_gen_share + low_income*low_income_share + 
            human_dev.student + bio_sci.student + soc.student + undeclared.student + 
            human_dev_share + bio_sci_share + soc_share + undeclared_share + current_phil_share +
            admission_type + 
            term_cum_gpa + grade_diff +
            n_students + mean_grade + mean_cum_gpa,# + (1|year), 
        data = ., 
        family = binomial)

summary(model)

## Interaction plots
model = glm(ever_phil ~ gender*women_share, data = analysis_df, family = binomial)

## This gives exactly what we want to plot, but requires a full newdata df
augment(model, newdata = expand.grid(gender = c('M', 'F'), women_share = .01*0:100), type.predict = 'link') %>%
    mutate(.conf.low = .fitted + .se.fit*qnorm(.025), 
           .conf.high = .fitted + .se.fit*qnorm(.975)) %>%
    # mutate_at(vars(matches('\\.')), boot::inv.logit) %>%
    # mutate_at(vars(matches('\\.')), exp) %>%
    ggplot(aes(women_share, .fitted, color = gender, fill = gender)) +
    geom_line() +
    geom_ribbon(aes(ymin = .conf.low, ymax = .conf.high), alpha = .5)

## This plots fitted values for each individual point, then a kind of second-order lm
## This is much more convenient; but ribbons don't reflect uncertainty in first-order models
## Inspired by <https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/>
# augment(model) %>%
#     ggplot(aes(women_share, .fitted, color = gender)) +
#     geom_point(aes(alpha = ever_phil)) +
#     geom_smooth(method = 'lm') +
#     scale_alpha_discrete(range = c(.05, 1)) +
#     theme_minimal()
#     
# augment(model) %>%
#     ggplot(aes(poc_share, .fitted, color = race)) +
#     geom_point(aes(alpha = ever_phil)) +
#     geom_smooth(method = 'lm') +
#     theme_minimal() +
#     facet_wrap(~ race)
# 
# augment(model) %>%
#     ggplot(aes(grade_diff, .fitted)) +
#     geom_point(alpha = .1) +
#     geom_smooth(method = 'lm') +
#     theme_minimal()

effects_plot = function(model, covar, group, alpha = ever_phil) {
    covar = enquo(covar)
    group = enquo(group)
    alpha = enquo(alpha)
    
    augment(model) %>%
        ggplot(aes(!!covar, .fitted, color = !!group)) +
        geom_point(aes(alpha = !!alpha)) +
        scale_alpha_discrete(range = c(.05, 1)) +
        geom_smooth(method = 'lm', se = FALSE)
}
effects_plot(model, 10*women_share, gender) + 
    xlab('% women share') + 
    theme_minimal()
effects_plot(model, 10*first_gen_share, first_gen)

effects_plot(model, 10*poc_share, race) + facet_wrap(~ race)
