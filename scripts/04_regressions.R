library(tidyverse)
library(lme4)
library(broom)

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
insecure_data_folder = '../data_insecure/'
plots_folder = '../plots/'

## Load data ----
dataf = read_rds(str_c(data_folder, '03_analysis_df.Rds'))





## Rough cut regressions ----
glmer(ever_phil ~ 1 + (women_share + gender + race + first_gen + low_income + admission_type + (1|year), 
      data = dataf, 
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

stop()
## Interaction plots ----
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
