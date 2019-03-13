library(tidyverse)

library(dagitty)
library(ggdag)
library(igraph)
library(tidygraph)
library(ggraph)

# devtools::install_github("schochastics/smglr")
library(smglr)


## Variables of interest
vars = c('class', 'admission_type', 'undeclared.student', 
         'grade_diff', 'dmg', 
         'peer_demographics', 'current_phil_share', 
         'n_students', 'intro_course',
         'instructor_demographics')
names(vars) = vars

## General controls
general_controls = c('year', 'quarter', ## seasonal and long-term trends
                     'instructor.log_total')  ## proxy for instructor seniority

## Define DAG ----
phil_dag = dagify(outcome ~ practical_major + undeclared.student +
                      other_major.student +
                      philosophy_person + mentoring, 
                  practical_major ~ class, 
                  undeclared.student ~ admission_type + intro_course, 
                  philosophy_person ~ prior_perceptions + 
                      classroom_culture + schema_clash + mentoring,
                  mentoring ~ instructor_demographics + bias, 
                  other_major.student ~ other_major_share,
                  schema_clash ~ classroom_culture + 
                      prior_perceptions + 
                      grade_diff + current_phil_share + 
                      peer_demographics +
                      instructor_demographics,
                  prior_perceptions ~ class, 
                  prev_phil_course ~ admission_type + class + 
                      undeclared.student, 
                  bias ~ dmg + instructor_demographics, 
                  classroom_culture ~ bias + n_students + mean_grade +
                      current_phil_share + other_major_share + 
                      peer_demographics + instructor_demographics,
                  n_students ~ intro_course, 
                  grade_diff ~ dmg + prev_phil_course, 
                  current_phil_share ~ intro_course, 
                  peer_demographics ~ current_phil_share + 
                      instructor_demographics,
                  mean_grade ~ bias + peer_demographics + 
                      instructor_demographics + current_phil_share,
                  latent = c('prev_phil_course', 'classroom_culture', 
                             'practical_major', 'prior_perceptions', 
                             'philosophy_person', 'mentoring', 'schema_clash', 'bias'),
                  
                  outcome = 'outcome')


## Visualize ----
# ggdag(phil_dag, node = TRUE, stylized = FALSE,
#       text_col = 'red') +
#     theme_dag_blank()

stress = phil_dag %>% 
    as_tbl_graph() %>% 
    layout_with_focus(., length(V(.))) %>% 
    `colnames<-`(c('x', 'y')) %>% 
    as_tibble()

phil_dag %>% 
    as_tbl_graph() %>% 
    mutate(var_type = case_when(name == 'outcome' ~ 'outcome', 
                                name %in% vars ~ 'variable of interest', 
                                name %in% latents(phil_dag) ~ 'unmeasured', 
                                name %in% c('mean_grade', 
                                            'other_major_share', 
                                            'other_major.student') ~ 'measured control',
                                TRUE ~ 'error')) %>% 
    ggraph(layout = 'manual', node.positions = stress) +
    geom_node_label(aes(label = name, fill = var_type), 
                    color = 'white') +
    geom_edge_link(arrow = arrow(angle = 10, length = unit(3, 'mm')),
                   aes(start_cap = circle(5, 'mm'),
                       end_cap = circle(8, "mm"))) +
    scale_fill_brewer(palette = 'Set1', name = 'variable type', 
                      direction = -1) +
    theme_graph()


## Construct control sets
controls = map(vars, ~adjustmentSets(phil_dag, exposure = .))
controls
all(map_int(controls, length) > 0)


## Construct formulas ----
## Strictly speaking, this constructs the RHS
construct_form = function(covars, other_vars = general_controls) {
    expr = str_c('1', 
                 str_c(covars, collapse = ' + '), 
                 str_c(other_vars, collapse = ' + '), 
                 sep = ' + ', collapse = ' + ')
    return(expr)
}

## Shorthands:  
## class -> low_income + first_gen
## peer_demographics -> women_share + poc_share
## instructor_demographics -> gender.instructor + race.instructor
expand_var = function(string, var, expansion) {
    ## NB 1: Pass expansion as a vector
    ## NB 2: This probably isn't robust to spacing.  Use spaces on around +, no spaces around *
    ## Non-interaction terms
    string = str_replace_all(string, 
                             str_c(var, '(?!\\*)'), 
                             str_c(expansion, collapse = ' + '))
    ## Interaction terms:  
    ## If foobar is expanded to foo and bar, 
    ## replace foobar*thing with foo*thing + bar*thing
    string = str_replace_all(string,
                                   str_c(var, '\\*', '([[:alnum:]]+)'),
                                   str_c(expansion, '\\*\\1',
                                         collapse = ' + '))
    return(string)
}
# expand_var('1 + class*foo + class*demographic', 
#            'class', 
#            c('low_income', 'first_gen'))

## Apply construct_form to each set of controls, then combine into df
reg_form_df = map_depth(controls, 2, construct_form) %>% 
    map_depth(3, ~ tibble(reg_form = .)) %>% 
    map_depth(2, bind_rows) %>% 
    map(bind_rows) %>% 
    bind_rows(.id = 'focal_var') %>% 
    ## Add interaction term for variable of interest
    mutate(reg_form = str_c(reg_form, str_c(focal_var, '*demographic'), 
                            sep = ' + ')) %>% 
    ## Expand shorthand variables
    mutate(reg_form = expand_var(reg_form, 'class', 
                                 c('low_income', 'first_gen')), 
           reg_form = expand_var(reg_form, 'peer_demographics', 
                                 c('women_share', 'poc_share')), 
           reg_form = expand_var(reg_form, 
                                 'instructor_demographics', 
                                 c('gender.instructor', 
                                   'race.instructor'))) %>% 
    ## Outcome variables
    crossing(tibble(outcome = c('ever_phil', 
                                'n_later_phil'))) %>% 
    mutate(reg_form = str_c(outcome, ' ~ ', reg_form), 
           ## ever_phil is a general control for the n_later_phil models
           reg_form = ifelse(outcome == 'n_later_phil', 
                             str_c(reg_form, ' + ever_phil'), 
                             reg_form)) %>% 
    select(outcome, focal_var, everything()) %>% 
    arrange(outcome, focal_var)


# thing = lm(reg_form_df$reg_form[[10]], data = train_df)
