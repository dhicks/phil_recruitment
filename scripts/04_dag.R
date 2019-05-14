library(tidyverse)
library(assertthat)

library(dagitty)
library(ggdag)
library(igraph)
library(tidygraph)
library(ggraph)

# devtools::install_github("schochastics/smglr")
library(smglr)

# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("Rgraphviz", version = "3.8")

library(Rgraphviz)
dotfile = '04_dag.dot'

plots_folder = '../plots/'
data_folder = '../data_insecure/'

## Variable sets ----
## Variables of interest
vars = c('class', 'admission_type', 'undeclared.student', 
         'grade_diff', 'dmg', 
         'peer_demographics', 'current_phil_share', 
         'n_students', 'course_division',
         'instructor_demographics', 
         'instructor.log_total')  ## proxy for instructor seniority
names(vars) = vars

## General controls
general_controls = c('year', 'quarter' ## seasonal and long-term trends
)  

## Define DAG ----
phil_dag = dagify(outcome ~ practical_major + undeclared.student +
                      other_major.student +
                      philosophy_person + mentoring, 
                  
                  admission_type ~ class,
                  bias ~ dmg + instructor.log_total + instructor_demographics, 
                  classroom_culture ~ bias + n_students + mean_grade +
                      current_phil_share + other_major_share + 
                      peer_demographics + instructor_demographics +
                      course_division + instructor.log_total,
                  course_division ~ admission_type,
                  current_phil_share ~ course_division,
                  dmg ~ instructor_demographics + instructor.log_total,
                  field_spec_ability ~ classroom_culture + grade_diff + prior_perceptions + mentoring,
                  gender_race_schemas ~ classroom_culture + 
                      prior_perceptions + 
                      current_phil_share + 
                      peer_demographics +
                      instructor_demographics,
                  grade_diff ~ dmg + prev_phil_course, 
                  instructor.log_total ~ course_division,
                  instructor_demographics ~ instructor.log_total,
                  logic_systematic_abstract ~ classroom_culture + 
                      prior_perceptions + 
                      current_phil_share + peer_demographics + 
                      instructor_demographics,  ## Cech, "Self-expressive edge of occupational sex segregation"
                  mean_grade ~ bias + peer_demographics + 
                      instructor_demographics + current_phil_share +
                      instructor.log_total,
                  mentoring ~ instructor_demographics + bias + instructor.log_total, 
                  n_students ~ course_division + instructor.log_total,
                  other_major.student ~ other_major_share,
                  other_major_share ~ course_division + instructor.log_total,
                  practical_major ~ class, 
                  undeclared.student ~ admission_type + course_division + other_major.student, 
                  peer_demographics ~ current_phil_share + 
                      instructor_demographics,
                  philosophy_person ~ prior_perceptions + 
                      classroom_culture + gender_race_schemas + mentoring +
                      logic_systematic_abstract +
                      grade_diff + field_spec_ability,
                  prev_phil_course ~ admission_type + class + 
                      undeclared.student, 
                  prior_perceptions ~ class, 
                  
                  latent = c('prev_phil_course', 'classroom_culture', 
                             'field_spec_ability', 'logic_systematic_abstract',
                             'practical_major', 'prior_perceptions', 
                             'philosophy_person', 'mentoring', 
                             'gender_race_schemas', 'bias'),
                  outcome = 'outcome')

var_labels = tribble(
    ~ name, ~ label,
    'current_phil_share', 'current majors', 
    'dmg', 'GDMGG', 
    'field_spec_ability', 'field spec. ability',
    'grade_diff', 'grade gap', 
    'instructor.log_total', 'inst. tot. students',
    'logic_systematic_abstract', 'L-S-A identity', 
    'n_students', 'class size', 
    'soc.-bio.-hum. dev.', 'other_major.student',
    'soc.-bio.-hum. dev. share', 'other_major_share', 
    'undeclared.student', 'undeclared', 
    'prev_phil_course', 'prev. phil. course'
)


## Visualize ----
## ggdag's layouts are ... yeah
# ggdag(phil_dag, node = TRUE, stylized = FALSE,
#       text_col = 'red') +
#     theme_dag_blank()

## smglr does a somewhat better job, but can't organize things hierarchically
# layout = phil_dag %>% 
#     as_tbl_graph() %>% 
#     layout_with_focus(., length(V(.))) %>% 
#     `colnames<-`(c('x', 'y')) %>% 
#     as_tibble()

## Rgraphviz layout
## Coercing from igraph:  <https://stackoverflow.com/questions/9663504/is-there-a-package-to-convert-network-or-igraph-networks-to-rgraphviz-compat>
## write.graph(mygraph, file="filename", format="dot")

phil_dag %>% 
    as_tbl_graph() %>% 
    write.graph(file = str_c(data_folder, dotfile), 
                format = 'dot')

layout = agread(str_c(data_folder, dotfile)) %>% 
{.@AgNode} %>% 
    map(~ .@center) %>% 
    map_dfr(~ tibble(x = .@x, 
                     y = .@y)) %>% 
    layout_rotate(90) %>% 
    set_names(c('x', 'y'))


phil_dag %>% 
    as_tbl_graph() %>% 
    left_join(var_labels) %>% 
    mutate(label = case_when(!is.na(label) ~ label, 
                             TRUE ~ str_replace_all(name, '_', ' ')), 
           var_type = case_when(name == 'outcome' ~ 'outcome', 
                                name %in% vars ~ 'variable of interest', 
                                name %in% latents(phil_dag) ~ 'unmeasured', 
                                name %in% c('mean_grade', 
                                            'other_major_share', 
                                            'other_major.student') ~ 'measured control',
                                TRUE ~ 'error')) %>% 
    ggraph(layout = 'manual', node.positions = layout) +
    geom_node_label(aes(label = label, fill = var_type), 
                    color = 'white') +
    geom_edge_link(arrow = arrow(angle = 10, length = unit(3, 'mm')),
                   aes(start_cap = circle(5, 'mm'),
                       end_cap = circle(8, "mm")), 
                   alpha = .5) +
    scale_fill_brewer(palette = 'Set1', name = '', 
                      direction = -1) +
    theme_graph() +
    theme(legend.position = 'bottom')

ggsave(str_c(plots_folder, '04_dag.png'), 
       height = 6, width = 11, scale = 1.5)


## Construct control sets ----
controls = map(vars, ~adjustmentSets(phil_dag, exposure = ., 
                                     type = 'canonical'))
controls
assert_that(all(map_int(controls, length) > 0))



## Table of controls ----
controls %>% 
    ## Replace empty lists with NAs
    modify_depth(2, 
                 ~ if (is_empty(.)) {
                     return(NA_character_)
                 } else {
                     return(.)
                 }) %>% 
    ## `bind_rows()` throws warnings when it doesn't recognize a column class
    quietly(map_dfr)(~ tibble(controls = .), .id = 'process') %>% 
    .$result %>% 
    mutate(model_idx = row_number()) %>%
    ## Unpack and arrange
    unnest(controls) %>% 
    mutate(t = TRUE) %>% 
    spread(key = controls, value = t, fill = FALSE) %>% 
    select(process, everything(), -model_idx, -`<NA>`) %>% 
    arrange(process) %>% 
    ## Generic controls
    mutate(year = TRUE, 
           quarter = TRUE, 
           instructor.log_total = TRUE) %>% 
    ## Generate table
    mutate_if(is.logical, 
              ~ifelse(., 'X', '')) %>% 
    mutate(process = str_replace_all(process, '_', ' '), 
           process = case_when(process == 'current phil share' ~ 'current phil. share', 
                               process == 'dmg' ~ 'GDMGG', 
                               process == 'grade diff' ~ 'grade gap',
                               process == 'instructor demographics' ~ 'inst. demographics', 
                               process == 'n students' ~ 'course size', 
                               process == 'undeclared.student' ~ 'undeclared', 
                               TRUE ~ process)) %>% 
    knitr::kable(format = 'markdown',
                 align = 'lccccccccccccccccccccccccccccccc', 
                 col.names = c('', 
                               'admission type', 
                               'class', 
                               'course division', 
                               'current phil. share', 
                               'GDMGG', 
                               'grade gap',
                               'inst. demographics', 
                               'inst. total students',
                               'mean grade',
                               'course size',
                               'soc., bio., hum. dev. share',
                               'soc., bio., hum. dev.',
                               'peer demographics',
                               'undeclared', 
                               'year', 
                               'quarter'), 
                 caption = 'Control variables used for each variable of interest.  Grade gap has two sets of control variables.  All control variables shown here were used in both major and later philosophy course models.  Later philosophy course models also included as a control whether the student ever majored in philosophy.\ref{tab.controls}') %>% 
    write_lines(str_c(plots_folder, '04_controls.md'))



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
    ## Add variable of interest
    mutate(full = str_c(reg_form, focal_var, sep = ' + '),
           interaction = str_c(reg_form, 
                               str_c(focal_var, '*demographic'), 
                               sep = ' + ')) %>% 
    select(-reg_form) %>% 
    gather(key = formula, value = reg_form, 
           full, interaction) %>% 
    ## Expand shorthand variables
    mutate(reg_form = expand_var(reg_form, 'class', 
                                 c('low_income', 'first_gen')), 
           reg_form = expand_var(reg_form, 'peer_demographics', 
                                 c('women_share', 'poc_share')), 
           reg_form = expand_var(reg_form, 
                                 'instructor_demographics', 
                                 c('gender.instructor', 
                                   'race.instructor')), 
           reg_form = expand_var(reg_form, 
                                 'n_students', 
                                 'log10(n_students)')) %>% 
    mutate(focal_var = map(focal_var, 
                           ~ case_when(. == 'class' ~ list(c('low_income', 'first_gen')), 
                                       . == 'peer_demographics' ~ list(c('women_share', 'poc_share')), 
                                       . == 'instructor_demographics' ~ list(c('gender.instructor', 'race.instructor')),
                                       TRUE ~ list(.))), 
           focal_var = flatten(focal_var)) %>% 
    unnest(focal_var) %>% 
    select(focal_var, everything()) %>% 
    ## Outcome variables
    tidyr::crossing(tibble(outcome = c('ever_phil', 
                                       'n_later_phil'))) %>% 
    mutate(reg_form = str_c(outcome, ' ~ ', reg_form), 
           ## ever_phil is a general control for the n_later_phil models
           reg_form = ifelse(outcome == 'n_later_phil', 
                             str_c(reg_form, ' + ever_phil'), 
                             reg_form)) %>% 
    select(outcome, focal_var, everything()) %>% 
    arrange(outcome, focal_var)

# thing = lm(reg_form_df$reg_form[[10]], data = train_df)

write_rds(reg_form_df, str_c(data_folder, '04_reg_form.Rds'))
