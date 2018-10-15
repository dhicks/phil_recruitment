## This script parses student demographics tables retrieved from the following website:  
## <https://sisds.ucdavis.edu/trends/Registrar_Trend_Intake.cfm>

## NB While the files have xls extensions, they're actually HTML files

library(tidyverse)
library(xml2)
library(rvest)

## parse_table parses an individual table extracted from a file
## cat_var sets the name of the variable of interest; 
## qtr sets the quarter starting month (eg, '03' for Spring)
parse_table = function(table, cat_var, qtr) {
    cat_var = quo_name(enquo(cat_var))
    table %>%
        as_tibble() %>%
        ## Drop Total
        filter(Label != 'Total') %>%
        ## Lengthen
        gather(key = year, value = n, -Label) %>%
        ## Clean variables
        transmute(!!cat_var := Label,
                  year = as.integer(year), 
                  # n_raw = n,
                  n = parse_number(n), 
                  n = replace_na(n, 0)) %>%
        ## Fractions
        group_by(year) %>%
        mutate(frac = n / sum(n, na.rm = TRUE)) %>%
        ungroup() %>%
        ## Quarter and term
        mutate(qtr = qtr, 
               term = str_c(year, qtr))
}

# parse_table(tables[[1]], gender, '03') %>% names()
# parse_table(tables[[2]], race, '03')

## parse_file loads the given file and parses both gender and race
## This exploits the fact that, as retrieved, gender is the first table and race is the second
parse_file = function(file, qtr) {
    top = read_html(file)
    tables = xml_find_all(top, '//table//table[@id="main"]') %>%
        xml_find_first('.//table') %>%
        html_table()
    
    results = list(gender = parse_table(tables[[1]], 'gender', qtr), 
                   race = parse_table(tables[[2]], 'race', qtr))
    return(results)
}

# parse_file('student_trend_Spring.xls', '03')

files = c('00_student_trend_Fall.xls', 
          '00_student_trend_Winter.xls', 
          '00_student_trend_Spring.xls', 
          '00_student_trend_Summer_I.xls', 
          '00_student_trend_Summer_II.xls')
data_folder = '../data_insecure/'

parsed = tibble(file = files,
                qtr = c('10', '01', '03', '05', '07')) %>%
    mutate(path = str_c(data_folder, files)) %>%
    mutate(parsed = map2(path, qtr, parse_file)) %>%
    pull(parsed) %>%
    transpose() %>%
    map(bind_rows) %>%
    write_rds(str_c(data_folder, '02_trends.Rds'))

## Little EDA ----
## Strong seasonal trends in racial demographics
## White share is highest during the regular AY (Fall-Spring), 
## then drops in summer sessions
## This also seems to be the case for other racial groups *besides* Asian-PI
## Hypothesis:  Asian-PI students are more likely to be on a nontraditional or accelerated schedule, and so are more likely to take summer courses
# library(lubridate)
# parsed$race %>%
#     filter(race != 'White', race != 'Asian-PI') %>%
#     group_by(year, term) %>%
#     mutate(term_parsed = parse_date_time2(term, 'Ym')) %>%
#     ungroup() %>%
#     ggplot(aes(term_parsed, frac)) +
#     geom_line(aes(color = race)) 
#     # stat_summary(fun.y = sum, geom = 'line', color = 'black')
# 
# ## Women share is also highest during the AY
# parsed$gender %>%
#     filter(gender == 'Female') %>%
#     mutate(term_parsed = parse_date_time2(term, 'Ym')) %>%
#     ggplot(aes(term_parsed, frac, group = 1L)) +
#     geom_line()

