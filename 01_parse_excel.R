#' This file parses the Excel file from the registrar's office, constructing 3 data files:  
#' - `crs.Rds`:  Records for each student-CRS combination, including
#'     - student's major at term of course
#'     - instructor name
#'     - course-level statistics, such as number of students and share of various minority categories
#' - `profile.Rds`:  Demographics for each student, including gender, race-ethnicity, whether they ever majored in philosophy, number of philosophy courses taken, major at the term where they first took philosophy, and CRNs for their first philosophy courses
#' - `major_long.Rds`: Student majors at each term, including summer session.  These data are incorporated into `crs` and `profile`, and may not need to be loaded separately.  

library(tidyverse)
library(readxl)
library(zoo)

# library(furrr)
# plan(multiprocess, workers = 3)
library(tictoc)
library(assertthat)

data_folder = '/Volumes/DSI_SECURE/phil_recruitment/'
data_file = 'Request 4681.xlsx'

## Load data ----
profile = read_excel(str_c(data_folder, data_file), 
                     'Profile') %>%
    select(-IsPHI_MAJOR, -PHI_CNT, -IsPHI_MINOR) %>%
    filter(!duplicated(.))

## Previous code to filter out repeated student IDs
## The IsPHI_MAJOR appears to have errors, so instead we just drop it and filter out duplicates
# repeats = count(profile_unfltd, ID) %>%
#     filter(n > 1) %>%
#     inner_join(profile_unfltd) %>%
#     filter(IsPHI_MAJOR == 'N') %>%
#     select(ID, IsPHI_MAJOR)
# profile = anti_join(profile_unfltd, repeats)
assert_that(length(unique(profile$ID)) == nrow(profile))

phi_crs = read_excel(str_c(data_folder, data_file), 'PHI_CRS')
assert_that(length(unique(phi_crs$E_PIDM)) == nrow(profile))

major_term = read_excel(str_c(data_folder, data_file), 'Major_Term')
assert_that(length(unique(major_term$E_PIDM)) == nrow(major_term))
assert_that(nrow(major_term) == nrow(profile))


## Major by term ----
major_long_file = '01_major_long.Rds'
if (!file.exists(str_c(data_folder, major_long_file))) {
    increment_quarter = function (qtr) {
        case_when(qtr == '01' ~ '03',
                  qtr == '03' ~ '05',
                  qtr == '05' ~ '07',
                  qtr == '07' ~ '10',
                  qtr == '10' ~ '01')
    }
    increment_term = function (term) {
        year = str_trunc(term, 4, ellipsis = '')
        qtr = str_trunc(term, 2, side = 'left', ellipsis = '')
        
        new_qtr = increment_quarter(qtr)
        if (is.na(new_qtr)) stop(str_c('Bad term ', term))
        if (qtr == '10') {
            new_year = as.character(as.integer(year) + 1)
        } else {
            new_year = year
        }
        return(str_c(new_year, new_qtr))
    }
    
    
    increment_recursively = function (term, n) {
        if (n == 0) {
            return(term)
        } else {
            new_term = increment_recursively(term, n - 1)
            return(increment_term(new_term))
        }
    }
    
    rel_terms = expand.grid(fct_inorder(str_c('YR', 1:10)), 
                            fct_inorder(c('FALL', 'WINTER', 'SPRING', 
                                          'SUMMER_I', 'SUMMER_II')), 
                            major_term$E_PIDM, 
                            stringsAsFactors = FALSE) %>% 
        arrange(Var3, Var1, Var2) %>%
        transmute(id = Var3, rel_year = Var1, quarter = Var2, 
                  rel_term = str_c(Var1, Var2))
    
    ## Currently this takes ~1 min per 1k rows
    tic()
    major_long = major_term %>%
        select(id = E_PIDM, admit_term = ADMIT_TERM, 
               starts_with('YR')) %>%
        # slice(1:5000) %>% 
        ## If admit_term is June (-06), replace it w/ July (-07)
        mutate(admit_term = str_replace(admit_term, '06$', '07')) %>%
        ## Long format
        gather(key = rel_term, value = major, 
               -id, -admit_term) %>%
        ## Full join to add rows for missing terms
        full_join(rel_terms) %>%
        arrange(id, rel_year, quarter) %>% 
        ## Identify first and last terms with non-NA values
        group_by(id) %>%
        mutate(row_idx = row_number(),
               first_term = min(row_idx[!is.na(major)]),
               last_term = max(row_idx[!is.na(major)])) %>% 
        filter(row_idx >= first_term, row_idx <= last_term) %>% 
        ## Interpolate admit_term, major
        ## NB this assumes major doesn't change until we see it change
        mutate(admit_term = na.locf(admit_term),
               major = na.locf(major), 
               ## Reset row index so that 1. row has index 0
               row_idx = row_idx - first_term) %>%
        select(-first_term, -last_term) %>%
        ## Increment term label
        ungroup() %>% 
        rowwise() %>% 
        mutate(term = map2_chr(admit_term, row_idx, increment_recursively)) %>% 
        # select(-admit_term, -rel_year, -quarter) %>%
        ## Majoring in philosophy?  
        group_by(id) %>%
        mutate(current_phil = str_detect(major, 'Philosophy')) %>%
        ungroup()
    toc()
    
    write_rds(major_long, str_c(data_folder, major_long_file))
} else {
    major_long = read_rds(str_c(data_folder, major_long_file))
}



## Student profiles ----
## Ever majored in philosophy
ever_majored = major_long %>%
    group_by(id) %>%
    summarize(ever_phil = any(current_phil, na.rm = TRUE))

## Combine and clean
profile_clean = profile %>%
    ## Ever majored in philosophy
    left_join(ever_majored, by = c('ID' = 'id')) %>%
    ## Variable names
    rename_all(tolower) %>%
    rename(first_gen = isfirstgeneration, 
           low_income = islowincome) %>%
    ## Replace Y/N w/ logicals
    mutate(first_gen = first_gen == 'Y', 
           low_income = low_income == 'Y')

## CRS ----
## Grade points
## <https://registrar.ucdavis.edu/records/transcripts/calculate-gpa>
grade_points = tribble(
    ~ COURSE_LETTER_GRADE, ~ grade_points, 
    'A+', 4.0,
    'A', 4.0,
    'A-', 3.7,
    'B+', 3.3,
    'B', 3,
    'B-', 2.7,
    'C+', 2.3,
    'C', 2.0,
    'C-', 1.7,
    'D+', 1.3,
    'D', 1.0,
    'D-', 0.7,
    'F', 0.0
)
## Values that don't have grade point equivalents:  
## I, NG, NS, P, S, WD2, WN, Y

## 1. round of CRS cleaning:  just selecting variables
crs_1 = phi_crs %>%
    left_join(grade_points) %>%
    ## Variables of interest
    transmute(id = E_PIDM,
              TERM,
              ## Term-CRN gives a unique identifier for each course
              course_id = str_c(TERM, '_', CRN), 
              id_title = {str_c(course_id, ' ', COURSE_TITLE) %>%
                      str_replace_all(' ', '_')},
              title = COURSE_TITLE,
              INSTRUCTOR, 
              grade = grade_points, 
              TERM_CUM_GPA) %>%
    rename_all(tolower) %>%
    ## Major at term
    left_join(major_long) %>% 
    select(id:term_cum_gpa, major, current_phil) %>%
    ## Parse term into year and quarter
    separate(term, into = c('year', 'quarter'), sep = 4, 
             remove = FALSE, convert = TRUE)

## 2. round:  course-level calculated variables
crs_2 = crs_1 %>%
    left_join(profile_clean) %>%
    group_by(course_id) %>%
    summarize(n_students = n(),
              mean_grade = mean(grade, na.rm = TRUE),
              mean_cum_gpa = mean(term_cum_gpa, na.rm = TRUE),
              women = sum(gender == 'F'), 
              poc = sum(race != 'White'), 
              first_gen = sum(first_gen), 
              low_income = sum(low_income)) %>%
    mutate_at(vars(women:low_income), 
              funs(share = ./n_students)) #%>%
# arrange(desc(n_students)) %>% 
## There are 56 courses where mean grade is empty
## These appear to be reading courses
# summarize_all(funs(empty = sum(is.na(.)))) %>% View

## 3. round:  student-level CRS summaries, including major at first philosophy course
crs_3 = crs_1 %>%
    arrange(id, year, quarter) %>%
    group_by(id) %>%
    mutate(n_phil = n()) %>%
    filter(term == first(term)) %>%
    ungroup() %>% 
    select(id, course_id, major_at_first_phil = major, 
           phil_at_first_phil = current_phil, n_phil) %>% 
    nest(course_id, .key = 'first_phil_course')
assert_that(nrow(crs_3) == nrow(profile))

## ~500 students (4%) take 2+ philosophy courses during the first term they take philosophy
crs_3 %>%
    unnest() %>%
    count(id) %>% 
    count(n) %>%
    mutate(frac = nn / sum(nn))

## ~266 students (1.6%) have already declared a major in philosophy
## 1300 (8%) have missing major data during the term they first take philosophy
crs_3 %>%
    count(current_phil) %>%
    mutate(frac = n / sum(n))

## Combine
crs_clean = full_join(crs_1, crs_2)
assert_that(nrow(crs_clean) == nrow(phi_crs))


## Output ----
profile_clean %>%
    full_join(crs_3) %>%
    write_rds(str_c(data_folder, '01_profile.Rds'))
write_rds(crs_clean, str_c(data_folder, '01_crs.Rds'))


## EDA/scratch ----
stop("Don't run EDA")
## Upshot:  
## - 3 calculated columns (PHI_CNT, IsPHI_MAJOR, and IsPHI_MINOR) in profile are ambiguous, have errors, or are mostly missing (resp.)
## - There are a few duplicate IDs in profile, w/ different values of IsPHI_MAJOR
## - ETHNICITY_CODE = ETHNICITY < RACE
## - Hispanic is treated as a race

## Count NAs
## 73% of entries for IsPHI_MINOR are missing
## gender is missing in a few hundred cases
profile %>%
    summarize_all(funs(empty = sum(is.na(.)))) %>%
    gather(variable, empty) %>%
    mutate(frac = empty / nrow(profile))

## Some IDs are repeated? 
# length(unique(profile_unfltd$ID)) == nrow(profile_unfltd)
## 19 IDs appear twice
# count(profile_unfltd, ID) %>%
#     filter(n > 1)
## Once IsPHI_MAJOR == N, once IsPHI_Major == Y
## So I guess these are the people who switched in or out of the major?  
## Removed from `profile` using `repeats` above

## Count of PHI_CNT variable
## ... what is this?  presumably not course counts! 
# count(profile, PHI_CNT)
## doesn't match the counts from phi_crs
# phi_crs %>%
#     count(E_PIDM) %>%
#     count(n)

## 403 majors over the time period
# count(profile, IsPHI_MAJOR)

## Gender
## More women than men (52% vs 48%)
count(profile, GENDER) %>%
    mutate(frac = n / sum(n))

## Ethnicity and race
## "Hispanic" is a race
count(profile, RACE)
## 16 ethnicity values, including NA
count(profile, ETHNICITY)
## Ethnicity nests w/in race
## NA nests w/in Other
count(profile, RACE, ETHNICITY)
count(profile, ETHNICITY_CODE, ETHNICITY)
count(profile, RACE, ETHNICITY_CODE, ETHNICITY)

## Other categoricals
count(profile, IsFIRSTGENERATION)
count(profile, IsLOWINCOME)
count(profile, ADMISSION_TYPE)


## Do major_term and profile agree on who majored in philosophy? 
mj_phil = major_term %>%
    gather(term, major, YR1FALL:YR10SPRING) %>%
    filter(!is.na(major)) %>%
    mutate(philosophy = str_detect(major, 'Philosophy')) %>%
    group_by(E_PIDM) %>%
    summarize(ever_phil = any(philosophy))

## No they do not, on 227 rows
# profile_unfltd %>%
#     select(ID, IsPHI_MAJOR) %>%
#     full_join(mj_phil, by = c('ID' = 'E_PIDM')) %>%
#     mutate(agree = (IsPHI_MAJOR == 'Y') == ever_phil) %>% 
#     count(agree)
# 
# disagree = profile_unfltd %>%
#     select(ID, IsPHI_MAJOR) %>%
#     full_join(mj_phil, by = c('ID' = 'E_PIDM')) %>%
#     mutate(agree = (IsPHI_MAJOR == 'Y') == ever_phil) %>%
#     filter(!agree) %>%
#     left_join(profile_unfltd)
## On 226 rows, profile thinks they never majored
# count(disagree, IsPHI_MAJOR)

## These students mostly took a lot more classes than you'd expect for a non-major
## So I think it's fair to assume that profile is incorrect
# inner_join(phi_crs, disagree, by = c('E_PIDM' = 'ID')) %>%
#     count(E_PIDM) %>%
#     arrange(n) %>% 
#     ggplot(aes(n)) + geom_bar()

## Course stuff
## 5 values for quarter: 1, 3, 5, 7, and 10
## Winter, Spring, Summer 1, Summer 2, and Fall? 
count(phi_crs, TERM) %>%
    separate(TERM, into = c('year', 'quarter'), sep = 4, 
             convert = TRUE) %>%
    arrange(year, quarter)

## 1234 distinct CRNs
count(phi_crs, CRN)
## 1288 distinct CRN-TERM combinations
count(phi_crs, TERM, CRN)
## Only 54 CRNs are repeated across terms
count(phi_crs, TERM, CRN) %>%
    count(CRN) %>%
    count(nn)

## Looks like CRNs were reset between 2009 and 2011
count(phi_crs, TERM, CRN, COURSE_TITLE) %>%
    add_count(CRN) %>%
    filter(nn > 1) %>%
    arrange(CRN, TERM)

## TERM-CRN appears to be unique
count(phi_crs, TERM, CRN) %>%
    count(TERM, CRN) %>%
    filter(nn > 1)

## A few very large TERM-CRN combinations
count(phi_crs, TERM, CRN) %>%
    ggplot(aes(n)) + geom_bar()
## Adding section number doesn't make things smaller
count(phi_crs, TERM, CRN, SECTION_NUMBER) %>%
    ggplot(aes(n)) + geom_bar()

count(phi_crs, TERM, CRN, COURSE_TITLE) %>%
    filter(n > 50) %>%
    arrange(desc(n))

phi_crs %>%
    filter(TERM == '201401', CRN == '84310') %>%
    count(INSTRUCTOR)

## Grade points
## <https://registrar.ucdavis.edu/records/transcripts/calculate-gpa>
grade_points = tribble(
    ~ COURSE_LETTER_GRADE, ~ grade_points, 
    'A+', 4.0,
    'A', 4.0,
    'A-', 3.7,
    'B+', 3.3,
    'B', 3,
    'B-', 2.7,
    'C+', 2.3,
    'C', 2.0,
    'C-', 1.7,
    'D+', 1.3,
    'D', 1.0,
    'D-', 0.7,
    'F', 0.0
)
## Values that don't have grade point equivalents:  
## I, NG, NS, P, S, WD2, WN, Y

count(phi_crs, COURSE_LETTER_GRADE) %>%
    pull(COURSE_LETTER_GRADE)
count(phi_crs, COURSE_LETTER_GRADE) %>%
    left_join(grade_points) %>%
    ggplot(aes(grade_points, n)) + geom_col()

## Course-level calculated statistics
crs_2 %>%
    select(-course_id) %>%
    cor(use = 'pairwise.complete') %>%
    as.tibble(rownames = 'var1') %>%
    gather(key = var2, value, -var1) %>%
    # ggplot(aes(var1, var2, fill = value, label = value)) +
    # geom_tile()
    filter(var1 < var2, value > .8) %>%
    arrange(desc(value))
