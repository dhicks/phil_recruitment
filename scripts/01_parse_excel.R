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
    ## These calculated various either contain errors, don't have a clear meaning, or are mostly missing
    select(-IsPHI_MAJOR, -PHI_CNT, -IsPHI_MINOR) %>%
    ## A few students have two entries, with different values of IsPHI_MAJOR
    filter(!duplicated(.))

## Previous code to filter out repeated student IDs
## The IsPHI_MAJOR appears to have errors, so instead we just drop it and filter out duplicates
# repeats = count(profile_unfltd, ID) %>%
#     filter(n > 1) %>%
#     inner_join(profile_unfltd) %>%
#     filter(IsPHI_MAJOR == 'N') %>%
#     select(ID, IsPHI_MAJOR)
# profile = anti_join(profile_unfltd, repeats)

## 1 row per student
assert_that(length(unique(profile$ID)) == nrow(profile))

phi_crs = read_excel(str_c(data_folder, data_file), 'PHI_CRS')
assert_that(length(unique(phi_crs$E_PIDM)) == nrow(profile))

major_term = read_excel(str_c(data_folder, data_file), 'Major_Term')
assert_that(length(unique(major_term$E_PIDM)) == nrow(major_term))
assert_that(nrow(major_term) == nrow(profile))


## Major by term ----
#' The dataset provides major data in an awkward format:  Each student is represented by a single row.  The column ADMIT_TERM identifies the term the student began at Davis.  Then a series of columns give the student's major.  These columns are formatted as "relative terms," eg, YR3SPRING.  There are no columns for summer sessions, and the YR2SPRING column is missing.  
#' 
#' This means cleaning the major data involves 3 transformations:  lengthening the dataset (1 row = 1 quarter); interpolating majors for missing terms; and converting relative terms to absolute terms, eg, 200601.  In addition, because students can be admitted during any quarter and often have missing data for intermediate quarters (perhaps because they were on leave?), we can't simply filter out missing values.  
#' 
#' The following block first defines a set of functions used for incrementing absolute terms — eg, 200601 -> 200603 and 201010 -> 201101 — and then generating a list of incremented terms of a given length starting from the student's admit_term.  rel_term contains a full list of relative terms, from YR1FALL through YR10SUMMER_II for each student.  abs_terms converts relative terms to absolute terms for each student.  Finally, major_long combines these resources following the 3 transformations above.  
major_long_file = '01_major_long.Rds'
if (!file.exists(str_c(data_folder, major_long_file))) {
    # if (TRUE) {
    ## ~240 sec + time to write to disk
    increment_quarter = function (qtr) {
        case_when(qtr == '01' ~ '03',
                  qtr == '03' ~ '05',
                  qtr == '05' ~ '07',
                  qtr == '07' ~ '10',
                  qtr == '10' ~ '01')
    }
    increment_term = function (term) {
        new_qtr = increment_quarter(term$qtr)
        if (is.na(new_qtr)) stop(str_c('Bad term ', term))
        if (term$qtr == '10') {
            new_year = as.character(as.integer(term$year) + 1)
        } else {
            new_year = term$year
        }
        return(list(year = new_year, qtr = new_qtr))
    }
    
    generate_terms = function(admit_term, total_terms = 5*10) {
        admit_year = str_trunc(admit_term, 4, ellipsis = '')
        admit_qtr = str_trunc(admit_term, 2, ellipsis = '', side = 'left')
        ## Fix admit quarter == 06
        # admit_qtr = ifelse(admit_qtr == '06', '07', admit_qtr)
        admit_term = list(year = admit_year, qtr = admit_qtr)
        
        terms = vector('list', total_terms)
        terms[[1]] = admit_term
        for (i in 2:total_terms) {
            terms[[i]] = increment_term(terms[[i-1]])
        }
        # return(terms)
        terms_parsed = map_chr(terms, ~ str_c(.$year, .$qtr))
        return(terms_parsed)
    }
    
    rel_terms = expand.grid(fct_inorder(str_c('YR', 1:10)), 
                            fct_inorder(c('FALL', 'WINTER', 'SPRING', 
                                          'SUMMER_I', 'SUMMER_II')), 
                            major_term$E_PIDM, 
                            stringsAsFactors = FALSE) %>% 
        arrange(Var3, Var1, Var2) %>%
        transmute(id = Var3, rel_year = Var1, quarter = Var2, 
                  rel_term = str_c(Var1, Var2))
    
    # test_students = major_term %>%
    #     slice(1:1000) %>%
    #     pull(E_PIDM)
    
    tic()
    abs_terms = major_term %>%
        select(id = E_PIDM, admit_term = ADMIT_TERM) %>%
        # filter(id %in% test_students) %>%
        ## If admit_term is June (-06), replace it w/ July (-07)
        mutate(admit_term = str_replace(admit_term, '06$', '07')) %>%
        ## Generate df of terms
        rowwise() %>%
        mutate(abs_term = list(generate_terms(admit_term))) %>%
        ungroup() %>%
        unnest() %>%
        arrange(id, abs_term) %>%
        group_by(id) %>%
        mutate(term_idx = row_number())
    
    major_long = major_term %>%
        select(id = E_PIDM, admit_term = ADMIT_TERM, starts_with('YR')) %>%
        # filter(id %in% test_students) %>%
        ## If admit_term is June (-06), replace it w/ July (-07)
        mutate(admit_term = str_replace(admit_term, '06$', '07')) %>%
        ## Lengthen
        gather(key = rel_term, value = major, starts_with('YR')) %>%
        ## Interpolate missing relative terms
        full_join(rel_terms, by = c('id', 'rel_term')) %>%
        # filter(id %in% test_students) %>%
        arrange(id, rel_year, quarter) %>%
        ## Propagate admit_term and major
        group_by(id) %>%
        mutate(admit_term = na.locf(admit_term), 
               major = na.locf(major, na.rm = FALSE)) %>%
        ## Parse admit_term, filter, use this to construct term_idx
        mutate(first_qtr = str_trunc(admit_term, 2, ellipsis = '', side = 'left'),
               first_qtr = case_when(first_qtr == '10' ~ 'FALL', 
                                     first_qtr == '01' ~ 'WINTER', 
                                     first_qtr == '03' ~ 'SPRING', 
                                     first_qtr == '05' ~ 'SUMMER_I', 
                                     first_qtr == '07' ~ 'SUMMER_II'), 
               first_term = str_c('YR1', first_qtr)) %>% 
        ## Temporary index used to remove pre-admit terms
        mutate(temp_idx = row_number()) %>%
        filter(temp_idx >= temp_idx[rel_term == first_term]) %>% 
        select(-temp_idx) %>%
        ## Term index, used for joining to abs_terms
        mutate(term_idx = row_number()) %>%
        ungroup() %>% 
        ## Absolute terms
        left_join(abs_terms) %>% 
        ## Clean up
        select(id, admit_term, rel_term, term = abs_term, major) %>%
        ## Write to disk
        write_rds(str_c(data_folder, major_long_file))
    toc()
} else {
    major_long = read_rds(str_c(data_folder, major_long_file))
}
major_long = major_long %>%
    ## Dummies for major status that might influence decision to major in philosophy
    ## Human Development, Biological Sciences, and Sociology have philosophy in AB/BS reqs
    mutate(current_phil = str_detect(major, 'Philosophy'), 
           human_dev = str_detect(major, 'Human Development'), 
           bio_sci = str_detect(major, 'Biological Sciences'),
           soc = str_detect(major, 'Sociology'),
           undeclared = str_detect(major, 'Undeclared'))

## Minimum number of term per student should be 9 years x 5 terms/yr + 1 (admitted in Summer II)
# count(major_long, id) %>% arrange(n)
assert_that(min(count(major_long, id)$n) >= 9*5+1)
## Should have same number of students as profile
assert_that(nrow(count(major_long, id)) == nrow(profile))


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
           low_income = low_income == 'Y') %>%
    ## Move "American Indian/Alaska Native" and "Pacific Islander. other" ethnicities to distinct races
    ## Then collapse all remaining "Other" ethnicities
    ## There are very few Black, Hispanic, Indigenous, and PI individuals, so we construct a 4-valued variable that groups these
    ## And a race x gender analytical variable
    mutate(race = case_when(str_detect(ethnicity, 'Native') ~ 'Indigenous', 
                            str_detect(ethnicity, 'Islander') ~ 'Pacific Islander',
                            TRUE ~ race), 
           ethnicity = ifelse(race == 'Other', 'Other', ethnicity),
           poc = ! race %in% c('White', 'Other'),
           race4 = fct_collapse(race, 
                                White = 'White', 
                                Asian = 'Asian', 
                                BHIP = c('Black', 'Hispanic', 'Indigenous', 'Pacific Islander'), 
                                Other = 'Other'), 
           gender = fct_relevel(gender, 'M'),
           race4 = fct_relevel(race4, 'White', 'Asian', 'BHIP', 'Other'), 
           demographic = interaction(gender, race4, drop = TRUE))

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
    select(id:term_cum_gpa, major:undeclared) %>%
    ## Parse term into year and quarter
    separate(term, into = c('year', 'quarter'), sep = 4, 
             remove = FALSE, convert = TRUE) %>% 
    ## GPA gap
    mutate(grade_diff = term_cum_gpa - grade) %>% 
    ## Control majors
    mutate(other_major = human_dev|bio_sci|soc)

## 2. round:  course-level calculated variables
crs_2 = crs_1 %>%
    left_join(profile_clean) %>%
    group_by(course_id) %>%
    summarize(n_students = n(),
              mean_grade = mean(grade, na.rm = TRUE),
              mean_cum_gpa = mean(term_cum_gpa, na.rm = TRUE),
              mean_grade_diff = mean(grade_diff, na.rm = TRUE),
              women = sum(gender == 'F'), 
              poc = sum(poc), 
              first_gen = sum(first_gen), 
              low_income = sum(low_income), 
              current_phil = sum(current_phil, na.rm = TRUE), 
              human_dev = sum(human_dev, na.rm = TRUE), 
              bio_sci = sum(bio_sci, na.rm = TRUE), 
              soc = sum(soc, na.rm = TRUE), 
              other_major = sum(other_major, na.rm = TRUE),
              undeclared = sum(undeclared, na.rm = TRUE)) %>%
    mutate_at(vars(women:undeclared), 
              funs(share = ./n_students)) #%>%
# arrange(desc(n_students)) %>% 
## There are 56 courses where mean grade is empty
## These appear to be reading courses
# summarize_all(funs(empty = sum(is.na(.)))) %>% View

## 3. round:  difference in mean GPA gap (DMG) across demographics
## We use mean of mean gap across individual white men as the reference value
## To reduce the effect of philosophy majors, we first average within students, and then across students
ref_gpa_gap = crs_1 %>% 
    left_join(profile_clean) %>% 
    filter(demographic == 'M.White') %>% 
    group_by(id) %>% 
    summarize(grade_diff = mean(grade_diff)) %>% 
    pull(grade_diff) %>% 
    mean(na.rm = TRUE)

crs_3 = crs_1 %>%
    # filter(course_id %in% c('200701_56652', '200701_63653')) %>%
    left_join(profile_clean) %>% 
    group_by(course_id, demographic) %>% 
    summarize(grade_diff = mean(grade_diff, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(demographic, grade_diff) %>% 
    mutate_if(is.numeric, 
              funs(. - ref_gpa_gap)) %>% 
    gather(demographic, dmg, -course_id, factor_key = TRUE) %>% 
    filter(!is.na(dmg))

## The variance in DMG is substantial, with sd's around .5 grade points or larger
## Averaged across *courses* and ignoring Others, women are disadvantaged and white men are advantaged.  The effect is small, though, less than .1 grade points
crs_3 %>%
    group_by(demographic) %>%
    summarize_at(vars(dmg), funs(mean, sd, n = n())) %>%
    mutate(se = sd / sqrt(n),
           ci.lo = mean + qnorm(.025)*se,
           ci.hi = mean + qnorm(.975)*se) %>%
    ggplot(aes(demographic, mean, color = n)) +
    geom_pointrange(aes(ymin = ci.lo,
                        ymax = ci.hi)) +
    geom_hline(yintercept = 0) +
    scale_color_viridis_c(trans = 'log10', direction = -1) +
    coord_cartesian(ylim = c(-.2, .2))

## This might seem paradoxical:  How can white men be advantaged relative to white men?  Averaging at the course level, a large diverse intro course counts the same as a small majors course of ~all white men.  

## 4. round:  student-level CRS summaries, including major at first philosophy course
crs_4 = crs_1 %>%
    arrange(id, year, quarter) %>%
    group_by(id) %>%
    mutate(n_phil = n()) %>%
    filter(term == first(term)) %>%
    ungroup() %>% 
    select(id, course_id, major_at_first_phil = major, 
           phil_at_first_phil = current_phil,
           human_dev:undeclared,
           n_phil) %>% 
    nest(course_id, .key = 'first_phil_course')
assert_that(nrow(crs_4) == nrow(profile))

## ~500 students (4%) take 2+ philosophy courses during the first term they take philosophy
crs_4 %>%
    unnest() %>%
    count(id) %>% 
    count(n) %>%
    mutate(frac = nn / sum(nn))

## 279 students (1.7%) have already declared a major in philosophy when they take their first course
## 9 have missing major data during the term they first take philosophy
crs_4 %>%
    count(phil_at_first_phil) %>%
    mutate(frac = n / sum(n))

## In each of these 9 cases, they took their first philosophy course before they were formally admitted
filter(crs_4, is.na(phil_at_first_phil)) %>%
    unnest() %>%
    left_join(major_long) %>% 
    group_by(id) %>%
    filter(term == first(term)) %>%
    mutate(first_phil_term = str_trunc(course_id, 6, ellipsis = ''), 
           first_phil_before_admit = first_phil_term < admit_term) %>%
    pull(first_phil_before_admit) %>% 
    all() %>% 
    assert_that(msg = 'Not all NAs have 1. phil before admit')

## Most popular majors at first philosophy course
## Note that Human Development, Biological Sciences, Soc show up in top 10
count(crs_4, major_at_first_phil) %>%
    arrange(desc(n))

## Combine
crs_clean = full_join(crs_1, crs_2, 
                      by = 'course_id', 
                      suffix = c('.student', '.course'))
assert_that(nrow(crs_clean) == nrow(phi_crs))


## Output ----
profile_clean %>%
    full_join(crs_4) %>%
    write_rds(str_c(data_folder, '01_profile.Rds'))
write_rds(crs_clean, str_c(data_folder, '01_crs.Rds'))
write_rds(crs_3, str_c(data_folder, '01_dmg.Rds'))


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
