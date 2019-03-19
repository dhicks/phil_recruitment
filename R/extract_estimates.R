extract_estimates = function(model, var) {
    if (inherits(model, 'lm')) {
        coefs_df = summary(model)$coefficients %>%
            as_tibble(rownames = 'term') %>%
            select(term, estimate = Estimate, se = `Std. Error`)
    } else if (inherits(model, 'hurdle')) {
        coefs_df = summary(model)$coefficients %>% 
            map(as_tibble, rownames = 'term') %>% 
            bind_rows(.id = 'hurdle_component') %>% 
            select(term, estimate = Estimate, se = `Std. Error`, hurdle_component)
    } else {
        stop('Model class not recognized')
    }
    
    filtered_df = filter(coefs_df, str_detect(term, var))
    baseline = c('estimate' = filtered_df[[1, 2]], 
                 'se' = filtered_df[[1, 3]])
    
    split = filtered_df %>% 
        pull(term) %>% 
        str_split(':')
    
    demographic = split %>% 
        map(~ keep(., str_detect(., 'demographic'))) %>% 
        map_chr(~ ifelse(length(.) == 0, 
                     'demographicM.White', 
                     .))
    
    process = split %>% 
        map_chr(~keep(., negate(str_detect)(., 'demographic')))
    
    effects_df = filtered_df %>%
        # separate(col = term, into = c('process', 'term'), 
        #          sep = ':', fill = 'right') %>%
        # replace_na(list('term' = 'demographicM.White')) %>%
        mutate(process = process, term = demographic) %>% 
        mutate(term = str_remove(term, 'demographic')) %>%
        separate(col = term, into = c('gender', 'race'), 
                 sep = '\\.', remove = FALSE) %>%
        mutate(estimate.comb = ifelse(term != 'M.White', 
                                      estimate + baseline['estimate'], 
                                      estimate),
               se.comb = sqrt(se^2 + baseline['se']^2),
               ci.low = estimate.comb + qnorm(.025)*se.comb,
               ci.high = estimate.comb + qnorm(.975)*se.comb)
    return(effects_df)
}

