extract_estimates = function(model, var, interaction) {
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
    
    ## Extract estimates, combining across demographics if this is an interaction model
    filtered_df = filter(coefs_df, str_detect(term, var))
    if (interaction) {
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
        process = map_chr(split, 
                    ~keep(., negate(str_detect)(., 'demographic')))
        
        effects_df = filtered_df %>% 
            mutate(process = process, term = demographic) %>% 
            mutate(term = str_remove(term, 'demographic')) %>% 
            separate(col = term, into = c('gender', 'race'), 
                     sep = '\\.', remove = FALSE) %>% 
            mutate(estimate.comb = ifelse(term != 'M.White', 
                                          estimate + baseline['estimate'], 
                                          estimate),
                   se.comb = sqrt(se^2 + baseline['se']^2))
    } else {
        effects_df = filtered_df %>% 
            mutate(process = term, 
                   estimate.comb = estimate, 
                   se.comb = se) %>% 
            select(-term) %>% 
            mutate(term = 'combined', 
                   gender = 'combined', 
                   race = 'combined')
    }

    ## CIs
    effects_df = effects_df %>%
        mutate(ci.low = estimate.comb + qnorm(.025)*se.comb,
               ci.high = estimate.comb + qnorm(.975)*se.comb)
    return(effects_df)
}

