estimates_plot = function(data) {
    thresholds = tribble(
        ~model_group, ~level, ~high,
        'linear', 'strong', Inf, 
        'linear', 'moderate', .6, 
        'linear', 'negligible', .25*.6,
        'linear', 'moderate', -.25*.6, 
        'linear', 'strong', -.6,
        'linear', 'left', -Inf,
        
        'logistic', 'strong', Inf,
        'logistic', 'moderate', 2-1,
        'logistic', 'negligible', 1.25-1,
        'logistic', 'moderate', 1/1.25 - 1, 
        'logistic', 'strong', 1/2 - 1, 
        'logistic', 'left', 0 - 1,
        
        'count', 'strong', Inf, 
        'count', 'moderate', 2-1, 
        'count', 'negligible', 1.25-1,
        'count', 'moderate', 1/1.25 - 1, 
        'count', 'strong', 1/2 - 1, 
        'count', 'left', 0 - 1
    ) %>% 
        ## Lead thresholds to get low/left-hand endpoints
        group_by(model_group) %>% 
        mutate(low = lead(high)) %>% 
        filter(level != 'left') %>% 
        ungroup() %>% 
        ## Stabilize order of model groups and levels
        mutate(model_group = fct_inorder(model_group),
               level = fct_rev(fct_inorder(level))) %>%  
        ## Combine with the processes included in the data, for faceting
        crossing(process = unique(data$process))
    
    p = ggplot(data = data, 
           aes(x = term, y = estimate.comb,
               ymin = ci.low, ymax = ci.high,
               color = race, 
               shape = model_type,
               linetype = gender,
               group = model_idx)) +
        geom_linerange(position = position_dodge(width = 1)) +
        geom_point(position = position_dodge(width = 1)) +
        facet_wrap(vars(process, model_group),
               dir = 'v',
               scales = 'free_y',
               drop = FALSE,
               nrow = n_distinct(estimates$model_group))
        
    yrange = map(ggplot_build(p)$layout$panel_scales_y, ~.$range$range)
    return(yrange)
    
    p = p +
        geom_rect(data = thresholds,
                  inherit.aes = FALSE,
                  aes(xmin = 'M.White', xmax = 'combined',
                      ymin = high, ymax = low,
                      alpha = level),
                  show.legend = FALSE) +
        scale_alpha_manual(values = c(0, .1, .2)) +
        geom_hline(yintercept = 0,
                   # data = reg_form,
                   linetype = 'dashed') +
        coord_flip(xlim = yrange) +
        xlab('') +
        ylab('estimated effect') +
        scale_y_continuous(labels = scales::percent_format(), expand = expand_scale(mult = c(0, .05))) +
        scale_linetype_manual(guide = guide_legend(order = 1), 
                              values = c('M' = 'dotted', 'F' = 'dashed', 
                                         'combined' = 'solid')) +
        scale_color_brewer(palette = 'Set1', guide = guide_legend(order = 2)) +
        scale_shape_manual(name = 'model', 
                           guide = guide_legend(nrow = 2, byrow = TRUE, 
                                                order = 3), 
                           values = c('lm' = 16, 'logistic' = 17, 'bias-reduced logistic' = 2, 
                                      'Poisson' = 23, 'hurdle count' = 15, 'hurdle zero' = 0)) +
        theme_bw() +
        theme(legend.position = 'bottom',
              legend.key.height = unit(2, 'lines'))
    
    return(p)
}
