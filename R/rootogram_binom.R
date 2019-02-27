## Construct a standing rootogoram for a binary response model
## [@KleiberVisualizingCountData2016] 
rootogram_binom = function(model, 
                           new_data = NULL, ## if not provided, uses fitting data
                           response, ## observed values of y
                           threshold, ## for discretizing fitted values
                           delogit = FALSE, ## do fitted values need to be de-logit-ed? 
                           sqrt_scale = TRUE, ## on the y axis
                           breaks = c(5, 10, 50, 1e2, 5e2, 1e3, 5e3, 1e4), 
                           minor_breaks = NULL
) {
    response_var = enquo(response)
    
    augmented_df = augment(model, newdata = new_data)
    if (delogit) {
        augmented_df = mutate(augmented_df, 
                              .fitted = boot::inv.logit(.fitted))
    }
    augmented_df = mutate(augmented_df, 
                          .predicted = .fitted > threshold)
    
    plot = ggplot(augmented_df) +
        stat_count(aes(!!response_var, color = 'observed', group = 1L), 
                   geom = 'col', fill = NA) +
        stat_count(aes(.predicted, color = 'fitted', group = 1L), 
                   geom = 'line') +
        stat_count(aes(.predicted, color = 'fitted', group = 1L), 
                   geom = 'point')
    if (sqrt_scale) {
        plot = plot + 
            scale_y_sqrt(breaks = breaks, 
                         minor_breaks = minor_breaks)
    }
    return(plot)
}
