## Rootogram for Poisson and NB models, plus zi and hurdle variants
rootogram_count = function(model_list,
                           response, 
                           model_names = NULL,
                           quoted = FALSE, 
                           new_data = NULL, 
                           sqrt_scale = TRUE,
                           breaks = c(5, 10, 50, 1e2, 5e2, 1e3, 5e3, 1e4), 
                           minor_breaks = NULL
                           ) {
    ## Wrapper to simplify passing a single model
    if (!inherits(model_list, 'list')) {
        model_list = list(model_list)
    }
    names(model_list) = model_names
    
    ## glm and MASS models have broom tidiers; pscl models don't
    augment_model = function(model, new_data) {
        if (inherits(model, 'lm')) {
            augmented_df = augment(model, newdata = new_data, 
                                   type.predict = 'response')  
        } else if (inherits(model, 'hurdle') | inherits(model, 'zeroinfl')) {
            augmented_df = mutate(new_data, 
                                  .fitted = predict(model, 
                                                    newdata = new_data, 
                                                    type = 'response'))
        } else {
            stop('Model class not recognized')
        }
        return(augmented_df)
    }
    
    if (quoted) {
        response_var = sym(response)
    } else {
        response_var = enquo(response)
    }
    
    if (is.null(new_data)) {
        new_data = model_list[[1]]$model
    }
    
    augmented = map(model_list, augment_model, new_data)
    
    ## Plot of observed values
    obs_plot = ggplot(new_data) +
        stat_count(aes(!!response_var, group = 1L), 
                   color = 'black',
                   geom = 'col', fill = NA)
    
    ## Construct line and point layers for each model
    construct_prediction_layer = function(model, name) {
        list(stat_bin(data = model, 
                      aes(.fitted, color = as.character(name), group = 1L), 
                      binwidth = 1, 
                      geom = 'line'),
             stat_bin(data = model, 
                      aes(.fitted, color = as.character(name), group = 1L), 
                      binwidth = 1, 
                      geom = 'point'))
    }
    prediction_layers = augmented %>% 
        imap(construct_prediction_layer) %>% 
        flatten()
    ## `+.gg()` understands a list of layers
    plot = obs_plot + prediction_layers
    
    ## Conventionally a sqrt transform allows comparison in the tails
    if (sqrt_scale) {
        plot = plot + scale_y_sqrt(breaks = breaks, 
                                   minor_breaks = minor_breaks)
    }
    return(plot)
}


# rootogram_count(model_pois, 
#                 NULL, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Poisson, training data',
#             subtitle = Sys.time())
# rootogram_count(model_pois, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Poisson, testing data',
#             subtitle = Sys.time())
# 
# 
# rootogram_count(model_nb, 
#                 NULL,
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Negative binomial, training data', 
#             subtitle = Sys.time())
# rootogram_count(model_nb, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Negative binomial, testing data', 
#             subtitle = Sys.time())
# 
# 
# rootogram_count(model_hurdle, 
#                 NULL,
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Hurdle negative binomial, training data', 
#             subtitle = Sys.time())
# rootogram_count(model_hurdle, 
#                 test_df, 
#                 n_later_phil) +
#     theme_minimal() +
#     ggtitle('Hurdle negative binomial, testing data', 
#             subtitle = Sys.time())
