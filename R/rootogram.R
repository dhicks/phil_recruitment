rootogram = function(outcome, ## character
                     model,
                     threshold = .5, ...) {
    if (outcome == 'ever_phil') {
        rootogram_binom(model, 
                        response = outcome, 
                        quoted = TRUE,
                        threshold = threshold, ...)
    } else if (outcome == 'n_later_phil') {
        rootogram_count(model, 
                        response = outcome, 
                        quoted = TRUE,
                        ...)
    } else {
        stop('outcome not recognized')
    }
}

