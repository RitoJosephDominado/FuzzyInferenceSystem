library(R6)

FuzzyInferenceSystem <- R6Class(
  classname = 'FuzzyInferenceSystem',
  public = list(
    linguistic_variable_list = list(),
    fuzzy_proposition_list = list()
  )
)

FuzzyInferenceSystem$set('public', 'evaluate_fuzzy_proposition_list', function(feature_df){
  result_list <- map(self$fuzzy_proposition_list, self$evaluate_fuzzy_proposition, feature_df = feature_df)
  result_df <- do.call(cbind, result_list)
  rownames(result_df) <- rownames(feature_df)
  colnames(result_df) <- names(self$fuzzy_proposition_list)
  return(result_df)
})

FuzzyInferenceSystem$set('public', 'evaluate_fuzzy_proposition', function(fuzzy_proposition, feature_df){
  
  if(fuzzy_proposition$type == 'simple_fuzzy_proposition'){
    linguistic_variable_name <- fuzzy_proposition$linguistic_variable_name
    fuzzy_set_name <- fuzzy_proposition$fuzzy_set_name
    
    
    temp <- Filter(function(lv){
      lv$name == linguistic_variable_name
    }, self$linguistic_variable_list)
    
    if(length(temp) == 0) return(NA_real_)
    x_linguistic_variable <- temp[[1]]
    
    membership_function <- x_linguistic_variable$fuzzy_set_list[[fuzzy_set_name]]$membership_function
    
    if(is.null(membership_function)){
      membership_df <- rep(NA_real_, nrow(feature_df))
    }else{
      membership_df <- membership_function(feature_df[, linguistic_variable_name])
    }

  }else if(fuzzy_proposition$type == 'union_fuzzy_proposition'){
    result_list <- map(fuzzy_proposition$argument_list, self$evaluate_fuzzy_proposition, feature_df = feature_df)
    if(length(result_list) > 0){
      membership_df <- do.call(pmax, result_list)
    }else{
      membership_df <- rep(NA_real_, nrow(feature_df))
    }
  }else if(fuzzy_proposition$type == 'intersection_fuzzy_proposition'){
    result_list <- map(fuzzy_proposition$argument_list, self$evaluate_fuzzy_proposition, feature_df = feature_df)
    if(length(result_list) > 0){
      membership_df <- do.call(pmin, result_list)
    }else{
      membership_df <- rep(NA_real_, nrow(feature_df))
    }
  }else if(fuzzy_proposition$type == 'negation_fuzzy_proposition'){
    return(1 - self$evaluate_fuzzy_proposition(fuzzy_proposition$argument, feature_df))
  }else{
    stop('Not a fuzzy proposition')
  }
  
  if(fuzzy_proposition$negated){
    return(1 - membership_df)
  }else{
    return(membership_df)
  }
})

FuzzyInferenceSystem$set('public', 'plot_feature', function(features, linguistic_variable_name){
  x_linguistic_variable <- self$linguistic_variable_list[[linguistic_variable_name]]
  num_fuzzy_sets <- length(x_linguistic_variable$fuzzy_set_list)
  rng <- seq(from = x_linguistic_variable$xlim[1], to = x_linguistic_variable$xlim[2], length.out = 100)
  plot(rng, x_linguistic_variable$fuzzy_set_list[[1]]$membership_function(rng), type = 'o', ylab = 'Membership', xlab = linguistic_variable_name)
  for(i in seq_len(num_fuzzy_sets)[-1]){
    lines(rng, x_linguistic_variable$fuzzy_set_list[[i]]$membership_function(rng), type = 'o')
  }
  abline(v = features[linguistic_variable_name], col = 'red', lwd = 2)
})
