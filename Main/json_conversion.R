convert_FuzzyInferenceSystem_to_list <- function(fuzzy_inference_system){
  list(
    linguistic_variable_list = fuzzy_inference_system$linguistic_variable_list %>% 
      map(~.x %>% 
            map(~.x[which(names(.x) != 'membership_function')])),
    
    fuzzy_proposition_list = fuzzy_inference_system$fuzzy_proposition_list
  )
}

convert_list_to_FuzzyInferenceSystem <- function(x_list){
  
  linguistic_variable_list <- x_list$linguistic_variable_list %>% map(~ .x %>% map(function(y_list){
    fuzzy_set <- if(y_list$type == 's_fuzzy_set'){
      s_fuzzy_set(y_list$p1, y_list$p2)
    }else if(y_list$type == 'z_fuzzy_set'){
      z_fuzzy_set(y_list$p1, y_list$p2)
    }else if(y_list$type == 'trapezoidal_fuzzy_set'){
      trapezoidal_fuzzy_set(y_list$p1, y_list$p2, y_list$p3, y_list$p4)
    }else if(y_list$type == 'gaussian_fuzzy_set'){
      gaussian_fuzzy_set(y_list$gaussian_mean, y_list$gaussian_sd)
    }
    fuzzy_set
  }))
  
  fis <- FuzzyInferenceSystem$new()
  fis$fuzzy_proposition_list <- x_list$fuzzy_proposition_list
  fis$linguistic_variable_list <- linguistic_variable_list
  
  fis
}
