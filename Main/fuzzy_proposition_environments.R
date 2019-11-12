
simple_fuzzy_proposition_environment <- function(linguistic_variable_name, fuzzy_set_name, negated = FALSE){
  env(
    type = 'simple_fuzzy_proposition',
    linguistic_variable_name = linguistic_variable_name,
    fuzzy_set_name = fuzzy_set_name,
    negated = FALSE
  )
}

intersection_fuzzy_proposition_environment <- function(..., negated = FALSE){
  env(
    type = 'intersection_fuzzy_proposition',
    argument_list = list(...),
    negated = negated
  )
}

union_fuzzy_proposition_environment <- function(..., negated = FALSE){
  env(
    type = 'union_fuzzy_proposition',
    argument_list = list(...),
    negated = negated
  )
}

negation_fuzzy_proposition_environment <- function(fuzzy_proposition){
  env(
    type = 'negation_fuzzy_proposition',
    argument = fuzzy_proposition
  )
}


convert_environment_to_fuzzy_proposition <- function(x_environment){
  if(x_environment$type == 'simple_fuzzy_proposition'){
    return(
      simple_fuzzy_proposition(
        x_environment$linguistic_variable_name,
        x_environment$fuzzy_set_name,
        negated = x_environment$negated
      )
    )
  }else if(x_environment$type == 'intersection_fuzzy_proposition'){
    return(
      do.call(
        intersection_fuzzy_proposition,
        c(
          map(x_environment$argument_list, convert_environment_to_fuzzy_proposition),
          negated = x_environment$negated
        )
      )
    )
  }else if(x_environment$type == 'union_fuzzy_proposition'){
    return(
      do.call(
        union_fuzzy_proposition,
        c(
          map(x_environment$argument_list, convert_environment_to_fuzzy_proposition),
          negated = x_environment$negated
        )
        
      )
    )
  }else if(x_environment$type == 'negation'){
    return(
      negation_fuzzy_proposition(
        convert_environment_to_fuzzy_proposition(x_environment$argument)
      )
    )
  }
}



convert_fuzzy_proposition_to_environment <- function(fuzzy_proposition){
  if(is.null(fuzzy_proposition)) return(NULL)
  if(fuzzy_proposition$type == 'simple_fuzzy_proposition'){
    return(
      simple_fuzzy_proposition_environment(
        fuzzy_proposition$linguistic_variable_name,
        fuzzy_proposition$fuzzy_set_name
      )
    )
  }else if(fuzzy_proposition$type == 'intersection_fuzzy_proposition'){
    return(
      do.call(
        intersection_fuzzy_proposition_environment,
        map(fuzzy_proposition$argument_list, convert_fuzzy_proposition_to_environment)
      )
    )
  }else if(fuzzy_proposition$type == 'union_fuzzy_proposition'){
    return(
      do.call(
        union_fuzzy_proposition_environment,
        map(fuzzy_proposition$argument_list, convert_fuzzy_proposition_to_environment)
      )
    )
  }else if(fuzzy_proposition$type == 'negation_fuzzy_proposition'){
    return(
      negation_fuzzy_proposition_environment(
        convert_fuzzy_proposition_to_environment(fuzzy_proposition$argument)
      )
    )
  }
}


