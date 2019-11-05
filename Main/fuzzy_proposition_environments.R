
simple_fuzzy_proposition_environment <- function(linguistic_variable_name, fuzzy_set_name){
  env(
    type = 'simple_fuzzy_proposition',
    linguistic_variable_name = linguistic_variable_name,
    fuzzy_set_name = fuzzy_set_name,
    is_negated = NULL
  )
}

intersection_fuzzy_proposition_environment <- function(){
  env(
    type = 'intersection_fuzzy_proposition',
    argument_list = list()
  )
}

union_fuzzy_proposition_environment <- function(){
  env(
    type = 'union_fuzzy_proposition',
    argument_list = list()
  )
}

negation_fuzzy_proposition_environment <- function(fuzzy_proposition){
  env(
    type = 'negation_fuzzy_proposition',
    fuzzy_proposition = fuzzy_proposition
  )
}

convert_environment_to_fuzzy_proposition <- function(x_environment){
  if(x_environment$type == 'simple_fuzzy_proposition'){
    return(
      simple_fuzzy_proposition(
        x_environment$linguistic_variable_name,
        x_environment$fuzzy_set_name
      )
    )
  }else if(x_environment$type == 'intersection_fuzzy_proposition'){
    return(
      intersection_fuzzy_proposition(
        map(x_environment$argument_list, convert_environment_to_fuzzy_proposition)
      )
    )
  }else if(x_environment$type == 'union_fuzzy_proposition'){
    return(
      union_fuzzy_proposition(
        map(x_environment$argument_list, convert_environment_to_fuzzy_proposition)
      )
    )
  }
}



convert_fuzzy_proposition_to_environment <- function(fuzzy_proposition){
  if(fuzzy_proposition$type == 'simple_fuzzy_proposition'){
    return(
      simple_fuzzy_proposition_environment(
        fuzzy_proposition$linguistic_variable_name,
        fuzzy_proposition$fuzzy_set_name
      )
    )
  }else if(fuzzy_proposition$type == 'intersection_fuzzy_proposition'){
    return(
      intersection_fuzzy_proposition_environment(
        map(fuzzy_proposition$argument_list, convert_fuzzy_proposition_to_environment)
      )
    )
  }else if(fuzzy_proposition$type == 'union_fuzzy_proposition'){
    return(
      union_fuzzy_proposition(
        map(fuzzy_proposition$argument_list, convert_fuzzy_proposition_to_environment)
      )
    )
  }
}


