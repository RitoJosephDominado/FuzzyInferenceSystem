
simple_fuzzy_proposition <- function(linguistic_variable_name, fuzzy_set_name){
  list(
    type = 'simple_fuzzy_proposition',
    linguistic_variable_name = linguistic_variable_name,
    fuzzy_set_name = fuzzy_set_name
  )
}

union_fuzzy_proposition <- function(...){
  list(
    type = 'union_fuzzy_proposition',
    argument_list = list(...)
  )
}

intersection_fuzzy_proposition <- function(...){
  list(
    type = 'intersection_fuzzy_proposition',
    argument_list = list(...)
  )
}

negation_fuzzy_proposition <- function(fuzzy_proposition){
  list(
    type = 'negation_fuzzy_proposition',
    argument = fuzzy_proposition
  )
}