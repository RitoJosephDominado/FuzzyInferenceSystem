
simple_fuzzy_proposition <- function(linguistic_variable_name, fuzzy_set_name, negated = FALSE){
  list(
    type = 'simple_fuzzy_proposition',
    negated = negated,
    linguistic_variable_name = linguistic_variable_name,
    fuzzy_set_name = fuzzy_set_name
  )
}

union_fuzzy_proposition <- function(..., negated = FALSE){
  list(
    type = 'union_fuzzy_proposition',
    negated = negated,
    argument_list = list(...)
  )
}

intersection_fuzzy_proposition <- function(..., negated = FALSE){
  list(
    type = 'intersection_fuzzy_proposition',
    negated = negated,
    argument_list = list(...)
  )
}

negation_fuzzy_proposition <- function(fuzzy_proposition = NULL){
  list(
    type = 'negation_fuzzy_proposition',
    argument = fuzzy_proposition
  )
}