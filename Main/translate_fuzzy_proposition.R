
library(purrr)
library(dplyr)

translate_fuzzy_proposition <- function(fuzzy_proposition){
  if(fuzzy_proposition$type == 'simple_fuzzy_proposition'){
    linguistic_variable_name <- fuzzy_proposition$linguistic_variable_name
    fuzzy_set_name <- fuzzy_proposition$fuzzy_set_name
    text <- paste(linguistic_variable_name, 'IS', fuzzy_set_name)
  }else if(fuzzy_proposition$type == 'union_fuzzy_proposition'){
    text_vec <- map(fuzzy_proposition$argument_list, translate_fuzzy_proposition) %>% unlist
    text <- paste0('(',  text_vec, ')', collapse = ' OR ')
  }else if(fuzzy_proposition$type == 'intersection_fuzzy_proposition'){
    text_vec <- map(fuzzy_proposition$argument_list, translate_fuzzy_proposition) %>% unlist
    text <- paste0('(',  text_vec, ')', collapse = ' AND ')
  }else if(fuzzy_proposition$type == 'negation_fuzzy_proposition'){
    if(fuzzy_proposition$argument$type == 'simple_fuzzy_proposition'){
      linguistic_variable_name <- fuzzy_proposition$argument$linguistic_variable_name
      fuzzy_set_name <- fuzzy_proposition$argument$fuzzy_set_name
      text <- paste(linguistic_variable_name, 'IS NOT', fuzzy_set_name)
      
    }else if(fuzzy_proposition$argument$type %in% c('union_fuzzy_proposition', 'intersection_fuzzy_proposition')){
      text <- paste0('it is NOT true that (', translate_fuzzy_proposition(fuzzy_proposition$argument), ')')
      
    }else if(fuzzy_proposition$argument$type == 'negation_fuzzy_proposition'){
      text <- translate_fuzzy_proposition(fuzzy_proposition$argument$argument)
    }
  
  }else{
    stop('Cannot translate. Not a fuzzy proposition.')
  }
  
  return(text)
}
