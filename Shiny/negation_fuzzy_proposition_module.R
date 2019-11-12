negation_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)
  x_negation_fuzzy_proposition <- parent[[index]]
  box(
    width = 12, title = 'Negation', status = 'warning', solidHeader = TRUE,
    h2('negation')
  )
  
}


negation_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
}