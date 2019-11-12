
fuzzy_rule_ui <- function(ui_name, main, index){
  ns <- NS(ui_name)
  name <- names(main$fuzzy_inference_system$fuzzy_proposition_list)[index]
  if(is.null(name) || grepl('^\\s*$', name)){
    name <- paste('Rule', index)
  }
  box(
    width = 12, title= paste('Fuzzy rule', index), #background = 'red',
    div(
      class = 'col-sm-12 col-md-12 col-lg-7',
      tags$div(id = ns('fuzzy_proposition_ui_div'))
    ),
    
    column(2, h3('THEN')),
    column(3, textInput(ns('consequent_text'), 'Consequent', value = name))
  )
}


fuzzy_rule_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
  fuzzy_proposition <- main$fuzzy_proposition_environment_list[[index]]
  
  fuzzy_proposition_ui <- switch(
    fuzzy_proposition$type,
    'simple_fuzzy_proposition' = simple_fuzzy_proposition_ui,
    'intersection_fuzzy_proposition' = compound_fuzzy_proposition_ui,
    'union_fuzzy_proposition' = compound_fuzzy_proposition_ui,
    'negation_fuzzy_proposition' = negation_fuzzy_proposition_ui
  )
  
  insertUI(
    selector = paste0('#', session$ns('fuzzy_proposition_ui_div')),
    ui = fuzzy_proposition_ui(
      session$ns(index), 
      main = main,
      parent = main$fuzzy_inference_system$fuzzy_proposition_list,
      index = index
    )
  )
  
  fuzzy_proposition_server = switch (
    fuzzy_proposition$type,
   'simple_fuzzy_proposition' = simple_fuzzy_proposition_server,
   'intersection_fuzzy_proposition' = compound_fuzzy_proposition_server,
   'union_fuzzy_proposition' = compound_fuzzy_proposition_server,
   'negation_fuzzy_proposition' = negation_fuzzy_proposition_server
  )
  
  callModule(
    module = fuzzy_proposition_server,
    id = index,
    main = main, triggers = triggers,
    parent = main$fuzzy_proposition_environment_list, 
    index = index
  )
  
  observeEvent(input$consequent_text, {
    names(main$fuzzy_proposition_environment_list)[index] <- input$consequent_text
  })
}

