
fuzzy_rule_ui <- function(ui_name, index){
  ns <- NS(ui_name)
  box(
    width = 12, title= paste('Fuzzy rule', index),
    # reactjsonOutput(ns('main_reactjson')),
    # fluidRow(
    #   column(4, h4('Fuzzy rules')),
    #   column(4, selectInput(ns('fuzzy_proposition_type_select'), 'Type', choices = c(
    #     Simple = 'simple_fuzzy_proposition',
    #     Intersection = 'intersection_fuzzy_proposition',
    #     Union = 'union_fuzzy_proposition'
    #   ))),
      # column(4, actionButton(ns('add_fuzzy_proposition_btn'), 'Add'))
    # ),
    column(7, tags$div(id = ns('fuzzy_proposition_ui_div'))),
    column(2, h3('THEN')),
    column(3, textInput(ns('consequent_text'), 'Consequent'))
  )
}


fuzzy_rule_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
  fuzzy_proposition <- main$fuzzy_inference_system$fuzzy_proposition_list[[index]]
  
  fuzzy_proposition_ui <- switch(
    fuzzy_proposition$type,
    'simple_fuzzy_proposition' = simple_fuzzy_proposition_ui,
    'intersection_fuzzy_proposition' = compound_fuzzy_proposition_ui,
    'union_fuzzy_proposition' = compound_fuzzy_proposition_ui
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
   'union_fuzzy_proposition' = compound_fuzzy_proposition_server
  )
  
  callModule(
    module = fuzzy_proposition_server,
    id = index,
    main = main, triggers = triggers,
    parent = main$fuzzy_inference_system$fuzzy_proposition_list, 
    index = index
  )
}