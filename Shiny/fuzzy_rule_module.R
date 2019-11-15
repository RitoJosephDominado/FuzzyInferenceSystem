
fuzzy_rule_ui <- function(ui_name, main, index){
  ns <- NS(ui_name)
  
  name <- main$consequent_vec[index]
  title <- substr(index, start = 5, stop = nchar(main$consequent_vec[index]))
  box(
    width = 12, title= paste('Fuzzy rule', title), #background = 'red',
    fluidRow(
      div(
        class = 'col-sm-12 col-md-12 col-lg-7',
        tags$div(id = ns('fuzzy_proposition_ui_div'))
      ),
      
      column(2, h3('THEN')),
      column(3, textInput(ns('consequent_text'), 'Consequent', value = name))
    ),
    column(3, actionButton(ns('delete_btn'), 'Delete'))
  ) %>% div(id = ns('fuzzy_rule_div'))
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
      parent = main$fuzzy_proposition_environment_list,
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
  
  consequent_observer <- observeEvent(input$consequent_text, ignoreInit = TRUE, {
    if(grepl('^\\s*$', index)){
      return(NULL)
    }

    main$consequent_vec[index] <- input$consequent_text
  })
  
  observeEvent(input$delete_btn, ignoreInit = TRUE, {
    main$fuzzy_inference_system$fuzzy_proposition_list[[index]] <- NULL
    main$fuzzy_proposition_environment_list[[index]] <- NULL
    main$consequent_vec <- main$consequent_vec[which(! names(main$consequent_vec) %in% index)]
    
    consequent_observer$destroy()
    
    removeUI(
      selector = paste0('#', session$ns('fuzzy_rule_div'))
    )
  })
}

