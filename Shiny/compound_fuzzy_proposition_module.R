
compound_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)
  fuzzy_proposition_type <- if(parent[[index]]$type == 'union_fuzzy_proposition'){
    'Union'
  }else if(parent[[index]]$type == 'intersection_fuzzy_proposition'){
    'Intersection'
  }
  
  box(
    width = 12, title = fuzzy_proposition_type, status = 'primary', solidHeader = TRUE,
    
    fluidRow(
      column(4, selectInput(ns('fuzzy_proposition_type_select'), 'Type', choices = c(
        Simple = 'simple_fuzzy_proposition',
        Intersection = 'intersection_fuzzy_proposition',
        Union = 'union_fuzzy_proposition'
      ))),
      column(4, actionButton(ns('add_fuzzy_proposition_btn'), 'Add'))
    ),
    tags$div(id = ns('fuzzy_proposition_ui_div'))
  )
}


compound_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
  
  observeEvent(input$add_fuzzy_proposition_btn, {
    
    # selected_type <- input$fuzzy_proposition_type_select
    # 
    # fuzzy_proposition <- if(selected_type == 'simple_fuzzy_proposition'){
    #   simple_fuzzy_proposition(NULL, NULL)
    # }else if(selected_type == 'intersection_fuzzy_proposition'){
    #   intersection_fuzzy_proposition()
    # }else if(selected_type == 'union_fuzzy_proposition'){
    #   union_fuzzy_proposition()
    # }else{
    #   stop('IIInvalid fuzzy proposition type (adding fuzzy propositions)')
    # }
    # index <- length(parent) + 1
    # parent[[index]] <- fuzzy_proposition
    # 
    # this <- parent[[index]]
    # 
    # insertUI(
    #   selector = paste0('#', session$ns('fuzzy_proposition_ui_div')),
    #   ui = simple_fuzzy_proposition_ui(index)
    # )
    # 
    # module <- switch (
    #   fuzzy_proposition$type,
    #   'simple_fuzzy_proposition' = simple_fuzzy_proposition_server,
    #   'intersection_fuzzy_proposition' = compound_fuzzy_proposition_server,
    #   'union_fuzzy_proposition' = compound_fuzzy_proposition_server
    # )
    # 
    # callModule(
    #   module = module,
    #   id = index,
    #   main = main, triggers = triggers,
    #   parent = this, index = index
    # )
    
    
    #---
    # fuzzy_proposition <- parent[[index]]
    
    
    child_index <- length(parent[[index]]$argument_list) + 1
    print(paste0('CHILD INDEX: ', child_index))
    fuzzy_proposition <- switch(
      input$fuzzy_proposition_type_select,
      'simple_fuzzy_proposition' = simple_fuzzy_proposition(NULL, NULL),
      'intersection_fuzzy_proposition' = intersection_fuzzy_proposition(),
      'union_fuzzy_proposition' = union_fuzzy_proposition()
    )
    
    parent[[index]]$argument_list[[child_index]] <- fuzzy_proposition %>% convert_fuzzy_proposition_to_environment
    
    fuzzy_proposition_ui <- switch(
      input$fuzzy_proposition_type_select,
      'simple_fuzzy_proposition' = simple_fuzzy_proposition_ui,
      'intersection_fuzzy_proposition' = compound_fuzzy_proposition_ui,
      'union_fuzzy_proposition' = compound_fuzzy_proposition_ui
    )
    
    insertUI(
      selector = paste0('#', session$ns('fuzzy_proposition_ui_div')),
      ui = fuzzy_proposition_ui(
        session$ns(child_index), 
        main = main,
        parent = parent[[index]]$argument_list,
        index = child_index
      )
    )
    
    fuzzy_proposition_server = switch (
      input$fuzzy_proposition_type_select,
      'simple_fuzzy_proposition' = simple_fuzzy_proposition_server,
      'intersection_fuzzy_proposition' = compound_fuzzy_proposition_server,
      'union_fuzzy_proposition' = compound_fuzzy_proposition_server
    )
    
    callModule(
      module = fuzzy_proposition_server,
      id = child_index,
      main = main, triggers = triggers,
      parent = parent[[index]]$argument_list, index = child_index
    )
  })
  
  
}