add_fuzzy_rule_ui <- function(name){
  ns <- NS(name)
  tagList(
    # width = 12, title = 'Add fuzzy rules',
    selectInput(
      ns('fuzzy_proposition_type_select'), 'Type', 
      choices = c(
        'Simple' = 'simple_fuzzy_proposition', 
        'Union' = 'union_fuzzy_proposition', 
        'Intersection' = 'intersection_fuzzy_proposition'
      )
    ),
    actionButton(ns('add_fuzzy_rule_btn'), 'Add'),
    tags$div(id = ns('fuzzy_rule_ui_div'))
  )
}

add_fuzzy_rule_server <- function(input, output, session, main, triggers){
  
  observeEvent(input$add_fuzzy_rule_btn, {
    selected_type <- input$fuzzy_proposition_type_select
    
    
    
    
    if(input$fuzzy_proposition_type_select == 'simple_fuzzy_proposition'){
      fuzzy_proposition <- simple_fuzzy_proposition(NULL, NULL)
    }else if(input$fuzzy_proposition_type_select == 'union_fuzzy_proposition'){
      fuzzy_proposition <- union_fuzzy_proposition()
    }else if(input$fuzzy_proposition_type_select == 'intersection_fuzzy_proposition'){
      fuzzy_proposition <- intersection_fuzzy_proposition()
    }
    index <- length(main$fuzzy_inference_system$fuzzy_proposition_list) + 1
    main$fuzzy_inference_system$fuzzy_proposition_list[[index]] <- fuzzy_proposition
    
    
    
    insertUI(
      selector = paste0('#', session$ns('fuzzy_rule_ui_div')),
      ui = fuzzy_rule_ui(
        session$ns(index), 
        index = index
      )
      
    )
    
    callModule(
      fuzzy_rule_server, index,
      main = main, triggers = triggers,
      parent = main$fuzzy_inference_system$fuzzy_proposition_list,
      index = index
    )
    
  })
}



