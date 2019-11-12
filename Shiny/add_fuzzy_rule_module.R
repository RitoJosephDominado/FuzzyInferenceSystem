add_fuzzy_rule_ui <- function(name){
  ns <- NS(name)
  tagList(
    # width = 12, title = 'Add fuzzy rules',
    box(
      width = 12,
      # fluidRow(
      verticalLayout(
        
      
        selectInput(
          ns('fuzzy_proposition_type_select'), 'Type', 
          choices = c(
            'Simple' = 'simple_fuzzy_proposition', 
            'Union' = 'union_fuzzy_proposition', 
            'Intersection' = 'intersection_fuzzy_proposition'
          ),width = '200px'
        ),
        actionButton(ns('add_fuzzy_rule_btn'), 'Add')
      )
    ),
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
    main$fuzzy_proposition_environment_list[[index]] <- convert_fuzzy_proposition_to_environment(fuzzy_proposition)

    insertUI(
      selector = paste0('#', session$ns('fuzzy_rule_ui_div')),
      ui = fuzzy_rule_ui(
        session$ns(index),
        main = main,
        index = index
      )
    )
    
    callModule(
      fuzzy_rule_server, index,
      main = main, triggers = triggers,
      parent = main$fuzzy_proposition_environment_list,
      index = index
    )
    
  })
  
  
  
  observe({
    triggers$uploaded_json$depend()
    isolate({
      lapply(seq_along(main$fuzzy_inference_system$fuzzy_proposition_list), function(i){
        insertUI(
          selector = paste0('#', session$ns('fuzzy_rule_ui_div')),
          ui = fuzzy_rule_ui(
            session$ns(i),
            main = main,
            index = i
          )
        )
        
        callModule(
          fuzzy_rule_server, i,
          main = main, triggers = triggers,
          parent = main$fuzzy_proposition_environment_list,
          index = i
        )
      })
    })
    
    
  })
}



