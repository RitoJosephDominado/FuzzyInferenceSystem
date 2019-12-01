# Module for adding fuzzy rules. This is the second step, right after setting up your linguistic variables and their fuzzy sets
# Here you use the linguistic variables and fuzzy sets created in the previous tab to configure the logic you want in your inference system

add_fuzzy_rule_ui <- function(name){
  ns <- NS(name)
  tagList(
    # ** Box for adding fuzzy rules ----
    box(
      width = 12, title = 'Add Fuzzy Rule',
      
      fluidRow(
        div(
          class = 'col-sm-7 col-md-4 col-lg-2',
          selectInput(
            ns('fuzzy_proposition_type_select'), 'Type', 
            choices = c(
              'Simple' = 'simple_fuzzy_proposition', 
              'Union' = 'union_fuzzy_proposition', 
              'Intersection' = 'intersection_fuzzy_proposition'
            ),width = '200px'
          )
        ),
        div(class = 'col-sm-5 col-md-4 col-lg-2', br(), actionButton(ns('add_fuzzy_rule_btn'), 'Add'))
      )
    ),
    
    # ** div where fuzzy rule ui's will be put into ----
    tags$div(id = ns('fuzzy_rule_ui_div')),
    p('-')
  )
}

add_fuzzy_rule_server <- function(input, output, session, main, triggers){
  # ** Observer for adding fuzzy rules ----
  observeEvent(input$add_fuzzy_rule_btn, {
    
    # ** ** Creating a fuzzy proposition based on the value in the dropdown with default parameters ----
    if(input$fuzzy_proposition_type_select == 'simple_fuzzy_proposition'){
      fuzzy_proposition <- simple_fuzzy_proposition(NULL, NULL)
    }else if(input$fuzzy_proposition_type_select == 'union_fuzzy_proposition'){
      fuzzy_proposition <- union_fuzzy_proposition()
    }else if(input$fuzzy_proposition_type_select == 'intersection_fuzzy_proposition'){
      fuzzy_proposition <- intersection_fuzzy_proposition()
    }else if(input$fuzzy_proposition_type_select == 'negation_fuzzy_proposition'){
      fuzzy_proposition <- negation_fuzzy_proposition()
    }
    main$fuzzy_proposition_counter <- main$fuzzy_proposition_counter + 1
    index <- main$fuzzy_proposition_counter
    main$fuzzy_inference_system$fuzzy_proposition_list[[paste0('rule', index)]] <- fuzzy_proposition
    main$consequent_vec[paste0('rule', index)] <- paste0('rule', index)
    main$fuzzy_proposition_environment_list[[paste0('rule', index)]] <- convert_fuzzy_proposition_to_environment(fuzzy_proposition)
    
    insertUI(
      selector = paste0('#', session$ns('fuzzy_rule_ui_div')),
      ui = fuzzy_rule_ui(
        session$ns(index),
        main = main,
        index = paste0('rule', index)
      )
    )
    
    callModule(
      fuzzy_rule_server, index,
      main = main, triggers = triggers,
      parent = main$fuzzy_proposition_environment_list,
      index = paste0('rule', index)
    )
  })
  
  # ** Observer for uploading json files ----
  observe({
    triggers$uploaded_json$depend()
    isolate({
      runjs('document.getElementById("add_fuzzy_rule-fuzzy_rule_ui_div").innerHTML = "";')
      
      lapply(seq_along(main$fuzzy_inference_system$fuzzy_proposition_list), function(i){
        insertUI(
          selector = paste0('#', session$ns('fuzzy_rule_ui_div')),
          ui = fuzzy_rule_ui(
            session$ns(i),
            main = main,
            index = paste0('rule', i)
          )
        )
        
        callModule(
          fuzzy_rule_server, i,
          main = main, triggers = triggers,
          parent = main$fuzzy_proposition_environment_list,
          index = paste0('rule', i)
        )
      })
    })
  })
}



