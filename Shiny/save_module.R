save_ui <- function(ui_name){
  ns <- NS(ui_name)
  tagList(
    box(
      width = 6, title = 'Linguistic variables',
      reactjsonOutput(ns('linguistic_variable_list_reactjson'))
    ),
    box(
      width = 6, title = 'Fuzzy propositions',
      reactjsonOutput(ns('fuzzy_proposition_list_reactjson'))
    ),
    box(
      width = 6, title = 'Fuzzy proposition environments',
      reactjsonOutput(ns('fuzzy_proposition_environment_list_reactjson'))
    ),
    box(
      width = 6,
      actionButton(ns('save_fuzzy_inference_system_btn'), 'Save (.json)')
    )
  )
}

save_server <- function(input, output, session, main, triggers){

  
  output$linguistic_variable_list_reactjson <- renderReactjson({
    # shiny::validate(main$fuzzy_inference_system$linguistic_variable_list)
    triggers$update_fuzzy_inference_system$depend()
    main$fuzzy_inference_system$linguistic_variable_list %>% reactjson
  })
  
  output$fuzzy_proposition_list_reactjson <- renderReactjson({
    triggers$update_fuzzy_inference_system$depend()
    
    main$fuzzy_inference_system$fuzzy_proposition_list <- map(main$fuzzy_proposition_environment_list, convert_environment_to_fuzzy_proposition)
    
    main$fuzzy_inference_system$fuzzy_proposition_list %>% reactjson
  })
  
  output$fuzzy_proposition_environment_list_reactjson <- renderReactjson({
    triggers$update_fuzzy_inference_system$depend()
    main$fuzzy_proposition_environment_list %>% map(.f = convert_environment_to_fuzzy_proposition) %>% reactjson
  })
  
  observeEvent(input$save_fuzzy_inference_system_btn, {
    print('saving!')
    
  })
}