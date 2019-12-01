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
      width = 6,
      textInput(ns('file_name_text'), 'Filename', value = 'FIS'),
      actionButton(ns('save_fuzzy_inference_system_btn'), 'Save (.json)')
    )
  )
}

save_server <- function(input, output, session, main, triggers){
  output$linguistic_variable_list_reactjson <- renderReactjson({
    triggers$update_fuzzy_inference_system$depend()

    lv_list <- main$fuzzy_inference_system$linguistic_variable_list
    names(lv_list) <- main$fuzzy_inference_system$linguistic_variable_list %>% map(~ .x$name) %>% unlist
    
    lv_list <- lv_list %>% map(~ list(
      name = .x$name,
      xlim = .x$xlim,
      fuzzy_set_list = .x$fuzzy_set_list %>% map(~.x[which(names(.x) != 'membership_function')])
    ))
    
    lv_list %>% reactjson
  })
  
  output$fuzzy_proposition_list_reactjson <- renderReactjson({
    triggers$update_fuzzy_inference_system$depend()
    
    main$fuzzy_inference_system$fuzzy_proposition_list <- map(main$fuzzy_proposition_environment_list, convert_environment_to_fuzzy_proposition)
    names(main$fuzzy_inference_system$fuzzy_proposition_list) <- main$consequent_vec
    main$fuzzy_inference_system$fuzzy_proposition_list %>% reactjson
  })
  
  observeEvent(input$save_fuzzy_inference_system_btn, {
    temp_fis <- as.environment(as.list(main$fuzzy_inference_system, all.names = TRUE))
    names(temp_fis$linguistic_variable_list) <- temp_fis$linguistic_variable_list %>% map(~ .x$name) %>% unlist
    
    json <- temp_fis %>% convert_FuzzyInferenceSystem_to_list %>% toJSON(pretty = TRUE)
    filename <- paste0('JSON/', input$file_name_text, '.json')
    write(json, filename)
    shinyalert(
      'Saved', paste0('Successfully saved ', filename, '.csv'),
      type = 'success'
    )
  })
}
