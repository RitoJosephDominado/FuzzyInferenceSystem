make_reactive_trigger <- function(){
  rv <- reactiveValues(a = 0)
  list(
    depend = function(){
      rv$a
      invisible()
    },
    trigger = function(){
      rv$a <- isolate(rv$a + 1)
    }
  )
}


upload_ui <- function(name){
  ns <- NS(name)
  tagList(
    box(
      width = 12,
      column(6, fileInput(ns('fuzzy_inference_system_json_file'), 'Fuzzy inference system file (.json)')),
      column(3, br(), actionButton(ns('upload_fuzzy_inference_system_json'), 'Upload'))
    ),
    box(
      width = 6, title = 'Linguistic variables',
      reactjsonOutput(ns('linguistic_variable_list_reactjson'))
    ),
    box(
      width = 6, title = 'Fuzzy propositions',
      reactjsonOutput(ns('fuzzy_proposition_list_reactjson'))
    )
  )
}

upload_server <- function(input, output, session, main, triggers, uploaded){
  
  observeEvent(input$upload_fuzzy_inference_system_json, {
    json <- read_json(input$fuzzy_inference_system_json_file$datapath)
    fis <- json[[1]] %>% fromJSON(simplifyDataFrame = FALSE) %>% convert_list_to_FuzzyInferenceSystem
    main$fuzzy_inference_system <- fis
    uploaded$fuzzy_inference_system <- fis
    
    main$fuzzy_proposition_environment_list <- fis$fuzzy_proposition_list %>% map(convert_fuzzy_proposition_to_environment)
    main$consequent_vec <- names(main$fuzzy_inference_system$fuzzy_proposition_list)
    names(main$consequent_vec) <- paste0('rule', seq(1, length(main$consequent_vec)))
    names(main$fuzzy_proposition_environment_list) <- paste0('rule', seq(1, length(main$consequent_vec)))
    main$fuzzy_proposition_counter <- length(main$consequent_vec)
    
    showNotification('Uploaded json')
    triggers$uploaded_json$trigger()
    
  })

  output$linguistic_variable_list_reactjson <- renderReactjson({
    req(uploaded$fuzzy_inference_system$linguistic_variable_list)
    lv_list <- uploaded$fuzzy_inference_system$linguistic_variable_list
    lv_list <- lv_list %>% map(~.x$fuzzy_set_list %>% map(~.x[which(names(.x) != 'membership_function')]))
    lv_list %>% reactjson()
  })
  
  output$fuzzy_proposition_list_reactjson <- renderReactjson({
    req(uploaded$fuzzy_inference_system$fuzzy_proposition_list)
    uploaded$fuzzy_inference_system$fuzzy_proposition_list %>% reactjson
  })
  
  
}