
simple_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)
  
  # linguistic_variables <- names(main$fuzzy_inference_system$linguistic_variable_list)

  # print(length(main$fuzzy_inference_system$linguistic_variable_list))
  # if(length(main$fuzzy_inference_system$linguistic_variable_list) == 0){
  #   fuzzy_sets <- NULL
  # }else{
  #   fuzzy_sets <- names(main$fuzzy_inference_system$linguistic_variable_list[[1]])
  # }
  linguistic_variables <- NULL
  fuzzy_sets <- NULL
    
  box(
    width = 12, title = 'Simple fuzzy proposition', status = 'success', solidHeader = TRUE,
    fluidRow(
      column(5, selectInput(ns('linguistic_variable_select'), 'Linguistic variable', choices = linguistic_variables)),
      column(2, selectInput(ns('is_negated_select'), '', choices = c('IS' = 'is', 'IS NOT' = 'is_not'))),
      column(5, selectInput(ns('fuzzy_set_select'), 'Fuzzy set', choices = fuzzy_sets)),
      column(3, selectInput(ns('test_select'), 'testsss', choices = c('choice1', 'choice2')))
    )
  )
}


simple_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
  observe({
    triggers$update_fuzzy_inference_system$depend()
    selected_linguistic_variable <- input$linguistic_variable_select
    linguistic_variable_names <- names(main$fuzzy_inference_system$linguistic_variable_list)

    updateSelectInput(
      session = session,
      inputId = 'linguistic_variable_select',
      choices = linguistic_variable_names,
      selected = selected_linguistic_variable
    )
  })
  
  
  observeEvent(input$linguistic_variable_select, {
    selected_linguistic_variable <- input$linguistic_variable_select
    print('changed lv!')
    fuzzy_sets <- names(main$fuzzy_inference_system$linguistic_variable_list[[selected_linguistic_variable]])
    print(fuzzy_sets)
    if(is.null(fuzzy_sets)){
      fuzzy_sets <- ''
    }
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      choices = fuzzy_sets
    )
    
    parent[[index]]$linguistic_variable_name <- selected_linguistic_variable
  })
  
  observeEvent(input$fuzzy_set_select, {
    print('changed fs! --------------')
    print(session$ns(index))
    parent[[index]]$fuzzy_set_name <- input$fuzzy_set_select
  })
  
  
}

