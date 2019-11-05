
simple_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)
  
  linguistic_variables <- names(main$fuzzy_inference_system$linguistic_variable_list)
  # print('humm')
  # print(main$fuzzy_inference_system$linguistic_variable_list)
  # print(length(main$fuzzy_inference_system$linguistic_variable_list))
  if(length(main$fuzzy_inference_system$linguistic_variable_list) == 0){
    fuzzy_sets <- NULL
  }else{
    fuzzy_sets <- names(main$fuzzy_inference_system$linguistic_variable_list[[1]])
  }
  
    
  box(
    width = 12, title = 'Simple fuzzy proposition',
    fluidRow(
      column(5, selectInput(ns('linguistic_variable_select'), 'Linguistic variable', choices = linguistic_variables)),
      column(2, selectInput(ns('is_negated_select'), '', choices = c('IS' = 'is', 'IS NOT' = 'is_not'))),
      column(5, selectInput(ns('fuzzy_set_select'), 'Fuzzy set', choices = fuzzy_sets))
    )
  )
}


simple_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  
  observe({
    triggers$update_fuzzy_inference_system$depend()
    linguistic_variable_names <- names(main$fuzzy_inference_system$linguistic_variable_list)

    updateSelectInput(
      session = session,
      inputId = 'linguistic_variable_select',
      choices = linguistic_variable_names
    )
  })
  
  
  observeEvent(input$linguistic_variable_select, {
    selected_linguistic_variable <- input$linguistic_variable_select
    fuzzy_sets <- names(parent[[selected_linguistic_variable]])
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      choices = fuzzy_sets
    )
  })
  
}

