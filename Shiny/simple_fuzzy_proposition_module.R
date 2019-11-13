
simple_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)
  
  linguistic_variables <- names(main$fuzzy_inference_system$linguistic_variable_list)

  x_simple_fuzzy_proposition <- parent[[index]]
  selected_linguistic_variable <- x_simple_fuzzy_proposition$linguistic_variable_name
  selected_fuzzy_set <- x_simple_fuzzy_proposition$fuzzy_set_name
  

  linguistic_variable_names <- names(main$fuzzy_inference_system$linguistic_variable_list)

  if(is.null(x_simple_fuzzy_proposition$fuzzy_set_name)){
    fuzzy_set_names <- c('')
  }else{
    fuzzy_set_names <- names(main$fuzzy_inference_system$linguistic_variable_list[[selected_linguistic_variable]]$fuzzy_set_list)
  }
  
  box(
    width = 12, title = 'Simple fuzzy proposition', status = 'success', solidHeader = TRUE,
    fluidRow(
      column(12, shinyWidgets::materialSwitch(ns('negate_switch'), strong('Negate'), status = 'primary', value = x_simple_fuzzy_proposition$negated), style = 'color:black')
    ),
    fluidRow(
      div(
        class = 'col-sm-12 col-md-5 col-lg-5',
        
        selectInput(ns('linguistic_variable_select'), 'Linguistic variable', choices = linguistic_variables, selected = selected_linguistic_variable)
      ),
      column(2, br(), textOutput(ns('is_negated_text')), br()),
      column(5, selectInput(ns('fuzzy_set_select'), 'Fuzzy set', choices = fuzzy_set_names, selected = selected_fuzzy_set))
    )
  )
}


simple_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  observeEvent(input$linguistic_variable_select, {
    selected_linguistic_variable <- input$linguistic_variable_select
    fuzzy_sets <- names(main$fuzzy_inference_system$linguistic_variable_list[[selected_linguistic_variable]]$fuzzy_set_list)
    if(is.null(fuzzy_sets)){
      fuzzy_sets <- ''
    }
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      choices = fuzzy_sets,
      selected = input$fuzzy_set_select
    )

    parent[[index]]$linguistic_variable_name <- selected_linguistic_variable
  })

  observeEvent(input$fuzzy_set_select, ignoreInit = TRUE,{
    parent[[index]]$fuzzy_set_name <- input$fuzzy_set_select
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      selected = input$fuzzy_set_select
    )
  })
  
  observe({
    triggers$update_fuzzy_inference_system$depend()
    selected_linguistic_variable <- input$linguistic_variable_select
    selected_fuzzy_set <- input$fuzzy_set_select
    linguistic_variable_names <- names(main$fuzzy_inference_system$linguistic_variable_list)
    
    if(is.null(selected_linguistic_variable) || (grepl('^\\s*$', selected_linguistic_variable)) || length(main$fuzzy_inference_system$linguistic_variable_list[[selected_linguistic_variable]]$fuzzy_set_list) == 0){
      fuzzy_set_names <- ''
    }else{
      fuzzy_set_names <- names(main$fuzzy_inference_system$linguistic_variable_list[[selected_linguistic_variable]]$fuzzy_set_list)
    }

    updateSelectInput(
      session = session,
      inputId = 'linguistic_variable_select',
      choices = linguistic_variable_names,
      selected = selected_linguistic_variable
    )
    
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      choices = fuzzy_set_names,
      selected = selected_fuzzy_set
    )
  })
  
  observeEvent(input$negate_switch, {
    parent[[index]]$negated <- input$negate_switch
  })
  
  output$is_negated_text <- renderText({
    if(input$negate_switch){
      'IS NOT'
    }else{
      'IS'
    }
  })
}

