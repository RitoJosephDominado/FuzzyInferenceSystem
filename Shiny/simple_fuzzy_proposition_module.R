
simple_fuzzy_proposition_ui <- function(ui_name, main, parent, index){
  ns <- NS(ui_name)

  x_simple_fuzzy_proposition <- parent[[index]]
  selected_linguistic_variable <- x_simple_fuzzy_proposition$linguistic_variable_name
  selected_fuzzy_set <- x_simple_fuzzy_proposition$fuzzy_set_name
  # x_linguistic_variable <- Filter(function(lv){
  #   lv$name == selected_linguistic_variable
  # }, main$fuzzy_inference_system$linguistic_variable_list)[[1]]
  # 
  temp <- Filter(function(lv){
    lv$name == selected_linguistic_variable
  }, main$fuzzy_inference_system$linguistic_variable_list)
  
  if(length(temp) == 0){
    x_linguistic_variable <- NULL
  }else{
    x_linguistic_variable <- temp[[1]]
  }
  
  
  linguistic_variable_name_vec <- main$fuzzy_inference_system$linguistic_variable_list %>% map(~.x$name) %>% unlist %>% unname

  if(is.null(x_simple_fuzzy_proposition$fuzzy_set_name)){
    fuzzy_set_name_vec <- c('')
  }else{
    fuzzy_set_name_vec <- names(x_linguistic_variable$fuzzy_set_list)
  }
  
  box(
    width = 12, title = 'Simple fuzzy proposition', status = 'success', solidHeader = TRUE,
    fluidRow(
      column(6, shinyWidgets::materialSwitch(ns('negate_switch'), strong('Negate'), status = 'primary', value = x_simple_fuzzy_proposition$negated))
    ),
    
    fluidRow(
      div(
        class = 'col-sm-8 col-md-5 col-lg-5',
        selectInput(ns('linguistic_variable_select'), 'Linguistic variable', choices = linguistic_variable_name_vec, selected = selected_linguistic_variable)
      ),
      column(2, br(), textOutput(ns('is_negated_text')), br()),
      div(
        class = 'col-sm-8 col-md-5 col-lg-5',
        selectInput(ns('fuzzy_set_select'), 'Fuzzy set', choices = fuzzy_set_name_vec, selected = selected_fuzzy_set)
      )
    )
  ) %>% div(id = ns('simple_fuzzy_proposition_div'))
}


simple_fuzzy_proposition_server <- function(input, output, session, main, triggers, parent = NULL, index){
  observeEvent(input$linguistic_variable_select, {
    selected_linguistic_variable <- input$linguistic_variable_select
    selected_fuzzy_set <- input$fuzzy_set_select
    linguistic_variable_name_vec <- main$fuzzy_inference_system$linguistic_variable_list %>% map(~.x$name) %>% unlist %>% unname
    
    if(is.null(selected_linguistic_variable)) return(NULL)
    
    temp <- Filter(function(lv){
      lv$name == selected_linguistic_variable
    }, main$fuzzy_inference_system$linguistic_variable_list)
    
    if(length(temp) > 0){
      x_linguistic_variable <- temp[[1]]
    }else{
      return(NULL)
    }
    
    if(is.null(selected_linguistic_variable) || (grepl('^\\s*$', selected_linguistic_variable)) || length(x_linguistic_variable$fuzzy_set_list) == 0){
      fuzzy_set_name_vec <- ''
    }else{
      fuzzy_set_name_vec <- names(x_linguistic_variable$fuzzy_set_list)
    }
    
    updateSelectInput(
      session = session,
      inputId = 'linguistic_variable_select',
      choices = linguistic_variable_name_vec,
      selected = selected_linguistic_variable
    )
    
    updateSelectInput(
      session = session,
      inputId = 'fuzzy_set_select',
      choices = fuzzy_set_name_vec
      # selected = selected_fuzzy_set
    )
    
    #----test end----
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
    isolate({
      selected_linguistic_variable <- input$linguistic_variable_select
      selected_fuzzy_set <- input$fuzzy_set_select
      linguistic_variable_name_vec <- main$fuzzy_inference_system$linguistic_variable_list %>% map(~.x$name) %>% unlist %>% unname
      
      if(is.null(selected_linguistic_variable)) return(NULL)
      temp <- Filter(function(lv){
        lv$name == selected_linguistic_variable
      }, main$fuzzy_inference_system$linguistic_variable_list)

      if(length(temp) > 0){
        x_linguistic_variable <- temp[[1]]
      }else{
        x_linguistic_variable <- NULL
      }
      
      if(is.null(x_linguistic_variable)){
        selected_linguistic_variable <- ''
      }
      
      if(is.null(selected_linguistic_variable) || (grepl('^\\s*$', selected_linguistic_variable)) || length(x_linguistic_variable$fuzzy_set_list) == 0){
        fuzzy_set_name_vec <- ''
      }else{
        fuzzy_set_name_vec <- names(x_linguistic_variable$fuzzy_set_list)
      }
      
      if(is.null(linguistic_variable_name_vec)) linguistic_variable_name_vec <- ''
      
      updateSelectInput(
        session = session,
        inputId = 'linguistic_variable_select',
        choices = linguistic_variable_name_vec,
        selected = selected_linguistic_variable
      )
      
      updateSelectInput(
        session = session,
        inputId = 'fuzzy_set_select',
        choices = fuzzy_set_name_vec,
        selected = selected_fuzzy_set
      )
    })
    
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

