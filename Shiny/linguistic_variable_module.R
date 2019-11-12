

linguistic_variable_ui <- function(ui_name, linguistic_variable_name){
  ns <- NS(ui_name)
  
  box(
    width = 12, title = linguistic_variable_name,
    collapsible = TRUE,
    tags$div(
      id = ns('add_fuzzy_set_div'),
      box(
        width = 12, title = 'Add Fuzzy Set', collapsible = TRUE,
        status = 'primary', solidHeader = TRUE,
        fluidRow(
          div(
            # 6,
            class = 'col-sm-12 col-md-12 col-lg-6',
            box(
              width = 12, title = 'Parameters', status = 'success', solidHeader = TRUE,
              column(4, textInput(ns('fuzzy_set_name_text'), 'Name')),
              column(6, selectInput(ns('fuzzy_set_type_select'), 'Type', choices = c(
                'Z-shaped' = 'z',
                'S-shaped' = 's',
                'Trapezoidal' = 'trapezoidal',
                'Gaussian' = 'gaussian'
              ))),
              uiOutput(ns('parameters_ui')),
              fluidRow(column(4, actionButton(ns('add_fuzzy_set_btn'), 'Add')))
            )
          ),
          div(
            # 6,
            class = 'col-sm-12 col-md-12 col-lg-6',
            box(
              width = 12, title = 'Plot', status = 'success', solidHeader = TRUE,
              plotOutput(ns('main_plot'))
            )
            
          )
        )
      )
    ),
    box(
      title = 'Fuzzy Sets', width = 12, background = 'light-blue',
      tags$div(id = ns('fuzzy_set_ui_div'))
    )
  )
}


linguistic_variable_server <- function(input, output, session, main, triggers, linguistic_variable_name, rng){
  fuzzy_set_list <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]$fuzzy_set_list
  if(length(fuzzy_set_list) > 0){
    lapply(seq_along(fuzzy_set_list), function(i){
      fuzzy_set_name <- names(fuzzy_set_list)[i]
      ui_name <- paste0(linguistic_variable_name, '-', fuzzy_set_name)
      insertUI(
        selector = paste0('#', session$ns('fuzzy_set_ui_div')),
        ui = fuzzy_set_ui(ui_name = session$ns(ui_name), linguistic_variable_name = linguistic_variable_name, fuzzy_set_name = fuzzy_set_name)
      )

      callModule(
        fuzzy_set_server, ui_name,
        main = main, triggers = triggers,
        linguistic_variable_name = linguistic_variable_name,
        fuzzy_set_name = fuzzy_set_name
      )
    })
  }
  
  
  local_triggers <- reactiveValues(
    added_fuzzy_set = make_reactive_trigger()
  )
  
  output$parameters_ui <- renderUI({
    parameter_inputs <- if(input$fuzzy_set_type_select %in% c('z', 's')){
      list(
        column(3, numericInput(session$ns('p1_numeric'), 'P1', 0, -10000, 10000, step = 0.1)),
        column(3, numericInput(session$ns('p2_numeric'), 'P2', 0, -10000, 10000, step = 0.1))
      )
    }else if(input$fuzzy_set_type_select == 'trapezoidal'){
      list(
        column(3, numericInput(session$ns('p1_numeric'), 'P1', 0, -10000, 10000, step = 0.1)),
        column(3, numericInput(session$ns('p2_numeric'), 'P2', 0, -10000, 10000, step = 0.1)),
        column(3, numericInput(session$ns('p3_numeric'), 'P3', 0, -10000, 10000, step = 0.1)),
        column(3, numericInput(session$ns('p4_numeric'), 'P4', 0, -10000, 10000, step = 0.1))
      )
    }else if(input$fuzzy_set_type_select == 'gaussian'){
      list(
        column(3, numericInput(session$ns('gaussian_mean_numeric'), 'Mean', 0, -10000, 10000, step = 0.1)),
        column(3, numericInput(session$ns('gaussian_sd_numeric'), 'Standard deviation', 0, -10000, 10000, step = 0.1))
      )
    }
    fluidRow(parameter_inputs)
    
  })
  
  
  observeEvent(input$add_fuzzy_set_btn, {
    
    fuzzy_set_name <- input$fuzzy_set_name_text
    
    if(fuzzy_set_name %in% names(main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]])){
      showModal(
        modalDialog(
          title = 'Invalid Fuzzy Set Name',
          p(paste0('A fuzzy set with that name has already been added to ', linguistic_variable_name))
        )
      )
      return(NULL)
    }else if(grepl('^\\s*$', fuzzy_set_name)){
      showModal(
        modalDialog(
          title = 'Invalid Fuzzy Set Name',
          p('Cannot enter a fuzzy set name with just whitespace')
        )
      )
      return(NULL)
    }
    
    fuzzy_set <- if(input$fuzzy_set_type_select == 'z'){
      z_fuzzy_set(input$p1_numeric, input$p2_numeric)
    }else if(input$fuzzy_set_type_select == 's'){
      s_fuzzy_set(input$p1_numeric, input$p2_numeric)
    }else if(input$fuzzy_set_type_select == 'trapezoidal'){
      trapezoidal_fuzzy_set(input$p1_numeric, input$p2_numeric, input$p3_numeric, input$p4_numeric)
    }else if(input$fuzzy_set_type_select == 'gaussian'){
      gaussian_fuzzy_set(input$gaussian_mean_numeric, input$gaussian_sd_numeric)
    }
    
    main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]$fuzzy_set_list[[fuzzy_set_name]] <- fuzzy_set

    ui_name <- paste0(linguistic_variable_name, '-', fuzzy_set_name)
    insertUI(
      selector = paste0('#', session$ns('fuzzy_set_ui_div')),
      ui = fuzzy_set_ui(ui_name = session$ns(ui_name), linguistic_variable_name = linguistic_variable_name, fuzzy_set_name = fuzzy_set_name)
    )
    
    callModule(
      fuzzy_set_server, ui_name,
      main = main, triggers = triggers,
      linguistic_variable_name = linguistic_variable_name,
      fuzzy_set_name = fuzzy_set_name
    )
    
    local_triggers$added_fuzzy_set$trigger()
  })
  

  output$main_plot <- renderPlot({
    local_triggers$added_fuzzy_set$depend()
    lv <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]
    if(length(lv$fuzzy_set_list) > 0){
      x_values <- seq(rng[1], rng[2], length.out = 100)
      plot(x_values, lv$fuzzy_set_list[[1]]$membership_function(x_values), type = 'o', xlim = c(rng[1], rng[2]), ylim = c(0, 1))
      
      if(length(lv$fuzzy_set_list) > 1){
        lapply(2:length(lv$fuzzy_set_list), function(i){
        # for(i in 2:length(lv$fuzzy_set_list)){
          lines(x_values, lv$fuzzy_set_list[[i]]$membership_function(x_values), type = 'o', xlim = c(rng[1], rng[2]))
        })
      }
    }
  })

}