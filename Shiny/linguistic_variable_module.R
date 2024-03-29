

linguistic_variable_ui <- function(main, linguistic_variable_ui_name, linguistic_variable_name, linguistic_variable_index){
  ns <- NS(linguistic_variable_ui_name)
  lv <- main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]
  
  div(
    id = ns('linguistic_variable_div'),
    box(
      width = 12,
      title = linguistic_variable_name,
      
      div(
        id = ns('add_fuzzy_set_div'),
        box(
          width = 12, title = 'Details', collapsible = TRUE,
          status = 'primary', solidHeader = TRUE,
          fluidRow(
            div(
              class = 'col-sm-12 col-md-12 col-lg-6',
              box(
                width = 12, title = 'Range', status = 'success', solidHeader = TRUE,
                HTML(paste0(h4('Min: ', lv$xlim[1], HTML('&emsp;'), 'Max: ', lv$xlim[2])))
              ),
              
              box(
                width = 12, title = 'Add Fuzzy Set', status = 'success', solidHeader = TRUE,
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
        div(id = ns('fuzzy_set_ui_div'))
      ),
      column(6, actionButton(ns('delete_btn'), 'Delete'), br())
    )
  )
}

linguistic_variable_server <- function(input, output, session, main, triggers, linguistic_variable_ui_name, linguistic_variable_name, linguistic_variable_index){
  local_triggers <- reactiveValues(
    added_fuzzy_set = make_reactive_trigger(),
    deleted_fuzzy_set = make_reactive_trigger()
  )
  
  fuzzy_set_list <- main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]$fuzzy_set_list
  if(length(fuzzy_set_list) > 0){
    
    lapply(seq_along(fuzzy_set_list), function(i){
      fuzzy_set_name <- names(fuzzy_set_list)[i]
      fuzzy_set_ui_name <- paste0(linguistic_variable_name, '-', fuzzy_set_name)
      insertUI(
        selector = paste0('#', session$ns('fuzzy_set_ui_div')),
        ui = fuzzy_set_ui(
          fuzzy_set_ui_name = session$ns(fuzzy_set_ui_name),
          linguistic_variable_name = linguistic_variable_name,
          linguistic_variable_index = index,
          fuzzy_set_name = fuzzy_set_name
        )
      )
      
      callModule(
        fuzzy_set_server, fuzzy_set_ui_name,
        main = main, triggers = triggers,
        linguistic_variable_name = linguistic_variable_name,
        linguistic_variable_index = linguistic_variable_index,
        fuzzy_set_name = fuzzy_set_name,
        local_triggers = local_triggers
      )
    })
    local_triggers$added_fuzzy_set$trigger()
  }
  
  # fuzzy_set_list <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]$fuzzy_set_list
  # if(length(fuzzy_set_list) > 0){
  #   lapply(seq_along(fuzzy_set_list), function(i){
  #     fuzzy_set_name <- names(fuzzy_set_list)[i]
  #     fuzzy_set_ui_name <- paste0(linguistic_variable_ui_name, '-', fuzzy_set_name)
  #     insertUI(
  #       selector = paste0('#', session$ns('fuzzy_set_ui_div')),
  #       ui = fuzzy_set_ui(
  #         fuzzy_set_ui_name = session$ns(fuzzy_set_ui_name),
  #         linguistic_variable_name = linguistic_variable_name,
  #         linguistic_variable_index = index,
  #         fuzzy_set_name = fuzzy_set_name
  #       )
  #     )
  # 
  #     callModule(
  #       fuzzy_set_server, ui_name,
  #       main = main, triggers = triggers,
  #       linguistic_variable_name = linguistic_variable_name,
  #       linguistic_variable_index = index,
  #       fuzzy_set_name = fuzzy_set_name
  #     )
  # 
  #   })
  # }
  
  # Rendering the box for entering fuzzy set parameters ----
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
  
  # Observer for adding fuzzy sets. Assigning it to a variable to destroy it later
  add_fuzzy_set_observer <- observeEvent(input$add_fuzzy_set_btn, ignoreInit = TRUE, {
    fuzzy_set_name <- input$fuzzy_set_name_text
    
    if(fuzzy_set_name %in% names(main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]$fuzzy_set_list)){
      shinyalert(
        'Invalid Fuzzy Set Name',
        paste0('A fuzzy set with the name "', fuzzy_set_name, '" has already been added to ', linguistic_variable_name),
        type = 'error', showConfirmButton = TRUE, closeOnClickOutside = TRUE
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
    
    main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]$fuzzy_set_list[[fuzzy_set_name]] <- fuzzy_set
    
    fuzzy_set_ui_name <- paste0(linguistic_variable_name, '-', fuzzy_set_name)
    
    insertUI(
      selector = paste0('#', session$ns('fuzzy_set_ui_div')),
      ui = fuzzy_set_ui(
        fuzzy_set_ui_name = session$ns(fuzzy_set_ui_name),
        linguistic_variable_name = linguistic_variable_name,
        linguistic_variable_index = linguistic_variable_index,
        fuzzy_set_name = fuzzy_set_name
      )
    )
    
    callModule(
      module = fuzzy_set_server, id = fuzzy_set_ui_name,
      main = main, triggers = triggers,
      fuzzy_set_ui_name = fuzzy_set_ui_name,
      linguistic_variable_name = linguistic_variable_name,
      linguistic_variable_index = linguistic_variable_index,
      fuzzy_set_name = fuzzy_set_name,
      local_triggers = local_triggers
    )
    
    local_triggers$added_fuzzy_set$trigger()
  })
  
  output$main_plot <- renderPlot({
    local_triggers$added_fuzzy_set$depend()
    local_triggers$deleted_fuzzy_set$depend()
    lv <- main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]
    
    if(length(lv$fuzzy_set_list) > 0){
      x_values <- seq(lv$xlim[1], lv$xlim[2], length.out = 100)
      plot(x_values, lv$fuzzy_set_list[[1]]$membership_function(x_values), type = 'o', xlim = c(lv$xlim[1], lv$xlim[2]), ylim = c(0, 1))
      
      if(length(lv$fuzzy_set_list) > 1){
        lapply(2:length(lv$fuzzy_set_list), function(i){
          lines(x_values, lv$fuzzy_set_list[[i]]$membership_function(x_values), type = 'o', xlim = c(lv$xlim[1], lv$xlim[2]))
        })
      }
    }
  })
  
  observeEvent(input$delete_btn, ignoreInit = TRUE, {
    add_fuzzy_set_observer$destroy()
    removeUI(selector = paste0('#', session$ns('add_fuzzy_set_div')))
    removeUI(selector = paste0('#', session$ns('linguistic_variable_div')))
    
    main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]] <- NULL
    triggers$update_fuzzy_inference_system$trigger()
  })
}