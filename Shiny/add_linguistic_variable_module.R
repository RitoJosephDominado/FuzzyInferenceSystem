
add_linguistic_variable_ui <- function(name){
  ns <- NS(name)
  box(
    width = 12, title = 'Add linguistic variable',
    fluidRow(
      column(2, textInput(ns('linguistic_variable_name_text'), label = 'Name')),
      column(3, numericInput(ns('range_min_numeric'), 'Min', 0, -10000, 10000, 0.1)),
      column(3, numericInput(ns('range_max_numeric'), 'Max', 100, -10000, 10000, 0.1)),
      column(2, br(), actionButton(ns('add_linguistic_variable_btn'), 'Add'))
    ),
    tags$div(id = ns('linguistic_variable_ui_div'))
    
  )
}

add_linguistic_variable_server <- function(input, output, session, main, triggers, plot_variables){
  observeEvent(input$add_linguistic_variable_btn, {
    linguistic_variable_name <- input$linguistic_variable_name_text
    
    if(linguistic_variable_name %in% names(main$fuzzy_inference_system$linguistic_variable_list)){
      showModal(
        modalDialog(
          title = 'Invalid Linguistic Variable Name',
          p('A linguistic variable with that name has already been added')
        )
      )
    }else if(grepl('^\\s*$', linguistic_variable_name)){
      showModal(
        modalDialog(
          title = 'Invalid Linguistic Variable Name',
          p('Cannot enter a linguistic variable name with just whitespace')
        )
      )
    }else{
      main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]] <- linguistic_variable(
        name = linguistic_variable_name,
        xlim = c(min = input$range_min_numeric, max = input$range_max_numeric)
      )
      rng <- c(min = input$range_min_numeric, max = input$range_max_numeric)
      plot_variables[[linguistic_variable_name]] <- rng
      
      insertUI(
        selector = paste0('#', session$ns('linguistic_variable_ui_div')),
        ui = linguistic_variable_ui(ui_name = session$ns(linguistic_variable_name), linguistic_variable_name = linguistic_variable_name)
      )
      
      callModule(
        linguistic_variable_server, id = linguistic_variable_name,
        main = main, triggers = triggers,
        linguistic_variable_name = linguistic_variable_name, rng = rng
      )
      triggers$added_linguistic_variable$trigger()
    }
  })
  
  
  # Rerendering ui when json is uploaded
  observe({
    triggers$uploaded_json$depend()
    lapply(main$fuzzy_inference_system$linguistic_variable_list, function(x_linguistic_variable){
    # for(x_linguistic_variable in main$fuzzy_inference_system$linguistic_variable_list){
      insertUI(
        selector = paste0('#', session$ns('linguistic_variable_ui_div')),
        ui = linguistic_variable_ui(
          ui_name = session$ns(x_linguistic_variable$name),
          linguistic_variable_name = x_linguistic_variable$name
        )
      )
      
      callModule(
        linguistic_variable_server, id = x_linguistic_variable$name,
        main = main, triggers = triggers,
        linguistic_variable_name = x_linguistic_variable$name, rng = x_linguistic_variable$xlim
      )
      
      
    })
    
  })
}