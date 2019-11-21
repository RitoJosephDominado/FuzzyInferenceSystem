
add_linguistic_variable_ui <- function(name){
  ns <- NS(name)
  div(
    box(
      width = 12, title = 'Add Linguistic Variable',
      fluidRow(
        column(2, textInput(ns('linguistic_variable_name_text'), label = 'Name')),
        column(3, numericInput(ns('range_min_numeric'), 'Min', 0, -10000, 10000, 0.1)),
        column(3, numericInput(ns('range_max_numeric'), 'Max', 100, -10000, 10000, 0.1)),
        column(2, br(), actionButton(ns('add_linguistic_variable_btn'), 'Add'))
      ),
      
    ),
    tags$div(id = ns('linguistic_variable_ui_div')),
    p('-')
  )
  
}

add_linguistic_variable_server <- function(input, output, session, main, triggers){
  observeEvent(input$add_linguistic_variable_btn, {
    linguistic_variable_name <- input$linguistic_variable_name_text
    
    if(linguistic_variable_name %in% names(main$fuzzy_inference_system$linguistic_variable_list)){
      shinyalert(
        title = 'Invalid Linguistic Variable Name', 'A linguistic variable with that name has already been added',
        type = 'error', showConfirmButton = TRUE, closeOnClickOutside = TRUE
      )
      
    }else if(grepl('^\\s*$', linguistic_variable_name)){
      shinyalert(
        'Invalid Linguistic Variable Name', 'Can\'t use a linguistic variable name with just whitespace',
        type = 'error', showConfirmButton = TRUE, closeOnClickOutside = TRUE
      )
    }else{
      main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]] <- linguistic_variable(
        name = linguistic_variable_name,
        xlim = c(min = input$range_min_numeric, max = input$range_max_numeric)
      )
      rng <- c(min = input$range_min_numeric, max = input$range_max_numeric)
     
      insertUI(
        selector = paste0('#', session$ns('linguistic_variable_ui_div')),
        ui = linguistic_variable_ui(
          ui_name = session$ns(linguistic_variable_name), 
          main = main,
          linguistic_variable_name = linguistic_variable_name
        )
      )

      callModule(
        linguistic_variable_server, id = linguistic_variable_name,
        main = main, triggers = triggers,
        linguistic_variable_name = linguistic_variable_name, rng = rng
      )
      
      triggers$added_linguistic_variable$trigger()
    }
  })
  
  observe(priority = 5, {
    triggers$uploaded_json$depend()
    shinyjs::runjs('document.getElementById("add_linguistic_variable-linguistic_variable_ui_div").innerHTML = "";')

  })
  
  # Rerendering ui when json is uploaded
  observe(priority = 3, {
    triggers$uploaded_json$depend()
    isolate({
      # shinyjs::runjs('document.getElementById("add_linguistic_variable-linguistic_variable_ui_div").innerHTML = "";')
      
      lapply(main$fuzzy_inference_system$linguistic_variable_list, function(x_linguistic_variable){
        insertUI(
          selector = paste0('#', session$ns('linguistic_variable_ui_div')),
          ui = linguistic_variable_ui(
            ui_name = session$ns(x_linguistic_variable$name),
            main = main,
            linguistic_variable_name = x_linguistic_variable$name
          )
        )

        callModule(
          session = session,
          linguistic_variable_server, id = x_linguistic_variable$name,
          main = main, triggers = triggers,
          linguistic_variable_name = x_linguistic_variable$name, rng = x_linguistic_variable$xlim
        )
      })
      # lapply(seq_along(main$fuzzy_inference_system$linguistic_variable_list), function(i){
      #   insertUI(
      #     selector = paste0('#', session$ns('linguistic_variable_ui_div')),
      #     ui = linguistic_variable_ui(
      #       ui_name = session$ns(i),
      #       main = main,
      #       linguistic_variable_name = main$fuzzy_inference_system$linguistic_variable_list[[i]]$name
      #     )
      #   )
      #   
      #   callModule(
      #     session = session,
      #     linguistic_variable_server, id = i,
      #     main = main, triggers = triggers,
      #     linguistic_variable_name = main$fuzzy_inference_system$linguistic_variable_list[[i]]$name, 
      #     rng = main$fuzzy_inference_system$linguistic_variable_list[[i]]$xlim
      #   )
      # })
    })
  })
}