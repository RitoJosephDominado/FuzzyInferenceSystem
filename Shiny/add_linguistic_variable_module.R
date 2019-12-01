# Module for adding linguistic variables. If you're making a FuzzyInferenceSystem from scratch, craeting linguistic variables is
# the first step. 


add_linguistic_variable_ui <- function(name){
  ns <- NS(name)
  div(
    # ** Box for adding new linguistic variables ----
    box(
      width = 12, title = 'Add Linguistic Variable',
      fluidRow(
        column(2, textInput(ns('linguistic_variable_name_text'), label = 'Name')),
        column(3, numericInput(ns('range_min_numeric'), 'Min', 0, -10000, 10000, 0.1)),
        column(3, numericInput(ns('range_max_numeric'), 'Max', 100, -10000, 10000, 0.1)),
        column(2, br(), actionButton(ns('add_linguistic_variable_btn'), 'Add'))
      )
    ),
    # ** div where linguistic variable ui's get put in ----
    div(id = ns('linguistic_variable_ui_div')),
    
    p('-')
  )
}

add_linguistic_variable_server <- function(input, output, session, main, triggers){
  # ** Observer for adding linguistic variables ----
  observeEvent(input$add_linguistic_variable_btn, {
    linguistic_variable_name <- input$linguistic_variable_name_text
    
    # ** ** Creating the a ui name for the new linguistic variable ----
    # The app uses temporary ui names instead of the actual names of the linguistic variables as names for the lists
    # This is done so that the names will be unique identifiers, and no ui-to-server confusion occurs when files are uploaded
    # with linguistic variables who use repeated names
    main$linguistic_variable_counter <- main$linguistic_variable_counter + 1
    linguistic_variable_ui_name <- paste0('linguistic_variable_', main$linguistic_variable_counter)
    
    # ** ** Adding the new linguistic variable into the main FIS model ----
    main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_ui_name]] <- linguistic_variable(
      name = linguistic_variable_name,
      xlim = c(min = input$range_min_numeric, max = input$range_max_numeric)
    )
    
    # ** ** Calling the linguistic variable ui and server functions ----
    insertUI(
      selector = paste0('#', session$ns('linguistic_variable_ui_div')),
      ui = linguistic_variable_ui(
        linguistic_variable_ui_name = session$ns(linguistic_variable_ui_name),
        linguistic_variable_name = linguistic_variable_name,
        linguistic_variable_index = main$linguistic_variable_counter,
        main = main
      )
    )
    
    callModule(
      linguistic_variable_server, id = linguistic_variable_ui_name,
      main = main, triggers = triggers,
      linguistic_variable_ui_name = linguistic_variable_ui_name,
      linguistic_variable_name = linguistic_variable_name,
      linguistic_variable_index = main$linguistic_variable_counter
    )
    
    triggers$added_linguistic_variable$trigger()
  })
  
  # ** ** Observer for the uploaded json trigger ----
  # This trigger is activated by the button in the upload module, which allows you to upload json files
  observe({
    triggers$uploaded_json$depend()
    isolate({
      shinyjs::runjs('document.getElementById("add_linguistic_variable-linguistic_variable_ui_div").innerHTML = "";')
      
      lapply(names(main$fuzzy_inference_system$linguistic_variable_list), function(linguistic_variable_ui_name){
        # ** ** Creating the ui name for a linguistic variable ----
        x_linguistic_variable <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_ui_name]]
        linguistic_variable_index <- substr(linguistic_variable_ui_name, 21, nchar(linguistic_variable_ui_name)) %>% as.integer()
        linguistic_variable_ui_name <- paste0('linguistic_variable_', linguistic_variable_index)
        
        # ** ** Inserting new linguistic variable ui and calling its server function ----
        insertUI(
          selector = paste0('#', session$ns('linguistic_variable_ui_div')),
          ui = linguistic_variable_ui(
            main = main,
            linguistic_variable_ui_name = session$ns(linguistic_variable_ui_name),
            linguistic_variable_name = x_linguistic_variable$name,
            linguistic_variable_index = linguistic_variable_index
          )
        )
        
        callModule(
          module = linguistic_variable_server,
          id = linguistic_variable_ui_name,
          main = main, triggers = triggers,
          linguistic_variable_ui_name = linguistic_variable_ui_name,
          linguistic_variable_name = x_linguistic_variable$name,
          linguistic_variable_index = linguistic_variable_index
        )
      })
    })
  })
}