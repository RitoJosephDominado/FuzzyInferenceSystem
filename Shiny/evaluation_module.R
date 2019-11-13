evaluation_ui <- function(name){
  ns <- NS(name)
  tabBox(
    width = 12,
    tabPanel(
      title = 'Manual',
      numericInput(ns('num_rows_numeric'), 'Number of rows', 1, 0, 1000, 1),
      column(
        width = 6,
        rHandsontableOutput(ns('input_hot'))
      ),
      column(
        width = 6,
        rHandsontableOutput(ns('output_hot'))
      )
    ),
    tabPanel(
      title = 'Upload',
      column(6, fileInput(ns('features_file'), 'Upload (.csv)')),
      column(4, br(), actionButton(ns('upload_features_btn'), 'Upload')),
      column(
        width = 6,
        rHandsontableOutput(ns('uploaded_input_hot'))
      ),
      column(
        width = 6,
        rHandsontableOutput(ns('uploaded_output_hot'))
      )
    ),
    fluidRow(p(''))
  )
}

evaluation_server <- function(input, output, session, main, triggers){
  tables <- reactiveValues(
    input_df = NULL,
    output_df = NULL,
    uploaded_input_df = NULL,
    uploaded_output_df = NULL
  )
  
  tables$input_df <- isolate({
    data.frame(
      matrix(
        0, 
        nrow = 1, 
        ncol = length(main$fuzzy_inference_system$linguistic_variable_list)
      )
    )
  })
  isolate({colnames(tables$input_df) <- names(main$fuzzy_inference_system$linguistic_variable_list)})

  observe({
    triggers$added_linguistic_variable$depend()
    triggers$update_fuzzy_inference_system$depend()
    
    input_df <- data.frame(
      matrix(
        0,
        nrow = input$num_rows_numeric,
        ncol = length(main$fuzzy_inference_system$linguistic_variable_list)
      )
    )
    colnames(input_df) <- names(main$fuzzy_inference_system$linguistic_variable_list)

    tables$input_df <- input_df
  })
  
  
  output$input_hot <- renderRHandsontable({
    triggers$added_linguistic_variable$depend()
    
    if(length(main$fuzzy_inference_system$linguistic_variable_list) == 0) return(NULL)
    
    default_df <- tables$input_df
    rhandsontable(default_df)
  })
  
  
  output$output_hot <- renderRHandsontable({
    triggers$update_fuzzy_inference_system$depend()
    if(length(main$fuzzy_inference_system$fuzzy_proposition_list) == 0) return(NULL)
    req(input$input_hot)
    input_df <- hot_to_r(input$input_hot)
    output_df <- main$fuzzy_inference_system$evaluate_fuzzy_proposition_list(input_df)
    colnames(output_df) <- names(main$fuzzy_inference_system$fuzzy_proposition_list)
    rhandsontable(output_df, readOnly = TRUE)
  })
}