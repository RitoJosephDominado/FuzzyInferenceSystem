evaluation_ui <- function(name){
  ns <- NS(name)
  tabBox(
    width = 12,
    tabPanel(
      title = 'Manual',
      numericInput(ns('num_rows_numeric'), 'Number of rows', 1, 0, 1000, 1),
      
      box(
        width = 6, title = 'Inputs', status = 'primary', solidHeader = TRUE,
        rHandsontableOutput(ns('input_hot')),
        br(),
        fluidRow(
          column(6, textInput(ns('input_df_filename_text'), 'Filename')),
          column(6, br(), actionButton(ns('save_input_df_btn'), 'Save'))
        )
      ),
      box(
        width = 6, title = 'Ouputs', status = 'primary', solidHeader = TRUE,
        rHandsontableOutput(ns('output_hot')),
        br(),
        fluidRow(
          column(6, textInput(ns('output_df_filename_text'), 'Filename')),
          column(6, br(), actionButton(ns('save_output_df_btn'), 'Save'))
        )
      ),
      p('-')
    ),
    tabPanel(
      title = 'Upload',
      fluidRow(
        column(6, fileInput(ns('features_file'), 'Upload (.csv)', width = '100%')),
        column(6, br(), actionButton(ns('upload_features_btn'), 'Upload')),
      ),
      
      fluidRow(
        box(
          width = 6, title = 'Inputs',status = 'primary', solidHeader = TRUE,
          rHandsontableOutput(ns('uploaded_input_hot'))
        ),
        box(
          width = 6, title = 'Outputs', status = 'primary', solidHeader = TRUE,
          rHandsontableOutput(ns('uploaded_output_hot'))
        )
      )
    ),
    p('')
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
    print('----------')
    print(input_df)
    output_df <- main$fuzzy_inference_system$evaluate_fuzzy_proposition_list(input_df)
    colnames(output_df) <- unname(main$consequent_vec)
    rhandsontable(output_df, readOnly = TRUE)
  })
  
  
  observeEvent(input$save_input_df_btn, {
    filename <- input$input_df_filename_text
    
    if(grepl('^\\s*$', filename)){
      shinyalert(
        'Invalid Filename',
        'Can\'t use a filename with just whitespace',
        type = 'error', showConfirmButton = TRUE, closeOnClickOutside = TRUE
      )
      return(NULL)
    }else{
      input_df <- input$input_hot %>% hot_to_r
      write_csv(input_df, path = paste0('Data/', filename, '.csv'))
      shinyalert(
        'Saved', paste0('Successfully saved ', filename, '.csv'),
        type = 'success'
      )
    }
  })
  
  observeEvent(input$save_output_df_btn, {
    filename <- input$output_df_filename_text
    
    if(grepl('^\\s*$', filename)){
      shinyalert(
        'Invalid Filename',
        'Can\'t use a filename with just whitespace',
        type = 'error', showConfirmButton = TRUE, closeOnClickOutside = TRUE
      )
      return(NULL)
    }else{
      output_df <- input$output_hot %>% hot_to_r
      write_csv(output_df, path = paste0('Outputs/', filename, '.csv'))
      shinyalert(
        'Saved', paste0('Successfully saved ', filename, '.csv'),
        type = 'success'
      )
    }
  })
  
  observeEvent(input$upload_features_btn, {
    tables$uploaded_input_df <- read_csv(input$features_file$datapath)
    
  })
  
  output$uploaded_input_hot <- renderRHandsontable({
    if(is.null(tables$uploaded_input_df)) return(NULL)
    # req()
    rhandsontable(tables$uploaded_input_df)
  })
  
  output$uploaded_output_hot <- renderRHandsontable({
    if(length(main$fuzzy_inference_system$fuzzy_proposition_list) == 0) return(NULL)
    req(input$uploaded_input_hot)
    input_df <- hot_to_r(input$uploaded_input_hot)
    output_df <- main$fuzzy_inference_system$evaluate_fuzzy_proposition_list(input_df)
    colnames(output_df) <- unname(main$consequent_vec)
    rhandsontable(output_df, readOnly = TRUE)
    
    rhandsontable(output_df)
  })
}