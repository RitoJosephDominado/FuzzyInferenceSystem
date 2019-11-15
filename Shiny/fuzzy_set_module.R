
fuzzy_set_ui <- function(ui_name, linguistic_variable_name, fuzzy_set_name){
  ns <- NS(ui_name)
  tags$div(
    id = ns('fuzzy_set_div'),
    box(
      width = 6, title = paste(linguistic_variable_name, ':', fuzzy_set_name),
      collapsible = TRUE, collapsed = TRUE,
      fluidRow(column(
        12, actionButton(ns('delete_btn'), 'Delete')
      )),
      br(),
      reactjsonOutput(ns('model_reactjson'))
    )
  )
}

fuzzy_set_server <- function(input, output, session, main, triggers, linguistic_variable_name, fuzzy_set_name, local_triggers){
  observeEvent(input$delete_btn, ignoreInit = TRUE, {
    main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]$fuzzy_set_list[[fuzzy_set_name]] <- NULL
    removeUI(
      selector = paste0('#', session$ns('fuzzy_set_div'))
    )
    local_triggers$deleted_fuzzy_set$trigger()
  })
  
  output$model_reactjson <- renderReactjson({
    fuzzy_set <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]]$fuzzy_set_list[[fuzzy_set_name]]
    reactjson(fuzzy_set[!names(fuzzy_set) %in% 'membership_function'])
  })
}
