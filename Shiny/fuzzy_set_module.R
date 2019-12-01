fuzzy_set_ui <- function(fuzzy_set_ui_name, linguistic_variable_name, linguistic_variable_index, fuzzy_set_name, main){
  ns <- NS(fuzzy_set_ui_name)
  
  div(
    id = ns('fuzzy_set_div'),
    box(
      width = 6, title = paste(linguistic_variable_name, ':', fuzzy_set_name),
      collapsible = TRUE, collapsed = TRUE,
      br(),
      reactjsonOutput(ns('main_reactjson')),
      actionButton(ns('delete_btn'), 'Delete')
    )
  )
}


fuzzy_set_server <- function(input, output, session, main, triggers, fuzzy_set_ui_name, linguistic_variable_name, linguistic_variable_index, fuzzy_set_name, local_triggers){
  output$main_reactjson <- renderReactjson({
    fuzzy_set <- main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]$fuzzy_set_list[[fuzzy_set_name]]
    reactjson(fuzzy_set[!names(fuzzy_set) %in% 'membership_function'])
  })
  
  observeEvent(input$delete_btn, ignoreInit = TRUE, {
    main$fuzzy_inference_system$linguistic_variable_list[[paste0('linguistic_variable_', linguistic_variable_index)]]$fuzzy_set_list[[fuzzy_set_name]] <- NULL
    removeUI(
      selector = paste0('#', session$ns('fuzzy_set_div'))
    )
    local_triggers$deleted_fuzzy_set$trigger()
  })
}