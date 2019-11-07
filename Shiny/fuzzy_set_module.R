
fuzzy_set_ui <- function(ui_name, linguistic_variable_name, fuzzy_set_name){
  ns <- NS(ui_name)
  box(
    width = 6, title = paste(linguistic_variable_name, ':', fuzzy_set_name),
    collapsible = TRUE, collapsed = TRUE,
    reactjsonOutput(ns('model_reactjson'))
  )
}

fuzzy_set_server <- function(input, output, session, main, triggers, linguistic_variable_name, fuzzy_set_name){
  output$model_reactjson <- renderReactjson({
    fuzzy_set <- main$fuzzy_inference_system$linguistic_variable_list[[linguistic_variable_name]][[fuzzy_set_name]]
    reactjson(fuzzy_set[!names(fuzzy_set) %in% 'membership_function'])
  })
}
