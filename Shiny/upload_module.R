make_reactive_trigger <- function(){
  rv <- reactiveValues(a = 0)
  list(
    depend = function(){
      rv$a
      invisible()
    },
    trigger = function(){
      rv$a <- isolate(rv$a + 1)
    }
  )
}


upload_ui <- function(name){
  ns <- NS(name)
  box(
    width = 12,
    column(6, fileInput(ns('fuzzy_inference_system_json_file'), 'Fuzzy inference system file (.json)')),
    column(3, br(), actionButton(ns('upload_fuzzy_inference_system_json'), 'Upload'))
  )
}

upload_server <- function(input, output, session, main, triggers){
  observeEvent(input$upload_fuzzy_inference_system_json, {
    print('ok ok')
  })
}