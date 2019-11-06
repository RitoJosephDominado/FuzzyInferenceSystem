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
  h3('uploaddd')
}

upload_server <- function(input, output, session, main, triggers){
  
}