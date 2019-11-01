
add_linguistic_variable_ui <- function(name){
  ns <- NS(name)
  box(
    width = 12, title = 'Add linguistic variable',
    fluidRow(
      column(2, textInput(ns('linguistic_variable_name_text'), label = 'Name')),
      column(2, br(), actionButton(ns('add_linguistic_variable_btn'), 'Add'))
    ),
    tags$div(id = ns('linguistic_variable_ui_div'))
    
  )
}

add_linguistic_variable_server <- function(input, output, session, main, triggers){
  observeEvent(input$add_linguistic_variable_btn, {
    insertUI(
      selector = paste0('#',session$ns('linguistic_variable_ui_div')),
      ui = h2('LV')
    )
    print('yep')
  })
}