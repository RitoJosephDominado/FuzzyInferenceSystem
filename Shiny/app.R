library(shiny)
library(shinydashboard)
library(R6)

source('upload_module.R')
source('add_linguistic_variable_module.R')
source('add_fuzzy_rule_module.R')
source('evaluation_module.R')

source('linguistic_variable_module.R')

setwd('..')

source('Main/membership_functions.R')
source('Main/linguistic_variable.R')
source('Main/fuzzy_propositions.R')
source('Main/FuzzyInferenceSystem.R')



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

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Upload', tabName = 'upload_tab', icon = icon('upload')),
      menuItem('Linguistic variables', tabName = 'add_linguistic_variable_tab', icon = icon('chart-line')),
      menuItem('Fuzzy rules', tabName = 'add_fuzzy_rule_tab', icon = icon('edit')),
      menuItem('Evaluation', tabName = 'evaluation_tab', icon = icon('table'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'upload_tab', upload_ui('upload')),
      tabItem(tabName = 'add_linguistic_variable_tab', add_linguistic_variable_ui('add_linguistic_variable')),
      tabItem(tabName = 'add_fuzzy_rule_tab', add_fuzzy_rule_ui('add_fuzzy_rule')),
      tabItem(tabName = 'evaulation_tab', evaluation_ui('evaluation'))
    )
  )
)

server <- function(input, output, session) {
  main <- reactiveValues(
    fuzzy_inference_system = FuzzyInferenceSystem$new()
  )
  
  triggers <- reactiveValues(
    uploaded_json = make_reactive_trigger()
  )
  
  callModule(
    upload_server, 'upload',
    main = main, triggers = triggers
  )
  
  callModule(
    add_linguistic_variable_server, 'add_linguistic_variable',
    main = main, triggers = triggers
  )
}

shinyApp(ui, server)