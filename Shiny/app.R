library(shiny)
library(shinydashboard)
library(rlang)
library(R6)
library(listviewer)

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


source('upload_module.R')
source('add_linguistic_variable_module.R')
source('add_fuzzy_rule_module.R')
source('evaluation_module.R')
source('linguistic_variable_module.R')
source('fuzzy_set_module.R') 

source('linguistic_variable_module.R')

source('fuzzy_rule_module.R')

source('simple_fuzzy_proposition_module.R')
source('compound_fuzzy_proposition_module.R')

source('save_module.R')

setwd('..')

source('Main/membership_functions.R')
source('Main/fuzzy_sets.R')
source('Main/linguistic_variable.R')
source('Main/fuzzy_propositions.R')
source('Main/FuzzyInferenceSystem.R')




ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
    sidebarMenu(
      id = 'main_sidebar',
      menuItem('Upload', tabName = 'upload_tab', icon = icon('upload')),
      menuItem('Linguistic variables', tabName = 'add_linguistic_variable_tab', icon = icon('chart-line')),
      menuItem('Fuzzy rules', tabName = 'add_fuzzy_rule_tab', icon = icon('edit')),
      menuItem('Evaluation', tabName = 'evaluation_tab', icon = icon('table')),
      menuItem('Save', tabName = 'save_tab', icon = icon('save'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'upload_tab', upload_ui('upload')),
      tabItem(tabName = 'add_linguistic_variable_tab', add_linguistic_variable_ui('add_linguistic_variable')),
      tabItem(tabName = 'add_fuzzy_rule_tab', add_fuzzy_rule_ui('add_fuzzy_rule')),
      tabItem(tabName = 'evaulation_tab', evaluation_ui('evaluation')),
      tabItem(tabName = 'save_tab', save_ui('save'))
    )
  )
)

server <- function(input, output, session) {
  main <- reactiveValues(
    fuzzy_inference_system = FuzzyInferenceSystem$new()
  )
  
  triggers <- reactiveValues(
    uploaded_json = make_reactive_trigger(),
    update_fuzzy_inference_system = make_reactive_trigger(),
    added_linguistic_variable = make_reactive_trigger()
  )
  
  plot_variables <- reactiveValues(
    linguistic_variable_range_list = list()
  )
  
  callModule(
    upload_server, 'upload',
    main = main, triggers = triggers
  )
  
  callModule(
    add_linguistic_variable_server, 'add_linguistic_variable',
    main = main, triggers = triggers,
    plot_variables = plot_variables
  )
  
  callModule(
    add_fuzzy_rule_server, 'add_fuzzy_rule',
    main = main, triggers = triggers
    # plot_variables = plot_variables
  )
  
  callModule(
    save_server, 'save',
    main = main, triggers = triggers
  )
  
  observeEvent(input$main_sidebar, {
    triggers$update_fuzzy_inference_system$trigger()
  })
}

shinyApp(ui, server)