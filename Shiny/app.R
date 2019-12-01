# author: Rito Joseph Dominado

# Loading packages ----
library(shiny)
library(shinydashboard)
library(rlang)
library(R6)
library(listviewer)
library(rhandsontable)
library(jsonlite)
library(readr)
library(shinyalert)
library(shinyjs)

# Loading files ----
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
source('negation_fuzzy_proposition_module.R')
source('save_module.R')

setwd('..')
source('Main/membership_functions.R')
source('Main/fuzzy_sets.R')
source('Main/linguistic_variable.R')
source('Main/fuzzy_propositions.R')
source('Main/FuzzyInferenceSystem.R')
source('Main/fuzzy_proposition_environments.R')
source('Main/translate_fuzzy_proposition.R')
source('Main/json_conversion.R')

# Root ui ----
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
    useShinyjs(),
    useShinyalert(),
    tabItems(
      tabItem(tabName = 'upload_tab', upload_ui('upload')),
      tabItem(tabName = 'add_linguistic_variable_tab', add_linguistic_variable_ui('add_linguistic_variable')),
      tabItem(tabName = 'add_fuzzy_rule_tab', add_fuzzy_rule_ui('add_fuzzy_rule')),
      tabItem(tabName = 'evaluation_tab', evaluation_ui('evaluation')),
      tabItem(tabName = 'save_tab', save_ui('save'))
    )
  )
)
# Root server function ----
server <- function(input, output, session) {
  
  # ** reactiveValues manipulated throughout ----
  main <- reactiveValues(
    fuzzy_inference_system = FuzzyInferenceSystem$new(),
    fuzzy_proposition_environment_list = list(),
    fuzzy_proposition_counter = 0,
    consequent_vec = NULL,
    linguistic_variable_counter = 0
  )
  
  uploaded <- reactiveValues(
    fuzzy_inference_system = NULL
  )
  
  # ** reactiveValues for reactive triggers ----
  triggers <- reactiveValues(
    uploaded_json = make_reactive_trigger(),
    update_fuzzy_inference_system = make_reactive_trigger(),
    added_linguistic_variable = make_reactive_trigger()
  )
  
  # ** calling server function for upload module ----
  callModule(
    upload_server, 'upload',
    main = main, triggers = triggers, uploaded = uploaded
  )
  
  # ** calling server function for add linguistic variable module ----
  callModule(
    add_linguistic_variable_server, 'add_linguistic_variable',
    main = main, triggers = triggers
  )
  
  # ** calling server function for add fuzzy rule module ----
  callModule(
    add_fuzzy_rule_server, 'add_fuzzy_rule',
    main = main, triggers = triggers
  )
  
  # ** calling server function for save module ----
  callModule(
    save_server, 'save',
    main = main, triggers = triggers
  )
  
  # ** calling server function for evaluation module ----
  callModule(
    evaluation_server, 'evaluation',
    main = main, triggers = triggers
  )
  
  # ** observer for clicking on sidebar ----
  observeEvent(input$main_sidebar, {
    triggers$update_fuzzy_inference_system$trigger()
    main$fuzzy_inference_system$fuzzy_proposition_list <- map(main$fuzzy_proposition_environment_list, convert_environment_to_fuzzy_proposition)
    names(main$fuzzy_inference_system$fuzzy_proposition_list) <- main$consequent_vec
  })
}

shinyApp(ui, server)
