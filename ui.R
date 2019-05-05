library(shiny)
library(shinythemes)
library(shinyCNC)

shinyUI(
  bootstrapPage(
    tags$head( tags$link(rel = "stylesheet", type = "text/css", href = "calc.css") ),
    shiny_cnc_UI(),
    #shinythemes::themeSelector(),
    theme = shinytheme("yeti"),
    uiOutput("localizedUI")
))