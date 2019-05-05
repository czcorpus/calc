library(shiny)
library(shinythemes)
library(shinyCNC)

shinyUI(
  bootstrapPage(
    shiny_cnc_UI(),
    #shinythemes::themeSelector(),
    theme = shinytheme("yeti"),
    uiOutput("localizedUI")
))