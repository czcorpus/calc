library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("yeti"),
  tags$head(
    tags$script(src="toolbar-adapter.js"),
    tags$link(href="toolbar-adapter.css", rel="stylesheet")
  ),
  uiOutput("toolbarAssets"),
  uiOutput("toolbar"),
  uiOutput("localizedUI")
))