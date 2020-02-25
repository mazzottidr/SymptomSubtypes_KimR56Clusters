##
# Packages
##
#install.packages("rsconnect")
#install.packages(c("shiny", "plotly", "shinydashboard", "shinyjs"))
#install.packages(c("V8"))

#library(rsconnect) 
library(shiny)
library(plotly)
library(shinydashboard)
library(shinyjs)

##
# Directory with Programs
##
programdat <- "programs"

##
# Functions
##
source("programs/functions.R")

##
# UI
##
source("programs/ui.R")

##
# Server
##
source("programs/server.R")

##
# Launch Shiny App
##
shinyApp(ui = ui, server = server)






