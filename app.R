library(shiny)
library(DT)
library(tidyverse)
library(here)
library(rhandsontable) # 7/31/24, testing to see if edit functionality is better
source(here("createdb.R"))

# for deployment
shinyApp(
  ui = ui, server = server
)






