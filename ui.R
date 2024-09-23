
# in the book it returns a tagList, but we'll see if it matters that we don't do that here
viewPan <- function(id) {
  tabPanel(
    title = "Viewing the Data",
    p("This section is for viewing the data only. You can use the three boxes below to filter for specific values of a certain column, or try typing in the search boxes to text-search."),
    hr(),
    fluidRow(
      column(4,selectInput(NS(id, 'select'), 'Select Cols', choices = '', multiple = TRUE)),
      column(4, selectInput(NS(id, 'filter'), 'Select Filter Col', choices = '')),
      column(4, selectInput(NS(id, 'value'), 'Select Filter Val(s)', choices = '', multiple = TRUE))
    ),
    hr(),
    DTOutput(NS(id, 'table'))
  )
}

# in the book it returns a tagList, but we'll see if it matters that we don't do that here
addPan <- function(id) {
  tabPanel(
    title = "Add some data!",
    p("This section is for adding data - once it's uploaded via .csv, you can edit and tweak things as needed. This also provides an opportunity to batch-edit values. For example, if you upload a .csv file with a list of existing IDs (ones that are already in the database), you can upload columns that you want to update (ex. change Sent from FALSE to TRUE, etc.)."),
    fileInput(NS(id, 'add'), 'upload data to add'),
    "Edit your uploaded data here if anything looks wrong (Ctrl + Enter to save changes, Esc to discard). If columns are missing, edit and reupload the .csv file instead.",
    rHandsontableOutput(NS(id, 'uploaded')),
    hr(),
    "Here is what it will look like in the database (bold rows are new).",
    DTOutput(NS(id, 'submitted')),
    hr(),
    wellPanel(fluidRow(
        column(4, actionButton(NS(id, 'submit'), label = 'Submit changes to the database')),
        column(3, textOutput(NS(id, 'submitResult'))
      )),
      wellPanel(fluidRow(
        column(4, actionButton(NS(id, 'commit'), label = 'Commit changes to the database')),
        column(3, textOutput(NS(id, 'commitResult')))
      ))
    )
  )
}

editPan <- function(id) {
  tabPanel(
    title = "HOT Edit some data",
    p("This section is for targeted editing of specific cells - if you don't have a .csv file and just want to edit a row or two, this is your best bet."),
    hr(),
    # bookmarkButton(),
    fluidRow(
      column(4,selectInput(NS(id, 'select'), 'Select Cols', choices = '', multiple = TRUE)),
      column(4, selectInput(NS(id, 'filter'), 'Select Filter Col', choices = '')),
      column(4, selectInput(NS(id, 'value'), 'Select Filter Val(s)', choices = '', multiple = TRUE))
    ),
    hr(),
    actionButton(NS(id, "write"), label = "Save", icon = NULL),
    actionButton(NS(id, "undo"), label = "Undo", icon = NULL),
    rHandsontableOutput(NS(id, 'table'))
  )
}

# static
utilsPan <- function(id) {
  tabPanel(
    title = "Utilities",
    actionButton(NS(id, "refresh"), "Refresh Database", icon = icon("rotate"))
  )
}

ui <- function() {
  library(shiny)
  library(DT)
  library(tidyverse)
  library(here)
  library(rhandsontable) # 7/31/24, testing to see if edit functionality is better
  source(here("createdb.R"))
  
  fluidPage(
    titlePanel("Testing..."), 
    tabsetPanel(
      viewPan('disp'),
      addPan('add'),
      editPan('edit'),
      utilsPan('util')
    )
)}

