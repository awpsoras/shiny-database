#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tidyverse)

display_pan <- tabPanel(
  title = "Viewing the Data",
  p("Here we will be testing useful ways of displaying data. Eventually we will work in databases as well."),
  
  wellPanel(
    selectInput('otherselection', 'Select cols', choices = '', multiple = TRUE),
  ),
  column(
    6,
    wellPanel(
      "Use this box to see unique vals of a certain column (helpful for searching)",
      selectInput('uniquecol', label = "", choices = ""),
      textOutput('options')
    )
  ),
  column (
    6,
    wellPanel(
      "Use this box to filter based on a specified column (more specific than the search box)",
      selectInput('filtercol', label = "Column to filter", choices = ''),
      textInput('filterval', label = "Value to search for")
    )
  ),
  
  br(),
  
  #wellPanel(DTOutput('df')),
  DTOutput('wholedf')
)

edit_pan <- tabPanel(
  title = "Add some data!",
  fileInput('add', 'upload data to add (simulating within code for now'),
  hr(),
  "Here is what it will look like in the database",
  DTOutput('submitted'),
  hr(),
  p("Edit your uploaded data here if anything looks wrong. Hopefully column name assignment is going to be able to be done as well."),
  DTOutput('uploaded'),
  actionButton('submitButton', label = 'Submit changes to the database')
)

ui <- fluidPage(
  titlePanel("Testing..."),
  tabsetPanel(
    display_pan,
    edit_pan
  )
)

server <- function(input, output, session) {

  con <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
  
  data <- tbl(con, "data")

  
  # when using database tbls we have to do colnames (dplyr)
  # datatables cannot display a tbl unfortunately
  # updating the options based on the current data
  updateSelectInput(session, 'otherselection', choices = colnames(data), selected = colnames(data))
  updateSelectInput(session, 'uniquecol', choices = colnames(data), selected = NA)
  updateSelectInput(session, 'filtercol', choices = colnames(data), selected = NA)
  
  # testing data as reactive tbl ------------------------------------------------
  # showing some values of the selected column
  output$options <- renderText({
    selection <- input$uniquecol
    if (selection == "") "" 
    else data %>% pull(input$uniquecol) %>% unique() %>% .[1:10] %>% paste0(collapse = ", ")
  })
  
  # updating the data based on the cols selected and potential filters
  wholedata_rct <- reactive({
    selectdata <- data %>% select(input$otherselection) %>% collect()
    if (input$filtercol == "" | input$filterval == "") selectdata
    else selectdata %>% filter(str_detect(!!sym(input$filtercol), input$filterval))
  })
  
  # showing the data
  output$wholedf <- renderDT(datatable(wholedata_rct()))
  
  # want to simulate an added row of data
  # we're going to use a regular tibble for this,
  # but really we'd be reading a csv
  to_add <- tibble(
    name = "Billy Bob",
    height = 2001,
    mass = 11,
    sex = "sure",
    species = "hill billy bob"
  )
  
  # https://yihui.shinyapps.io/DT-edit/
  
  # maybe it's because it's not a reactive value?
  # we'll try that
  to_add_rct <- reactiveVal(to_add)
  # updating the to_add data based on edit actions (Ctrl + Enter)
  observeEvent(input$uploaded_cell_edit, {
    editData(to_add_rct(), input$uploaded_cell_edit, rownames = FALSE) %>% 
      to_add_rct()
  })
  
  # Again, showing the reactive data
  output$uploaded <- renderDT(
    datatable(to_add_rct(), editable = "row", rownames = FALSE, selection = 'none', options = list(searching = FALSE))
  )
  
  # So now I guess we have to try to add the row to the table
  # and show what the added result would look like
  # putting the added data first ensures new is at the top
  output$submitted <- renderDT(
    datatable(
      data %>% 
        head() %>% collect() %>% bind_rows(to_add_rct(), .)
    )
  )
  
  # then the submit button will do rows_insert on the database using the reactive to_add data
  # so this does update the value of data,
  # but for some reason the view data tab doesn't update unless I change the select or filter vals
  # So I guess whole data is the problem?
  # I feel like we need to fix the whole data paradigm
  # and make it independent of the select / filter thing
  observeEvent(input$submitButton, {
    # I think I am misunderstanding what in_place means
    data <<- data %>% 
      rows_insert(
        to_add_rct(), conflict = "ignore", copy = TRUE, in_place = FALSE
      )
  })
  
}

shinyApp(ui = ui, server = server)







