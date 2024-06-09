#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# https://yihui.shinyapps.io/DT-edit/
# https://shiny.posit.co/r/gallery/widgets/datatables-options/ 

library(shiny)
library(DT)
library(tidyverse)

display_pan <- tabPanel(
  title = "Viewing the Data",
  p("Here we will be testing useful ways of displaying data. Eventually we will work in databases as well."),
  wellPanel(
    selectInput('otherselection', 'Select cols', choices = '', multiple = TRUE),
  ),
  wellPanel(
    "Use this box to see unique vals of a certain column (helpful for searching)",
    selectInput('uniquecol', label = "", choices = ""),
    textOutput('options', inline = TRUE)
  ),
  hr(),
  DTOutput('wholedf')
)

edit_pan <- tabPanel(
  title = "Add some data!",
  fileInput('add', 'upload data to add'),
  "Edit your uploaded data here if anything looks wrong. If columns are missing, recommend editing the .csv file instead.",
  DTOutput('uploaded'),
  hr(),
  "Here is what it will look like in the database",
  DTOutput('submitted'),
  hr(),
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
  
  custom_datatable <- function(data, ...) {
    datatable(
      data, 
      editable = "row", rownames = FALSE, selection = 'none', ..., 
      # filter = "top" is useful for big tables
      options = list(
        searching = TRUE, 
        dom = "tlip"
        #layout = list(topEnd = NULL)
      )
    )
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
  data <- tbl(con, "data")
  data_rct <- reactiveVal(data)

  # when using database tbls we have to do colnames (dplyr)
  updateSelectInput(session, 'otherselection', choices = colnames(data), selected = colnames(data))
  updateSelectInput(session, 'uniquecol', choices = colnames(data), selected = NA)
  updateSelectInput(session, 'filtercol', choices = colnames(data), selected = NA)
  
  # showing some values of the selected column
  output$options <- renderText({
    selection <- input$uniquecol
    if (selection == "") "" 
    else data %>% pull(input$uniquecol) %>% unique() %>% .[1:10] %>% paste0(collapse = ", ")
  })
  
  output$wholedf <- renderDT({
    selectdata <- data_rct() %>% select(input$otherselection) %>% collect %>% custom_datatable(filter = "top")
  })
  
  # want to simulate an added row of data
  to_add <- tibble(name = "Billy Bob", height = 2001, mass = 600, birth_year = 23, species = "Hillbilly")
  # this will have to be reactive based on the csv upload I think
  to_add_rct <- reactiveVal(to_add)
  
  # updating the to_add data based on edit actions (Ctrl + Enter)
  # not super useful, but cool
  observeEvent(input$uploaded_cell_edit, {
    editData(to_add_rct(), input$uploaded_cell_edit, rownames = FALSE) %>% 
      to_add_rct()
  })
  
  output$uploaded <- renderDT(to_add_rct() %>% datatable(options = list(dom = "t")))
  
  # combining sample of existing data then
  # making a hidden marker column to bold the new stuff
  output$submitted <- renderDT(
    data %>% head %>% 
      collect %>% 
      bind_rows(
        to_add_rct() %>% mutate(added = TRUE) %>% relocate(added), 
        .
      ) %>% 
      datatable(
        ., 
        editable = "row", rownames = FALSE, selection = 'none',
        # searching option affects all filters
        options = list(
          searching = TRUE, 
          dom = "tlp",
          rowCallback = JS(
            "function(row, data) {",
            "  if (data[0]) { $(row).css('font-weight', 'bold') };",
            "}"
          ),
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      )
  )
  
  # then the submit button will do rows_insert on the database using the reactive to_add data
  # this works!!!
  # It's also not persistent through refreshes of the app
  # and maybe we can come up with some kind of iterable table
  # also we can always add another editable table section and use rows_update
  # I think the next thing will be to add a counter that shows how many rows we've added
  observeEvent(input$submitButton, {
    # I think I am misunderstanding what in_place means
   data_rct() %>% 
      rows_insert(
        to_add_rct(), conflict = "ignore", copy = TRUE, in_place = FALSE
      ) %>% 
      data_rct()
  })
  
}

shinyApp(ui = ui, server = server)







