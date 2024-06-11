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
library(here)

display_pan <- tabPanel(
  title = "Viewing the Data",
  p("Here we will be testing useful ways of displaying data. Eventually we will work in databases as well."),
  fluidRow(
    column(
      6,
      wellPanel(
        "Use this box to select which columns are visible.",
        selectInput('selection', '', choices = '', multiple = TRUE))
    ),
    column(
      6,
      wellPanel(
        "Use this box to see unique vals of a certain column (helpful for searching).",
        selectInput('uniquecol', label = "", choices = ""),
        textOutput('options', inline = TRUE)
      )
    )
  )
  ,
  hr(),
  DTOutput('wholedf')
)

add_pan <- tabPanel(
  title = "Add some data!",
  fileInput('add', 'upload data to add'),
  "Edit your uploaded data here if anything looks wrong (Ctrl + Enter to save changes, Esc to discard). If columns are missing, recommend editing the .csv file instead.",
  DTOutput('uploaded'),
  hr(),
  "Here is what it will look like in the database (bold rows are new).",
  DTOutput('submitted'),
  hr(),
  wellPanel(fluidRow(
    column(4, actionButton('submitAdd', label = 'Submit changes to the database')),
    column(3, textOutput('submitResult'))
  )),
  wellPanel(fluidRow(
    column(4, actionButton('commitAdd', label = 'Commit changes to the database')),
    column(3, textOutput('commitResult'))
  ))
)


edit_pan <- tabPanel(
  title = "Edit some data - be careful!",
  "I guess we can't reuse the existing DTOutput for everything since that one is not editable as far as I can tell.",
  "But I suppose we can reuse the select functionality. Maybe we need to have some more advanced filtering.",
  hr(),
  fluidRow(
    column(4,selectInput('editSelection', 'Select Cols', choices = '', multiple = TRUE)),
    column(4, selectInput('filterSelection', 'Select Filter Col', choices = '')),
    column(4, selectInput('valueSelection', 'Select Filter Val(s)', choices = '', multiple = TRUE))
  ),
  hr(),
  DTOutput('editTable')
)

ui <- fluidPage(
  titlePanel("Testing..."), 
  tabsetPanel(
    display_pan,
    add_pan,
    edit_pan
  )
)

server <- function(input, output, session) {
  
  # overall, data_rct() is a reactve, lazy tbl reference
  # to_add_rct() is an in-memory tibble, actual data
  
  # custom_datatable <- function(data, ...) {
  #   datatable(
  #     data, 
  #     editable = "row", rownames = FALSE, selection = 'none', ..., 
  #     # filter = "top" is useful for big tables
  #     options = list(
  #       searching = TRUE, 
  #       dom = "tlip"
  #       #layout = list(topEnd = NULL)
  #     )
  #   )
  # }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
  data_rct <- tbl(con, "data") %>% reactiveVal()
  
  # view section -------------------------------------------------------------------------------------------------------------

  # when using database tbls we have to do colnames (dplyr)
  # since we are using reactive here, everything here has to be in an observe
  # these are independent I think ?
  observe(label = "View Select/Unique",{
    updateSelectInput(session, 'selection', choices = colnames(data_rct()), selected = colnames(data_rct()))
    updateSelectInput(session, 'uniquecol', choices = colnames(data_rct()), selected = NA)
  })
  
  # showing some values of the selected column
  output$options <- renderText({
    selection <- input$uniquecol
    if (selection == "") "" 
    else data_rct() %>% select(selection) %>% distinct() %>% collect() %>% slice(1:10) %>% pull() %>%  paste0(collapse = ", ")
  })
  
  output$wholedf <- renderDT({
    data_rct() %>% select(input$selection) %>% collect %>% datatable(filter = "top", rownames = FALSE, selection = 'none')
  })
  
  # add section -------------------------------------------------------------------------------------------------------------
  
  # want to simulate an added row of data
  # will be reactive since we have edits and can overwrite it with the uploaded csv
  # in reality this won't exist and will be initialized by the upload button only
  to_add_rct <- tibble(name = "Bill", height = 2001, mass = 600, birth_year = 23, species = "Hillbilly") %>%
    reactiveVal()
  
  # here is where we can update the data to add based on the csv input
  # the upload thing has name, size, type, and datapath
  # begins as NULL (not empty string like input boxes)
  observe(label = "Update to_add from csv", {
    if (!is.null(input$add)) {
      input$add$datapath %>% read_csv %>% to_add_rct()
    }
  })
  
  # updating the to_add data based on edit actions (Ctrl + Enter)
  # not super useful, but cool
  observeEvent(label = "Update to_add from cell edit", input$uploaded_cell_edit, {
    editData(to_add_rct(), input$uploaded_cell_edit, rownames = FALSE) %>% 
      to_add_rct()
  })
  
  output$uploaded <- renderDT(to_add_rct() %>% datatable(editable = "row", selection = 'none', rownames = FALSE, options = list(dom = "t")))
  
  # combining sample of existing data then
  # making a hidden marker column to bold the new stuff
  output$submitted <- renderDT(
    data_rct() %>% 
      head %>% 
      collect %>% 
      bind_rows(
        to_add_rct() %>% mutate(added = TRUE) %>% relocate(added), 
        .
      ) %>% 
      datatable(
        ., 
        editable = "row", rownames = FALSE, selection = 'none',
        options = list(
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

  output$result <- renderText('Not submitted yet.')
  
  # so both of these options work (in_place does it invisibly)
  # and neither cause nasty nesting SQL since we are explicitly computing the command
  # It was making two temp tables in one round - one of them is probs the "to_add" table
  # I think copy_inline makes it only generate one
  observeEvent(label = "Submit add to temp table", input$submitAdd, {
    print(RSQLite::dbListTables(con))
    
    to_add_tbl <- dbplyr::copy_inline(con, to_add_rct())
    # explicitly update data_rct(), because here the identity of data_rct() actually changes - it becomes a different tbl (a temp one)
    data_rct() %>% rows_insert(to_add_tbl, conflict = "ignore") %>% compute %>% data_rct()
    
    output$submitResult <- if (input$submitAdd == 1) "Data added!" %>% renderText()
    else paste0("Data added! (x", input$submitAdd, "!)" ) %>% renderText()
  })
  
  # this will actually modify the data - idk how to access the temp table
  # so I suppose if the copied table is temporary, then maybe we can just do the connection bit and it won't 
  # keep the temp table?
  observeEvent(label = "Commit add to main table", input$commitAdd, {
    writeCon <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
    mainData <- tbl(writeCon, "data")
    rows_insert(mainData, to_add_rct(), conflict = "ignore", copy = TRUE, in_place = TRUE)
    DBI::dbDisconnect(writeCon)
  })
  
  # edit section -------------------------------------------------------------------------------------------------------------
  
  # # somehow the observes being separate makes it better

  observe(label = "Edit View Columns", updateSelectInput(
    session, 'editSelection', choices = colnames(data_rct()), selected = colnames(data_rct())[1:5]
  ))
  observe(label = "Edit Filter Column", updateSelectInput(
    session, 'filterSelection', choices = input$editSelection, selected = input$filterSelection
  ))
  observe(label = "Edit Filter Val", updateSelectInput(
    session, 'valueSelection', 
    choices = if ( input$filterSelection == "" ) "" else data_rct() %>% pull(input$filterSelection) %>% unique %>% sort
  ))
  
  # editTable rendering
  observe(label = "Show Edit Table",
    output$editTable <- renderDT({
      selectData <- data_rct() %>% select(input$editSelection)
      filterData <- if (is.null(input$valueSelection) | any(input$valueSelection == "")) {
        selectData %>% collect() 
      } else {
        selectData %>% 
          filter(
            !!sym(input$filterSelection) %in% input$valueSelection
          ) %>% 
          collect()
      }
      filterData %>% datatable(
        rownames = FALSE, selection = 'none', editable = 'row', 
        options = list(dom = 'tpli')
      )
      
    })
  )
  
}



shinyApp(ui = ui, server = server)







