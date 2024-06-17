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

source(here("createdb.R"))

display_pan <- tabPanel(
  title = "Viewing the Data",
  p("This section is for viewing the data only. You can use the three boxes below to filter for specific values of a certain column, or try typing in the search boxes to text-search."),
  hr(),
  fluidRow(
    column(4,selectInput('viewSelect', 'Select Cols', choices = '', multiple = TRUE)),
    column(4, selectInput('viewFilter', 'Select Filter Col', choices = '')),
    column(4, selectInput('viewValue', 'Select Filter Val(s)', choices = '', multiple = TRUE))
  ),
  hr(),
  DTOutput('viewTable')
)

add_pan <- tabPanel(
  title = "Add some data!",
  p("This section is for adding data - once it's uploaded via .csv, you can edit and tweak things as needed. This also provides an opportunity to batch-edit values. For example, if you upload a .csv file with a list of existing IDs (ones that are already in the database), you can upload columns that you want to update (ex. change Sent from FALSE to TRUE, etc.)."),
  fileInput('add', 'upload data to add'),
  "Edit your uploaded data here if anything looks wrong (Ctrl + Enter to save changes, Esc to discard). If columns are missing, edit and reupload the .csv file instead.",
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
  p("This section is for targeted editing of specific cells - if you don't have a .csv file and just want to edit a row or two, this is your best bet."),
  hr(),
  fluidRow(
    column(4,selectInput('editSelect', 'Select Cols', choices = '', multiple = TRUE)),
    column(4, selectInput('editFilter', 'Select Filter Col', choices = '')),
    column(4, selectInput('editValue', 'Select Filter Val(s)', choices = '', multiple = TRUE))
  ),
  hr(),
  DTOutput('editTable')
)

utils_pan <- tabPanel(
  title = "Utilities",
  actionButton("refresh", "Refresh Database", icon = icon("rotate"))
)

ui <- fluidPage(
  titlePanel("Testing..."), 
  tabsetPanel(
    display_pan,
    add_pan,
    edit_pan,
    utils_pan
  )
)

server <- function(input, output, session) {
  
  # overall, data_tbl_rct() is a reactve, lazy tbl reference
  # to_add_rct() is an in-memory tibble, actual data
  
  # utilities ------------------------------------------------------------------------------------------------------------
  # putting our refresh at the top here
  # I think it would be cool to like save a copy of the current state of the
  # main table and then have a master undo-changes option in the utilities
  # So then I guess the data_tbl_rct() would have to be a copy of the current state
  # and then we'd have a main like "save changes" and then
  # you can replace or overwrite your version with the saved / temporary one
  # I feel like that would be OK, and if performance becomes really bad
  # then we can always tweak it later, like find the difference between 
  # our current table and the existing one and then only upsert those rows
  # but really it'd probably be doing the same thing if we do it directly, 
  # so who knows if it would actually be more efficient.
  # Plus we can always try duckdb instead, assuming that's available via rconnnect. 
  # One would have to imagine that's the case.
  
  observeEvent(input$refresh, refreshDB())
  
  # I am going to try to convert it to duckdb and see if that makes it go away
  # nope it doesn't lol
  # I am going to assume it does not matter
  make_connection <- function() { DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite")) }
  make_connection <- function() { duckdb::dbConnect(duckdb::duckdb(), here("test_duckdb")) }
  close_connection <- function(con) { DBI::dbDisconnect(con) }
  
  con <- make_connection()
  
  # this error happens whenever I click stop - it only goes away when I don't connect at all
  # which obviously is not an option
  # Error: invoke_wrapped: throwing std::runtime_error
  # this didn't help
  onStop(function() {
    cat("Closing connection...\n")
    close_connection(con)
  })
  
  data_tbl_rct <- tbl(con, "data") %>% reactiveVal()
  
  # view section -------------------------------------------------------------------------------------------------------------
  view_rct <- reactiveVal()
  
  # somehow the observes being separate makes it better
  observe(label = "View Columns", updateSelectInput(
    session, 'viewSelect', choices = colnames(data_tbl_rct()), selected = colnames(data_tbl_rct())[1:5]
  ))
  observe(label = "View Filter", updateSelectInput(
    session, 'viewFilter', choices = input$viewSelect, selected = input$viewFilter
  ))
  observe(label = "View Val", updateSelectInput(
    session, 'viewValue', 
    choices = if (input$viewFilter == "") ""
    else data_tbl_rct() %>% pull(input$viewFilter) %>% unique %>% sort
  ))
  
  # update view_rct based on current selection from data_tbl_rct()
  # only render the table once we have some select params (which should be within microseconds)
  observe(label = "Show View Table", { if (!is.null(input$viewSelect)) {
      data_tbl_rct() %>% 
        select(input$viewSelect) %>% {
          if (is.null(input$viewValue) | any(input$viewValue == "")) . 
          else filter(., !!sym(input$viewFilter) %in% input$viewValue )
        } %>% view_rct()
      
      output$viewTable <- renderDT(
        datatable(
          view_rct() %>% collect(), 
          rownames = FALSE, selection = 'none', editable = FALSE, options = list(dom = 'tpli'),
          filter = "top"
        )
      ) 
    }
  })
  
  # add section -------------------------------------------------------------------------------------------------------------
  
  # will be reactive since we have edits and can overwrite it with the uploaded csv
  # in reality this won't exist and will be initialized by the upload button only
  to_add_rct <- tibble(name = "Bill", height = 2001, mass = 600, birth_year = 23, species = "Hillbilly") %>%
    reactiveVal()
  
  # remember input$add has has name, size, type, and datapath
  # begins as NULL (not empty string like input boxes)
  observe(label = "Update to_add from csv", {
    if (!is.null(input$add)) {
      input$add$datapath %>% read_csv %>% to_add_rct()
    }
  })
  
  # updating the to_add data based on edit actions (Ctrl + Enter)
  observeEvent(label = "Update to_add from cell edit", input$uploaded_cell_edit, {
    editData(to_add_rct(), input$uploaded_cell_edit, rownames = FALSE) %>% 
      to_add_rct()
  })
  
  output$uploaded <- renderDT(to_add_rct() %>% datatable(editable = "row", selection = 'none', rownames = FALSE, options = list(dom = "t")))
  
  # combining sample of existing data then
  # making a hidden marker column to bold the new stuff
  output$submitted <- renderDT(
    data_tbl_rct() %>% 
      head %>% 
      collect %>% 
      bind_rows(to_add_rct() %>% mutate(added = TRUE) %>% relocate(added), .) %>% 
      datatable(
        ., 
        editable = FALSE, rownames = FALSE, selection = 'none',
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
  
  # copy_inline makes it only generate one temp table (the one with the new row(s))
  observeEvent(label = "Submit add to temp table", input$submitAdd, {
    to_add_tbl <- dbplyr::copy_inline(con, to_add_rct())

    # explicitly update data_tbl_rct(), because here the identity of data_tbl_rct() actually changes - it becomes a different tbl (a temp one)
    # this is semi bad because it means we won't be able to see other changes
    # so for example if we do this submit thing, we now have diverged from the main "data" table
    # so then if we do an edit on the edit pane, we end up not being able to see it
    # but edit_rct is based on the data_tbl, so I guess we still do? Idk it's crazy
    
    data_tbl_rct() %>% rows_insert(to_add_tbl, conflict = "ignore") %>% compute %>% data_tbl_rct()
    
    output$submitResult <- if (input$submitAdd == 1) "Data added!" %>% renderText()
    else paste0("Data added! (x", input$submitAdd, "!)" ) %>% renderText()
  })
  
  # this will actually modify the data (not the temp table)
  observeEvent(label = "Commit add to main table", input$commitAdd, {
    writeCon <- make_connection()
    mainData <- tbl(writeCon, "data")
    rows_insert(mainData, to_add_rct(), conflict = "ignore", copy = TRUE, in_place = TRUE)
    DBI::dbDisconnect(writeCon)
  })
  
  # edit section -------------------------------------------------------------------------------------------------------------
  edit_rct <- reactiveVal()
  
  # somehow the observes being separate makes it better
  observe(label = "Edit View Columns", updateSelectInput(
    session, 'editSelect', choices = colnames(data_tbl_rct()), selected = colnames(data_tbl_rct())[1:5]
  ))
  observe(label = "Edit Filter Column", updateSelectInput(
    session, 'editFilter', choices = input$editSelect, selected = input$editFilter # this maintains the val
  ))
  observe(label = "Edit Filter Val", updateSelectInput(
    session, 'editValue', 
    choices = if (input$editFilter == "") ""
              else data_tbl_rct() %>% pull(input$editFilter) %>% unique %>% sort
  ))
  
  # update edit_rct based on current selection from data_tbl_rct()
  observe(label = "Show Edit Table", {
    if (!is.null(input$editSelect)) {
      data_tbl_rct() %>% 
        select(input$editSelect) %>% {
          if (is.null(input$editValue) | any(input$editValue == "")) . 
          else filter(., !!sym(input$editFilter) %in% input$editValue )
        }
    } %>% edit_rct()
    
    output$editTable <- renderDT(
      datatable(
        edit_rct() %>% collect(), 
        rownames = FALSE, selection = 'none', editable = 'row', options = list(dom = 'tpli')
      )
    )
  })
  
  # edit table handling - I guess we'll commit directly to the server!
  # updating data based on edit actions (Ctrl + Enter)
  # it will break if you try to edit the primary key (if you edit names)
  # however, if we change this to upsert then it would work
  observeEvent(label = "Update to_add from cell edit", input$editTable_cell_edit, {
    edits <- editData(edit_rct() %>% collect, input$editTable_cell_edit, rownames = FALSE)
    write_con <- make_connection()
    edits_tbl <- dbplyr::copy_inline(write_con, edits)
    # edits contains more than just the single edited row though so idk what will happen here
    rows_update(
      tbl(write_con, "data"), 
      edits_tbl, 
      unmatched = "ignore", in_place = TRUE
    )
    DBI::dbDisconnect(write_con)
  })
  
  # end of server function
}


# this is how to make it accessible for multiple local sessions if you're curious
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8787))







