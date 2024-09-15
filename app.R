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

library(rhandsontable) # 7/31/24, testing to see if edit functionality is better

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
  rHandsontableOutput('uploaded'),
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

hot_edit_pan <- tabPanel(
  title = "HOT Edit some data",
  p("This section is for targeted editing of specific cells - if you don't have a .csv file and just want to edit a row or two, this is your best bet."),
  hr(),
  bookmarkButton(),
  fluidRow(
    column(4,selectInput('hot_editSelect', 'Select Cols', choices = '', multiple = TRUE)),
    column(4, selectInput('hot_editFilter', 'Select Filter Col', choices = '')),
    column(4, selectInput('hot_editValue', 'Select Filter Val(s)', choices = '', multiple = TRUE))
  ),
  hr(),
  actionButton("hot_edit_write", label = "Save", icon = NULL),
  actionButton("hot_edit_undo", label = "Undo", icon = NULL),
  rHandsontableOutput('hot_editTable')
)

utils_pan <- tabPanel(
  title = "Utilities",
  actionButton("refresh", "Refresh Database", icon = icon("rotate"))
)

# made function to try bookmarking, but 
# bookmark does not seem to store anything
ui <- function(request) {fluidPage(
  titlePanel("Testing..."), 
  tabsetPanel(
    display_pan,
    add_pan,
    hot_edit_pan,
    utils_pan
  )
)}

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
  
  # For some reason RConnect does not like duck db. Idk why, but it's annoying.
  
  
  observeEvent(input$refresh, refreshDB())
  
  # make_connection <- function() { DBI::dbConnect(RSQLite::SQLite(), here("data", "testdb.sqlite")) }
  
  # make_connection <- function() { duckdb::dbConnect(duckdb::duckdb(), here("data", "test_duckdb")) }
  make_connection <- function() { DBI::dbConnect(RSQLite::SQLite(), here("data", "testdb.sqlite")) }
  close_connection <- function(con) { DBI::dbDisconnect(con) }
  
  con <- make_connection()
  
  # this error happens whenever I click stop - it only goes away when I don't connect at all
  # Error: invoke_wrapped: throwing std::runtime_error
  # this error actually seems to be related to the viewer pane in RStudio
  # The error occurs when hitting "Stop" when the app is running in the viewer pane,
  # but does not occur when the app runs in a new window or external. Pretty weird.
  # test test test
  onStop(function() {
    cat("Closing connection...\n")
    close_connection(con)
  })
  
  data_tbl_rct <- tbl(con, "data") %>% reactiveVal()
  
  # view section -------------------------------------------------------------------------------------------------------------

  
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
  
  # initialize with just default data
  view_rct <- reactiveVal()
  data_tbl_rct |> view_rct()
  
  # update view_rct based on current selection from data_tbl_rct()
  # only render the table once we have some select params (which should be within microseconds)
  # this doesn't quite update when we change the base data for some reason
  # maybe we need to update based on data_tbl_rct?
  # observe(label = "Show View Table", { if (!is.null(input$viewSelect)) {
  #     data_tbl_rct() %>% 
  #       select(input$viewSelect) %>% {
  #         if (is.null(input$viewValue) | any(input$viewValue == "")) . 
  #         else filter(., !!sym(input$viewFilter) %in% input$viewValue )
  #       } %>% view_rct()
  #     
  #     output$viewTable <- renderDT(
  #       datatable(
  #         view_rct() %>% collect(), 
  #         rownames = FALSE, selection = 'none', editable = FALSE, options = list(dom = 'tpli'),
  #         filter = "top"
  #       )
  #     ) 
  #   }
  # })
  
  observeEvent(data_tbl_rct(), label = "Show View Table", {
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
  })
  
  
  
  
  # add section -------------------------------------------------------------------------------------------------------------
  
  # will be reactive since we have edits and can overwrite it with the uploaded csv
  # in reality this won't exist and will be initialized by the upload button only
  # to_add_rct <- tibble(name = "Bill", height = 2001, mass = 600, birth_year = 23, species = "Hillbilly") %>%
  #   reactiveVal()
  
  to_add_rct <- reactiveVal()
  
  
  observeEvent(input$add, label = "Update to_add from csv", {
    input$add$datapath %>% read_csv %>% to_add_rct()
  })
  
  # render the uploaded table
  observeEvent(to_add_rct(), {
    output$uploaded <- to_add_rct() %>% rhandsontable() %>% renderRHandsontable()
  })
  
  # updating the to_add data based on edit actions (Ctrl + Enter)
  # also updating the "simulated" add data
  observeEvent(label = "Update to_add from cell edit", input$uploaded, {
    input$uploaded %>% hot_to_r() %>% to_add_rct()
    
    # combining sample of existing data then
    # making a hidden marker column to bold the new stuff
    output$submitted <- renderDT(
      data_tbl_rct() %>% 
        head %>% 
        collect %>% 
        bind_rows(
          to_add_rct() %>% 
            mutate(added = TRUE) %>% 
            relocate(added), 
          . # stacking, wish there was an insert function
        ) %>% 
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
  })

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
    output$submitResult <- 
      if (input$submitAdd == 1) "Data added!" %>% renderText()
      else paste0("Data added! (x", input$submitAdd, "!)" ) %>% renderText()
  })
  
  # this will actually modify the data (not the temp table)
  observeEvent(label = "Commit add to main table", input$commitAdd, {
    writeCon <- make_connection()
    mainData <- tbl(writeCon, "data")
    
    rows_insert(mainData, to_add_rct(), conflict = "error", copy = TRUE, in_place = TRUE)
    # addResult <- tryCatch({
    #   rows_insert(mainData, to_add_rct(), conflict = "error", copy = TRUE, in_place = TRUE) |> 
    #     paste0(nrow(.), "rows committed!")
    # }, warning = function(w) {
    #   paste0("Warning:", conditionMessage(w))
    # }, error = function(e) {
    #   paste0("Error:", conditionMessage(e))
    # })
    # 
    # print("Add result")
    # print(addResult)
    
    
    DBI::dbDisconnect(writeCon)
    
    # output$commitResult <- 
    #   if (input$submitAdd == 1) "Data added!" %>% renderText()
    #   else paste0("Data committed! (x", input$submitAdd, "!)" ) %>% renderText()
    
    # output$commitResult <- addResult |> renderText()
    
    # How to reset add data??
    # also tried this
    # rm doesn't work, nor does setting the input$add to null
    unlink(input$add)
    # NULL |> to_add_rct()
  })
  
  
  # hot_edit section -------------------------------------------------------------------------------------
  hot_edit_rct <- reactiveVal()
  
  # somehow the observes being separate makes it better
  observe(updateSelectInput(
    session, 'hot_editSelect', choices = colnames(data_tbl_rct()), selected = colnames(data_tbl_rct())[1:5]
  ))
  observe(updateSelectInput(
    session, 'hot_editFilter', choices = input$hot_editSelect, selected = input$hot_editFilter # this maintains the val
  ))
  observe(updateSelectInput(
    session, 'hot_editValue', 
    choices = if (input$hot_editFilter == "") ""
    else data_tbl_rct() %>% pull(input$hot_editFilter) %>% unique %>% sort
  ))
  
  # update edit_rct based on current selection from data_tbl_rct()
  # for some reason the whole factors -> dropdown thing isn't working
  # idk why
  observe({
    {if (!is.null(input$hot_editSelect)) {
      data_tbl_rct() %>% 
        select(input$hot_editSelect) %>% {
          if (is.null(input$hot_editValue) | any(input$hot_editValue == "")) . 
          else filter(., !!sym(input$hot_editFilter) %in% input$hot_editValue )
        }
    } else data_tbl_rct() } %>% hot_edit_rct()
    
    # why does this have to be in an observe block?
    output$hot_editTable <- hot_edit_rct() %>% 
      collect() %>% 
      rhandsontable(height = 500) %>% 
      hot_col("name", readOnly = TRUE) %>%  # can make primary key read only to avoid wrecking
      renderRHandsontable()
  })
  
  # input$hot_editTable isn't great since it tries to update every time we change the view filter
  # I guess we should just update the data rct here?
  # this way the view pane should update when we save edits
  observeEvent(input$hot_edit_write, {
    edits <- input$hot_editTable %>% hot_to_r()
    # can we grab only the displayed things? or what even is happening here?
    print(edits %>% as_tibble())
    write_con <- make_connection()
    edits_tbl <- dbplyr::copy_inline(write_con, edits)
    rows_update(
      tbl(write_con, "data"), 
      edits_tbl, 
      unmatched = "ignore", in_place = TRUE
    )
    DBI::dbDisconnect(write_con)
    
    tbl(con, "data") %>% data_tbl_rct()
  })
  
  # just re-rendering the table when undo since underlying data does not change until saved
  observeEvent(input$hot_edit_undo, {
    output$hot_editTable <- hot_edit_rct() %>% 
      collect() %>% 
      rhandsontable(height = 500) %>% 
      hot_col("name", readOnly = TRUE) %>% 
      renderRHandsontable()
  })
  
  
  
  # end of server function
}


# this is how to make it accessible for multiple local sessions if you're curious
# shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8787))

# for deployment
shinyApp(
  ui = ui, server = server, enableBookmarking = "server"
)






