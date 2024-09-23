
viewServer <- function(id, data_tbl_rct) {
  moduleServer(id, function(input, output, session) {
    # somehow the observes being separate makes it better
    observe(updateSelectInput(
      session, 'select', choices = colnames(data_tbl_rct()), selected = colnames(data_tbl_rct())[1:5]
    ))
    observe(updateSelectInput(
      session, 'filter', choices = input$select, selected = input$filter
    ))
    observe(updateSelectInput(
      session, 'value', 
      choices = if (input$filter == "") ""
      else data_tbl_rct() %>% pull(input$filter) %>% unique %>% sort
    ))
    
    # initialize with just default data
    view_rct <- reactiveVal()

    # still doing the null col thing if we do observeEvent
    # I honestly don't understand why
    # also still not keeping up with edits
    # maybe since the tbl doesn't change
    # it techincally doesn't update?
    # but the view things do update correctly
    # maybe if we handle null select and then do update
    # based on data_tbl_rct()?
    observe({
      data_tbl_rct() %>%
        select(input$select) %>% {
          if (is.null(input$value) | any(input$value == "")) .
          else filter(., !!sym(input$filter) %in% input$value )
        } %>% view_rct()
    })
    
    observeEvent(view_rct(), {
      output$table <- view_rct() %>% 
        collect() %>% 
        datatable(
          rownames = FALSE, selection = 'none', editable = FALSE, options = list(dom = 'tpli'),
          filter = "top"
        ) %>% 
        renderDT()
    })
  })
}

addServer <- function(id, data_tbl_rct, con) {
  moduleServer(id, function(input, output, session) {
    to_add_rct <- reactiveVal()
    observeEvent(input$add, {
      input$add$datapath %>% read_csv %>% to_add_rct()
    })
    # render the uploaded table
    observeEvent(to_add_rct(), {
      output$uploaded <- to_add_rct() %>% rhandsontable() %>% renderRHandsontable()
    })
    # updating the to_add data based on edit actions (Ctrl + Enter)
    # also updating the "simulated" add data
    observeEvent(input$uploaded, {
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
    observeEvent(input$submit, {
      to_add_tbl <- dbplyr::copy_inline(con, to_add_rct())
      data_tbl_rct() %>% rows_insert(to_add_tbl, conflict = "ignore") %>% compute %>% data_tbl_rct()
      output$submitResult <- 
        if (input$submitAdd == 1) "Data added!" %>% renderText()
        else paste0("Data added! (x", input$submitAdd, "!)" ) %>% renderText()
    })
    
    # this will actually modify the data (not the temp table)
    observeEvent(input$commit, {
      mainData <- tbl(con, "data")
      rows_insert(mainData, to_add_rct(), conflict = "error", copy = TRUE, in_place = TRUE)

      output$commitResult <- 
        if (input$commitResult == 1) "Committed!" %>% renderText()
        else paste0("Can't commit more than once (I think)")
      # How to reset add data?? null and reseting the data doesn't work
      unlink(input$add) # i forget if this works
    })
  })
}

editServer <- function(id, data_tbl_rct, con) {
  moduleServer(id, function(input, output, session) {
    hot_edit_rct <- reactiveVal()
    
    # somehow the observes being separate makes it better
    observe(updateSelectInput(
      session, 'select', choices = colnames(data_tbl_rct()), selected = colnames(data_tbl_rct())[1:5]
    ))
    observe(updateSelectInput(
      session, 'filter', choices = input$select, selected = input$filter # this maintains the val
    ))
    observe(updateSelectInput(
      session, 'value', 
      choices = if (input$filter == "") ""
      else data_tbl_rct() %>% pull(input$filter) %>% unique %>% sort
    ))
    
    # update edit_rct based on current selection from data_tbl_rct()
    # for some reason the whole factors -> dropdown thing isn't working, idk why
    observe({
      {if (!is.null(input$select)) {
        data_tbl_rct() %>% 
          select(input$select) %>% {
            if (is.null(input$value) | any(input$value == "")) . 
            else filter(., !!sym(input$filter) %in% input$value )
          }
      } else data_tbl_rct() } %>% hot_edit_rct()
      
      # why does this have to be in an observe block?
      output$table <- hot_edit_rct() %>% 
        collect() %>% 
        rhandsontable(height = 500) %>% 
        hot_col("name", readOnly = TRUE) %>%  # can make primary key read only to avoid wrecking
        renderRHandsontable()
    })
    
    observeEvent(input$write, {
      edits <- input$table %>% hot_to_r()
      # can we grab only the displayed things? or what even is happening here?
      print(edits %>% as_tibble())
      edits_tbl <- dbplyr::copy_inline(con, edits)
      rows_update(
        tbl(con, "data"), 
        edits_tbl, 
        unmatched = "ignore", in_place = TRUE
      )
      
      # refresh reactive data to update view table
      # this should work
      tbl(con, "data") %>% data_tbl_rct()
    })
    
    # just re-rendering the table when undo since underlying data does not change until saved
    observeEvent(input$undo, {
      output$table <- hot_edit_rct() %>% 
        collect() %>% 
        rhandsontable(height = 500) %>% 
        hot_col("name", readOnly = TRUE) %>% 
        renderRHandsontable()
    })
  })
}

utilsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$refresh, refreshDB())
  })
}

server <- function(input, output, session) {
  
  # so these functions don't appear for some reason
  # and aren't visible inside the modules
  con <- DBI::dbConnect(RSQLite::SQLite(), here("data", "testdb.sqlite"))
  
  # can the server functions see this?
  # but the question is, are they getting the reference or the value?
  data_tbl_rct <- tbl(con, "data") %>% reactiveVal()
  
  # maybe we should just pass it the connection instead?
  # view still not keeping in sync with edits
  # needs to be interacted with to re-render
  viewServer('disp', data_tbl_rct)
  addServer('add', data_tbl_rct, con)
  editServer('edit', data_tbl_rct, con)
  
  utilsServer('util')
  
  onStop(function() {
    DBI::dbDisconnect(con)
    cat("Closing connection...\n")
  })
}