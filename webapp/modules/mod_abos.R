choices_abos <- setNames(
  c(10,3,6), # values (what server receives)
  c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo") # labels (what user sees)
)

mod_abos_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Abos verwalten"),
    
    hr(),
    
    h4("Abgelaufene 10er-Abos"),
    
    dataTableOutput(ns("abo_10_expired")),
    
    actionButton(ns("archive_abo_10"), "Abo archivieren", disabled = FALSE),
    
    hr(),
    
    h4("Abgelaufene Monats-Abos"),
    
    dataTableOutput(ns("abo_month_expired")),
    
    actionButton(ns("archive_abo_month"), "Abo archivieren", disabled = FALSE),
    
    hr(),
    
    h3("Abo-Preise"),
    
    hr(),
    
    h4("Abo-Preise anpassen"),
    
    selectInput(ns("course"), "Kurs wählen", choices = NULL),
    
    selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
    
    numericInput(ns("new_price"), "Neuer Abo-Preis", value = 300, min = 0, step = 10),
    
    actionButton(ns("update_price"), "Abo-Preis anpassen")

  )
}


mod_abos_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # THIS is your ns function in server -> needed for modals
    
    # Abgelaufene Abos
    
    ## 10er Abos
    
    # table with expired abos
    data_abo_10_expired <- reactive({
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      
      attended_courses <- get_attended_courses_abo_10(con)
      
      # filter
      abo_10_expired <- attended_courses |> filter(still_left == 0)
      
      # return
      abo_10_expired
    })
    
    output$abo_10_expired <- renderDT({
      
      # make a nice version of the df for the ui
      abo_10_expired_display <- data_abo_10_expired() |> 
        select(vorname, name, still_left) |> 
        arrange(still_left) |> 
        rename(
          "Name" = name,
          "Vorname" = vorname,
          "Übrige Termine" = still_left
        )
      
      # render
      datatable(
        abo_10_expired_display,
        selection = "single",
        options = list(pageLength = 10)
      )
      
    })
    
    ## Monats-Abos
    data_abo_month_expired <- reactive({
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      
      members <- get_members_abo_month(con)
      members$abo_end <- as.Date(members$abo_end)
      
      # filter
      members_abo_month_expired <- members |> 
        filter(abo_end <= today()) |> 
        arrange(abo_end)
      
      # return
      members_abo_month_expired 
    })
    
    output$abo_month_expired <- renderDT({
      
      # make a nice version of the df for the ui
      abo_month_expired_display <- data_abo_month_expired() |> 
        mutate(abo_end = format_swiss_date(abo_end)) |> 
        select(vorname, name, abo_end) |> 
        rename(
          "Vorname" = vorname,
          "Name" = name,
          "Abo gültig bis" = abo_end
        )
      
      # render
      datatable(
        abo_month_expired_display,
        selection = "single",
        options = list(pageLength = 10)
      )
    })
    
    
    
    # DELETE
    
    # SOMEWHERE, I STILL NEED TO IMPLEMENT THE UPDATE END DATE FOR THE 10 ABO
    
    # reactive value to store if a abo 10 or abo month is being archived
    rv_archive_type <- reactiveVal()
    certificate_list <- reactiveVal(integer())
    
    ## Modals
    show_archive_modal <- function() {
      
      showModal(
        modalDialog(
          title = "Abo archivieren",
          
          "Was soll nach der Abo-Archivierung passieren?",
          
          footer = tagList(
            modalButton("Abbrechen"), # this button closes the modal when pressed
            
            actionButton(
              ns("archive_only"),
              "Nur archivieren"
            ),
            
            actionButton(
              ns("replace_abo"),
              "Archivieren und ein neues Abo erstellen"
            )
          )
        )
      )
    }
    
    show_add_abo_modal <- function() {
      showModal(
        modalDialog(
          title = "Neues Abo erstellen",
          
          selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
          dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy"),
          
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns("confirm_new_abo"), "Abo erstellen")
          )
        )
      )
    }
    
    show_certificate_modal <- function() {
      showModal(
        modalDialog(
          title = "Zertifikate",
          
          "Soll für diese Person am Schluss ein Zertifikat erstellt werden?",
          
          footer = tagList(
            
            actionButton(
              ns("certificate_no"),
              "Nein"
            ),
            
            actionButton(
              ns("certificate_yes"),
              "Zur Zertifikats-Liste hinzufügen"
            )
          )
        )
      )
    }
    
    ## functions
    get_selected_row_data <- function(archive_type) {
      if (archive_type == "10") {
        selected_row <- input$abo_10_expired_rows_selected
        data <- data_abo_10_expired()[selected_row, ]
      } else {
        selected_row <- input$abo_month_expired_rows_selected
        data <- data_abo_month_expired()[selected_row, ]
      }
      
      # return
      data
    }
    
    ## server logic
    # When user selects an abo to remove, the first modal is shown
    observeEvent(input$archive_abo_10, {
      show_archive_modal()
      rv_archive_type("10")
    })
    observeEvent(input$archive_abo_month, {
      show_archive_modal()
      rv_archive_type("month")
    })
    
    # in case abo only needs to be archived
    observeEvent(input$archive_only, {
      # close the previous modal
      removeModal()
      
      # get the selected row data
      data <- get_selected_row_data(rv_archive_type())
      
      # safety check that row is selected
      if (length(data) > 0) {
        # archive the abo
        archive_abo(con, data$abo_id)
        
        # ask the user if the current member should be added to certificate list
        show_certificate_modal()
      } else {
        removeModal()
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
    })
    
    # in case new abo gets created
    observeEvent(input$replace_abo, {
      # close previous modal
      removeModal()
      
      # get selected row data
      data <- get_selected_row_data(rv_archive_type())
      
      # safety check
      if (length(data) > 0) {
        # show the modal for adding the abo
        show_add_abo_modal()
      } else {
        removeModal()
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
    })
    observeEvent(input$confirm_new_abo, {
      req(input$abo, input$abo_start)
      
      # get selected row data
      data <- get_selected_row_data(rv_archive_type())
      
      # add the new abo
      add_abo(con, input$abo, data$user_id, input$abo_start)
      global_refresh$abos <- global_refresh$abos + 1
      
      # archive the old abo
      archive_abo(con, data$abo_id)
      
      # ask the user if the current member should be added to certificate list
      show_certificate_modal()
    })
    
    # ask the user about certificates
    observeEvent(input$certificate_yes, {
      # append current user_id to the list for making certificates at the end
      certificate_list(
        c(certificate_list(), get_selected_row_data(rv_archive_type())$user_id)
      )
      
      # close the previous modal
      removeModal()
      
      # now update the global refresh such that the list of expired abos gets updated
      global_refresh$abos <- global_refresh$abos + 1
    })
    
    observeEvent(input$certificate_no, {
      # close the previous modal
      removeModal()
      
      # now update the global refresh such that the list of expired abos gets updated
      global_refresh$abos <- global_refresh$abos + 1
    })
    
    
    # Abo price
    
    # update choices for input$course -> gets refreshed if new courses get added
    observeEvent(global_refresh$courses, {
      courses <- get_courses(con)
      choices_courses <- setNames(
        courses$course_id, # values (what server receives)
        courses$kursname # labels (what user sees)
      )
      
      updateSelectInput(
        session,
        "course",
        choices = choices_courses
      )
    })
    
    # change value for price input based on selected inputs
    observeEvent(input$abo, {
      req(input$course)
      
      price <- get_abo_price(con, input$course, input$abo)$abo_price
      
      updateNumericInput(
        session,
        "new_price",
        value = price
      )
    })
    observeEvent(input$course, {
      req(input$abo)
      
      price <- get_abo_price(con, input$course, input$abo)$abo_price
      
      updateNumericInput(
        session,
        "new_price",
        value = price
      )
    })
    
    # UPDATE
    observeEvent(input$update_price, {
      req(input$course, input$abo, input$new_price)
      
      update_abo_price(con, input$course, input$abo, input$new_price)
    })
  })
}