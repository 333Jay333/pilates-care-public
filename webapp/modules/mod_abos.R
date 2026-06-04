choices_abos <- setNames(
  c(10,3,6), # values (what server receives)
  c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo") # labels (what user sees)
)

mod_abos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      tabPanel(
        title = "Abos archivieren",
        
        tagList(
          tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
          
          div(style = "margin-top: 2rem;"), # top margin
          fluidRow(
            column(
              6,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-xbox-x"), "Abgelaufene 10er-Abos"
                ),
                dataTableOutput(ns("abo_10_expired")),
                actionButton(
                  ns("archive_abo_10"), 
                  tagList(tags$i(class = "ti ti-archive"), "Abo archivieren"), 
                  class = "btn-primary btn-sm", 
                  disabled = FALSE
                )
              ),
            ),
            
            column(
              6,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-xbox-x"), "Abgelaufene Monats-Abos"
                ),
                dataTableOutput(ns("abo_month_expired")),
                actionButton(
                  ns("archive_abo_month"), 
                  tagList(tags$i(class = "ti ti-archive"), "Abo archivieren"), 
                  class = "btn-primary btn-sm", 
                  disabled = FALSE
                )
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              div(
                class = "pc-card",
                
                # Clickable header that toggles the collapse
                tags$p(
                  class = "pc-section-label",
                  style = "cursor: pointer; user-select: none; margin-bottom: 0;",
                  `data-toggle` = "collapse",
                  `data-target` = paste0("#", ns("abo_list_collapse")),
                  tags$i(class = "ti ti-list"), "Aktive Abos archivieren",
                  tags$i(class = "ti ti-chevron-down", style = "margin-left: auto;")
                ),
                
                # Collapsible content — no "in" class means collapsed by default
                div(
                  id = ns("abo_list_collapse"),
                  class = "collapse",  # add "in" here to start expanded instead
                  style = "margin-top: 1rem;",
                  dataTableOutput(ns("abo_list")),
                  div(
                    style = "margin-top:5px",
                    actionButton(
                      ns("archive_abo_list"),
                      tagList(tags$i(class = "ti ti-archive"), "Abo archivieren"),
                      class = "btn-primary btn-sm", 
                      disabled = FALSE
                    )
                  )
                )
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-file-certificate"), "Zertifikate für archivierte Abos"
                ),
                selectInput(ns("therapist"), "Therapeut*in", choices = NULL, multiple = FALSE),
                h5(strong("Für folgende Personen wird ein Zertifikat generiert:")),
                tableOutput(ns("certificates_to_generate")),
                actionButton(
                  ns("make_certificates"), 
                  tagList(tags$i(class = "ti ti-file-plus"), "Zertifikate erstellen"), 
                  class = "btn-primary btn-sm", 
                  disabled = TRUE
                )
              )
            )
          )
        )
      )
    ),
    
    hr(),
    
    h4("Abgelaufene 10er-Abos"),
    
    # dataTableOutput(ns("abo_10_expired")),
    
    actionButton(ns("archive_abo_10"), "Abo archivieren", disabled = FALSE),
    
    hr(),
    
    h4("Abgelaufene Monats-Abos"),
    
    # dataTableOutput(ns("abo_month_expired")),
    
    actionButton(ns("archive_abo_month"), "Abo archivieren", disabled = FALSE),
    
    hr(),
    
    h4("Abo archivieren"),
    
    hr(),
    
    h4("Zertifikate für archivierte Abos"),
    
   
    
    
    
    
    
    hr(),
    
    h3("Neues Abo erstellen"),
    
    selectInput(ns("members"), "Person wählen", choices = NULL, multiple = FALSE),
    
    selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
    
    dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy"),
    
    actionButton(ns("member_abo_add"), "Neues Abo erstellen", disabled = TRUE),
    
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
    
    ns <- session$ns  # ns function in server -> needed for modals
    
    # ABOS ARCHIVIEREN
    
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
        options = list(
          pageLength = 10,
          language = german_datatable(),
          dom = "tp"  # show only the table without search bar (dom = "f"), "show X entries" (dom = "l"), pagination ("p"), Info ("Showing 1 to 10 of 50") ("i"), processing indicator ("r") 
        )
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
        options = list(
          pageLength = 10,
          language = german_datatable(),
          dom = "tp" # show only the table without search bar (dom = "f"), "show X entries" (dom = "l"), pagination ("p"), Info ("Showing 1 to 10 of 50") ("i"), processing indicator ("r") 
        )
      )
    })
    
    
    
    # DELETE
    
    # reactive value to store if a abo 10 or abo month is being archived
    rv_archive_type <- reactiveVal()
    selected_member <- reactiveVal(NULL)
    certificate_list <- reactiveVal(
      data.frame(
        user_id = integer(),
        abo_id = integer(),
        vorname = character(),
        name = character()
        
      )
    )
    
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
      if (nrow(data) > 0) {
        # in case it is a 10 abo, set the end date to today
        if (rv_archive_type() == 10) {
          update_abo_end(con, data$abo_id, today())
        }
        
        # save selected member
        selected_member(data)
        
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
      if (nrow(data) > 0) {
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
      
      # save selected member
      selected_member(data)
      
      # add the new abo
      add_abo(con, input$abo, data$user_id, input$abo_start)
      global_refresh$abos <- global_refresh$abos + 1
      
      # in case the old abo is a 10 abo, set the end date to today
      if (rv_archive_type() == 10) {
        update_abo_end(con, data$abo_id, today())
      }
      
      # archive the old abo
      archive_abo(con, data$abo_id)
      
      # ask the user if the current member should be added to certificate list
      show_certificate_modal()
    })
    
    # ask the user about certificates
    observeEvent(input$certificate_yes, {
      
      data <- selected_member()
      
      # append current user_id and abo_id to the list for making certificates at the end
      certificate_list(
        rbind(
          certificate_list(),
          data.frame(
            user_id = data$user_id,
            abo_id = data$abo_id,
            vorname = data$vorname,
            name = data$name
          )
        )
      )
      
      # close the previous modal
      removeModal()
      
      # now update the global refresh such that the list of expired abos gets updated
      global_refresh$abos <- global_refresh$abos + 1
      
      print(certificate_list())
    })
    
    observeEvent(input$certificate_no, {
      # close the previous modal
      removeModal()
      
      # now update the global refresh such that the list of expired abos gets updated
      global_refresh$abos <- global_refresh$abos + 1
    })
    
    # archive member from list
    
    # get data for the table
    data_abo_list <- reactive({
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      
      data <- get_active_abos(con)
      
      # return
      data
    })
    
    # render the table
    output$abo_list <- renderDT({
      
      # make a nice version
      abo_list_display <- data_abo_list() |> 
        select(vorname, name, abo_type) |> 
        mutate(
          abo_type = abo_type |>
            recode_values(
              10 ~ "10er Abo",
              3 ~ "3-Monats Abo",
              6 ~ "6-Monats Abo"
            )
        ) |>
        mutate(
          abo_type = sapply(abo_type, abo_badge)
        ) |> 
        rename(
          "Vorname" = vorname,
          "Name" = name,
          "Abo Typ" = abo_type
        )
      
      # render
      datatable(
        abo_list_display,
        selection = "single",
        escape = FALSE, # enable HTML rendering
        options = list(pageLength = 10)
      )
    })
    
    # archive member from list
    observeEvent(input$archive_abo_list, {
      
      # which row is selected
      selected <- input$abo_list_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
      
      # Get the selected row data
      abo_archive <- data_abo_list()[selected, ]
      
      # save selected member for certificate modal
      selected_member(abo_archive)
      
      # in case it is a 10 abo, set the end date to today
      if (abo_archive$abo_type == 10) {
        update_abo_end(con, abo_archive$abo_id, today())
      }
      
      # archive abo
      archive_abo(con, abo_archive$abo_id)
      
      # show certificate modal
      show_certificate_modal()
    })
    
    # make certificates
    
    # update choices in case new therapists get added
    observeEvent(global_refresh$therapists, {
      therapists <- get_therapists(con)
      choices_therapists <- setNames(
        therapists$user_id, # values (what server receives)
        therapists$vorname # labels (what user sees)
      )
      
      updateSelectInput(
        session,
        "therapist",
        choices = choices_therapists
      )
    })
    
    # get data for certificate table
    data_certificates <- reactive({
      
      members <- certificate_list() |> 
        select(vorname, name) |> 
        rename(
          "Vorname" = vorname,
          "Name" = name
        )
      
      # return
      members
    })
    
    # render certificate table
    output$certificates_to_generate <- renderTable(
      data_certificates()
    )
    
    # enable button
    observe({
      if (nrow(certificate_list()) > 0) {
        updateActionButton(session, "make_certificates", disabled = FALSE)
      } else {
        updateActionButton(session, "make_certificates", disabled = TRUE)
      }
    })
    
    # generate the certificates
    observeEvent(input$make_certificates, {
      req(input$therapist)
      
      # ---- disable button immediately ---- -> to prevent double-clicking
      shinyjs::disable("make_certificates")
      
      # ensure re-enable of the button no matter what happens
      on.exit(shinyjs::enable("make_certificates"), add = TRUE)
      
      # make certificates
      make_certificates(con, therapist_user_id = input$therapist, members_user_ids = certificate_list()$user_id)
    })
    
    # ADD NEW ABO
    
    # get members and update if new members get added
    observeEvent(global_refresh$members, {
      members <- get_members(con)
      choices_members <- setNames(
        members$user_id,  # values (what server receives)
        paste(members$vorname, members$name)  # labels (what user sees)
      )
      
      updateSelectInput(
        session,
        "members",
        choices = choices_members
      )
    })
    
    # check if inputs exist to enable button
    observe({
      if (length(input$members) > 0) {
        updateActionButton(session, "member_abo_add", disabled = FALSE)
      } else {
        updateActionButton(session, "member_abo_add", disabled = TRUE)
      }
    })
    
    # if user presses add button, insert new abo
    observeEvent(input$member_abo_add, {
      req(input$members, input$abo, input$abo_start)
      
      active_abo <- get_active_abo_user_id(con, input$members)
      
      # check that user doesn't already have an active abo
      if (nrow(active_abo) > 0) {
        
        showNotification(
          "Abbruch: Diese Person hat bereits ein aktives Abo.",
          type = "warning"
        )
        
        return()
      }
      
      # add abo
      insert_abo(con, input$members, input$abo, input$abo_start)
      
      showNotification(
        "Abo hinzugefügt",
        type = "message"
      )
      
      
    })
    
    # ABO PRICE
    
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