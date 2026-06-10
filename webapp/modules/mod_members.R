choices_abos <- setNames(
  c(10,3,6), # values (what server receives)
  c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo")
)

mod_members_ui <- function(id) {
  ns <- NS(id)
    
  tagList(
    # use custom css
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    
    fluidRow(
      # Left column: add form
      column(
        4,
        div(class = "pc-card",
          
          tags$p(
            class = "pc-section-label", 
            tags$i(class = "ti ti-user-plus"), "Teilnehmer*in hinzufügen"
          ),
          fluidRow(
           column(6, textInput(ns("vorname"), "Vorname")),
           column(6, textInput(ns("name"), "Name"))
          ),
          tags$hr(),
          
          # Clickable header that toggles the collapse
          tags$p(
            class = "pc-section-label",
            style = "cursor: pointer; user-select: none; margin-bottom: 0;",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("contact_list_collapse")),
            tags$i(class = "ti ti-address-book"), "Versicherung & Kontakt (optional)",
            tags$i(class = "ti ti-chevron-down", style = "margin-left: auto;")
          ),
          
          # Collapsible content — no "in" class means collapsed by default
          div(
            id = ns("contact_list_collapse"),
            class = "collapse",  # add "in" here to start expanded instead
            style = "margin-top: 1rem;",
            fluidRow(
              column(6, textInput(ns("kk"), "Krankenkasse")),
              column(6, textInput(ns("zv"), "Zusatzversicherung")),
              column(6, textInput(ns("vnr"), "Versicherungs-Nummer")),
              column(6, textInput(ns("adresse"), "Adresse")),
              column(6, textInput(ns("plz"), "PLZ/Ort")),
              column(6, textInput(ns("mail"), "E-Mail"))
            ),
          ),
          tags$hr(),
          tags$p(
            class = "pc-section-label", 
            tags$i(class = "ti ti-file-description"), "Kurs und Abo"
          ),
          selectInput(ns("course"), "Kurs", choices = NULL),
          fluidRow(
            column(6, selectInput(ns("abo"), "Abo", choices = choices_abos)),
            column(
              6,
              dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy")
            )
          ),
          div(
            style = "margin-top:10px",
            actionButton(
              ns("add"), 
              tagList(tags$i(class = "ti ti-user-plus"), " Hinzufügen"),
              class = "btn-primary btn-sm", 
              disabled = TRUE
            )
          )
        )
      ),
      
      # Right column: table + remove
      column(
        8,
        div(
          class = "pc-card",
          
          tags$p(
            class = "pc-section-label",
            tags$i(class = "ti ti-users"), "Teilnehmende verwalten"
          ),
          
          DTOutput(ns("members_table_edit")),
          
          div(
            style = "display:flex; justify-content:flex-end; margin-top:15px;",
            actionButton(
              ns("remove"),
              tagList(
                tags$i(class = "ti ti-trash"),
                " Entfernen"
              ),
              class = "btn-danger btn-sm"
            )
          )
        )
      )
    )
  )
}

mod_members_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # ns function in server -> needed for modals
    
    # check if courses update
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
    
    # check if inputs exist to enable button
    observe({
      if (nzchar(input$name) && nzchar(input$vorname) && length(input$course) > 0 && length(input$abo) > 0 && !is.null(input$abo_start)) {
        updateActionButton(session, "add", disabled = FALSE)
      } else {
        updateActionButton(session, "add", disabled = TRUE)
      }
    })
    
    # CREATE
    observeEvent(input$add, {
      
      # 1. insert member
      insert_member(con, input$kk, input$zv, input$vnr, input$vorname, input$name, input$adresse, input$plz, input$mail)
      
      global_refresh$members <- global_refresh$members + 1
      
      # 2. add member to course
      member <- get_member_vorname_name(con, input$vorname, input$name)
      
      insert_course_member(con, member$user_id, input$course)
      
      # 3. add abo for member
      add_abo(con, input$abo, member$user_id, input$abo_start)
      global_refresh$abos <- global_refresh$abos + 1
    })
    
    # DELETE
    show_delete_modal <- function() {
      showModal(
        modalDialog(
          title = "Teilnehmende entfernen",
          
          "Sind Sie sich sicher?",
          
          footer = tagList(
            modalButton("Abbrechen"), # this button closes the modal when pressed
            actionButton(
              ns("remove_yes"),
              "Entfernen"
            )
          )
        )
      )
    }
    
    observeEvent(input$remove, {
      # which are rows selected?
      selected <- input$members_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
      
      show_delete_modal()
    })
    
    members_data <- reactive({
      global_refresh$members
      df <- get_members(con)
      data <- df |> select(vorname, name, user_id)
      data # return
    })
    
    output$members_table_edit <- renderDT({
      data <- members_data()
      data_display <- data |> select(vorname, name)
      data_display <- data_display |> 
        rename(
          "Vorname" = vorname,
          "Name" = name
        )
      
      datatable(
        data_display,
        selection = "multiple",
        options = list(
          pageLength = 5,
          language = german_datatable(),
          dom = "ftp" # show only search, table, and pages
        )
      )
    })
    
    observeEvent(input$remove_yes, {
      
      # Get the selected row data
      members_remove <- members_data()[selected, ]
      
      for (i in 1:nrow(members_remove)) {
        delete_member_user_id(con, members_remove[i, ]$user_id)

        global_refresh$members <- global_refresh$members + 1
        global_refresh$abos <- global_refresh$abos + 1
      }
    })
    
  })
}