choices_abos <- setNames(
  c(10,3,6), # values (what server receives)
  c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo")
)

mod_members_ui <- function(id) {
  ns <- NS(id)
    
  tagList(
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    
    h3("Teilnehmende"),
    
    fluidRow(
      # Left column: add form
      column(4,
             div(class = "pc-card",
                 tags$p(class = "pc-section-label", "Teilnehmer*in hinzufügen"),
                 fluidRow(
                   column(6, textInput(ns("vorname"), "Vorname")),
                   column(6, textInput(ns("name"), "Name"))
                 ),
                 selectInput(ns("course"), "Kurs", choices = NULL),
                 fluidRow(
                   column(6, selectInput(ns("abo"), "Abo", choices = choices_abos)),
                   column(6, dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy"))
                 ),
                 tags$hr(),
                 tags$p(class = "pc-section-label", "Versicherung & Kontakt (optional)"),
                 fluidRow(
                   column(6, textInput(ns("kk"), "Krankenkasse")),
                   column(6, textInput(ns("zv"), "Zusatzversicherung")),
                   column(6, textInput(ns("vnr"), "Versicherungs-Nummer")),
                   column(6, textInput(ns("adresse"), "Adresse")),
                   column(6, textInput(ns("plz"), "PLZ/Ort")),
                   column(6, textInput(ns("mail"), "E-Mail"))
                 ),
                 actionButton(ns("add"), "Hinzufügen", class = "btn-primary btn-sm", disabled = TRUE)
             )
      ),
      
      # Right column: table + remove
      column(8,
             div(class = "pc-card",
                 tags$p(class = "pc-section-label", "Teilnehmende verwalten"),
                 DTOutput(ns("members_table_edit")),
                 actionButton(ns("remove"), "Entfernen", class = "btn-danger btn-sm")
             )
      )
    )
  )
}
  
#   tagList(
#     h3("Teilnehmende"),
#     
#     hr(),
#     
#     h4("Teilnehmer*in hinzufügen"),
#     
#     textInput(ns("name"), "Name"),
#     textInput(ns("vorname"), "Vorname"),
#     selectInput(ns("course"), "Zu Kurs hinzufügen", choices = choices_courses),
#     selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
#     dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy"),
#     textInput(ns("kk"), "Name der Krankenkasse (optional)"),
#     textInput(ns("zv"), "Zusatzversicherung (optional)"),
#     
#     
#     actionButton(ns("add"), "Teilnehmer*in hinzufügen", disabled = TRUE),
#     
#     hr(),
#     
#     h4("Teilnehmer*in entfernen"),
#     
#     DTOutput(ns("members_table_edit")),
#     
#     actionButton(ns("remove"), "Teilnehmer*in entfernen", disabled = FALSE),
#   )
# }


mod_members_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
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
        options = list(pageLength = 10)
      )
    })
    
    observeEvent(input$remove, {
      # which are rows selected?
      selected <- input$members_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
      
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