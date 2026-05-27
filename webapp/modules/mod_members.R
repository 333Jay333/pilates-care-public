mod_members_ui <- function(id, con) {
  ns <- NS(id)
  
  courses <- get_courses(con)
  choices_courses <- setNames(
    courses$course_id, # values (what server receives)
    courses$kursname # labels (what user sees)
  )
  
  choices_abos <- setNames(
    c(10,3,6), # values (what server receives)
    c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo")
  )
  
  tagList(
    h3("Teilnehmende"),
    
    hr(),
    
    h4("Teilnehmer*in hinzufügen"),
    
    textInput(ns("name"), "Name"),
    textInput(ns("vorname"), "Vorname"),
    selectInput(ns("course"), "Zu Kurs hinzufügen", choices = choices_courses),
    selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
    dateInput(ns("abo_start"), "Abo-Beginn", format = "dd.mm.yyyy"),
    textInput(ns("kk"), "Name der Krankenkasse (optional)"),
    textInput(ns("zv"), "Zusatzversicherung (optional)"),
    textInput(ns("vnr"), "Versicherungs-Nummer (optional)"),
    textInput(ns("adresse"), "Adresse (optional)"),
    textInput(ns("plz"), "PLZ/Ort (optional)"),
    textInput(ns("mail"), "E-Mail (optional)"),
    
    actionButton(ns("add"), "Teilnehmer*in hinzufügen", disabled = TRUE),
    
    hr(),
    
    h4("Teilnehmer*in entfernen"),
    
    DTOutput(ns("members_table_edit")),
    
    actionButton(ns("remove"), "Teilnehmer*in entfernen", disabled = FALSE),
  )
}


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
      member_user_id <- member$user_id
      
      insert_course_member(con, member_user_id, input$course)
      
      # 3. add abo for member
      if (input$abo == 10) {
        insert_abo(con, member_user_id, input$abo, input$abo_start)
      } else { # calculate abo end automatically for 3 and 6 month abo
        if (input$abo == 3) {
          abo_end <- input$abo_start + 91 
        } else {
          abo_end <- input$abo_start + 183
        }
        insert_abo_end(con, member_user_id, input$abo, input$abo_start, abo_end)
      }
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