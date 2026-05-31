library(shinyjs) # for showing/hiding elements

mod_attendance_ui <- function(id, con) {
  ns <- NS(id)
  
  courses <- get_courses(con)
  choices_courses <- setNames(
    courses$course_id, # values (what server receives)
    courses$kursname # labels (what user sees)
  )
  
  tagList(
    
    useShinyjs(), # important for showing/hiding ui elements
    
    h3("Anwesenheit"),
    
    hr(),
    
    h4("Anwesenheit hinzufügen"),
    
    selectInput(ns("course"), "Kurs wählen", choices = choices_courses),
    
    fluidRow(
      column(3, actionButton(ns("last_month"), "Termine letzter Monat")),
      column(3, actionButton(ns("last_three_months"), "Termine letzte 3 Monate")),
      column(3, actionButton(ns("specific_date"), "Termin manuell auswählen"))
    ),
    
    br(),
    
    hidden(
      div(
        id = ns("normal_date_ui"),
        selectInput(ns("course_date"), "Termin wählen", choices = NULL)
      )
    ),
    
    hidden(
      div( # needs to be put inside a container to give it an id
        id = ns("specific_date_ui"),
        tagList(
          dateInput(ns("course_date_specific"), "Termin wählen", format = "dd.mm.yyyy"),
          fluidRow(
            column(2, actionButton(ns("minus_one"), "1 Woche vorher")),
            column(2, actionButton(ns("plus_one"), "1 Woche nachher"))
          ),
          br()
        )
      )
    ),
    
    hidden(
      actionButton(ns("load"), "Anwesenheit für dieses Datum erfassen", disabled = TRUE)
    ),
      
    br(),
    br(),
    
    checkboxGroupInput(ns("members"), "", choices = NULL),

    hidden(
      actionButton(ns("add"), "Answesenheit erfassen", disabled = TRUE)
    ),
    
    hr(),
    
    h4("Anwesenheit entfernen"),
    
    DTOutput(ns("attendance_table_edit")),
    
    actionButton(ns("remove"), "Anwesenheit entfernen", disabled = FALSE)
  )
}


mod_attendance_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    # value to store if specific_course_date is used
    specific <- reactiveVal(FALSE)
    
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
    
    # NORMAL COURSE_DATE UI
    # show the normal ui for course dates
    observeEvent(input$last_month, {
      hide("specific_date_ui")
      show("normal_date_ui")
      show("load")
      
      specific(FALSE)
      
      req(input$course)
      choices_server <- get_course_dates_course_id_after_date(con, input$course, today()-31)$course_date
      choices_names <- format_swiss_date_with_origin(choices_server)
      choices <- setNames(
        choices_server,
        choices_names
      )
      
      updateSelectInput(
        session,
        "course_date",
        choices = choices
      )
      
      # safety check
      if (length(choices) == 0) {
        updateActionButton(session, "load", disabled = TRUE) # was not possible to do my normal observe -> enable/disable, so this is my workaround
        showNotification("Kurs hat keine Termine in gewähltem Zeitraum", type = "warning")
        return()
      } else {
        updateActionButton(session, "load", disabled = FALSE)
      }
    })
    
    observeEvent(input$last_three_months, {
      hide("specific_date_ui")
      show("normal_date_ui")
      show("load")
      
      specific(FALSE)
      
      req(input$course)
      choices_server <- get_course_dates_course_id_after_date(con, input$course, today()-3*31)$course_date
      choices_names <- format_swiss_date_with_origin(choices_server)
      choices <- setNames(
        choices_server,
        choices_names
      )
      
      updateSelectInput(
        session,
        "course_date",
        choices = choices
      )
      
      if (length(choices) == 0) {
        updateActionButton(session, "load", disabled = TRUE) # was not possible to do my normal observe -> enable/disable, so this is my workaround
        showNotification("Kurs hat keine Termine in gewähltem Zeitraum", type = "warning")
        return()
      } else {
        updateActionButton(session, "load", disabled = FALSE)
      }
    })
    
    # SPECIFIC_DATE UI
    # in case user needs a specific date, show the specific_date ui
    observeEvent(input$specific_date, {
      hide("normal_date_ui")
      show("specific_date_ui")
      show("load")
      
      specific(TRUE)
      
      updateActionButton(session, "load", disabled = FALSE)
    })
    
    # update course_date based on buttons
    observeEvent(input$minus_one, {
      req(input$course_date_specific)
      
      new_course_date <- input$course_date_specific - 7
      
      updateDateInput(
        session,
        "course_date_specific",
        value = new_course_date
      )
    })
    
    observeEvent(input$plus_one, {
      req(input$course_date_specific)
      
      new_course_date <- input$course_date_specific + 7
      
      updateDateInput(
        session,
        "course_date_specific",
        value = new_course_date
      )
    })
    
    # show checkboxes when ready
    observeEvent(input$load, {
      req(input$course)
      choices <- get_course_members_course_id(con, input$course)$user_id
      
      # Safety check
      if (length(choices) == 0) {
        updateCheckboxGroupInput(
          session,
          "members",
          label = "",
          choices = character(0),
          selected = character(0)
        )
        showNotification("Kurs hat keine Teilnehmende", type = "warning")
        return()
      }
      
      # get the names of course members
      choices_names <- character(length(choices)) # good practice: set final length of vector already -> see https://bookdown.org/csgillespie/efficientR/programming.html
      for (i in 1:length(choices)) {
        member <- get_member_user_id(con, choices[i])
        choices_names[i] <- paste(member$vorname, member$name)
      }
      
      choices_checkbox <- setNames(
        choices, # values (what server receives)
        choices_names # labels (what user sees)
      )
      
      # show label & set choices
      updateCheckboxGroupInput(
        session,
        "members",
        label = "Teilnehmende",
        choices = choices_checkbox
      )
      
      # show add button
      show("add")
    })
    
    # check if inputs from checkboxes exist to enable button add
    observe({
      valid_course <- !is.null(input$course) && nzchar(input$course)
      valid_members <- length(input$members) > 0
      
      if (valid_course && valid_members) {
        updateActionButton(session, "add", disabled = FALSE)
      } else {
        updateActionButton(session, "add", disabled = TRUE)
      }
    })
    
    # CREATE
    observeEvent(input$add, {
      req(input$course, input$members) # really important here because I couldn't do the observe correctly
      course_date_to_add <- integer(1)
      
      if (specific()) {
        req(input$course_date_specific)
        possible_dates <- get_course_dates_course_id(con, input$course)$course_date
        selected_date <- as.integer(input$course_date_specific)
        
        # safety check if selected date is not a possible date
        if (!(selected_date %in% possible_dates)) {
          showNotification("Kurs hat keinen Termin an gewähltem Datum", type = "warning")
          return()
        }
        
        course_date_to_add <- selected_date
      } else {
        course_date_to_add <- as.integer(input$course_date)
      }
      
      course_date_id <- get_course_date_id_course_id_course_date(con, input$course, course_date_to_add)$course_date_id
      
      # insert attendance
      count <- 0
      for (member in input$members) {
        active_abo <- get_active_abo_user_id(con, member)
        
        # safety check if attendance can be added for this abo
        # STILL NEEDS TO BE IMPLEMENTED
        
        insert_attendance(con, course_date_id = course_date_id, user_id = member, abo_id = active_abo$abo_id)
        count <- count + 1
        global_refresh$attendance <- global_refresh$attendance + 1
        global_refresh$abos <- global_refresh$abos + 1
      }
      # show notification
      if (count == 1) {
        showNotification("Anwesenheit für 1 Person hinzugefügt", type = "message")
      } else {
        showNotification(paste("Anwesenheit für", count, "Personen hinzugefügt"), type = "message")
      }
    })
    
    # DELETE
    attendance_data <- reactive({
      req(input$course)
      global_refresh$attendance # don't forget this or it won't be reactive
      data <- get_attendance_course_id(con, input$course)
      data$course_date <- format_swiss_date_with_origin(data$course_date) # make dates for table readable
      data # return
    })
    
    output$attendance_table_edit <- renderDT({
      data <- attendance_data()
      data_display <- data |> select(course_date, vorname, name)
      data_display <- data_display |> 
        rename(
          "Termin" = course_date,
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
      selected <- input$attendance_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Anwesenheit auswählen", type = "warning")
        return()
      }
      
      # Get the selected row data
      attendance_remove <- attendance_data()[selected, ]
      
      for (i in 1:nrow(attendance_remove)) {
        delete_attendance(con, attendance_remove[i, ]$course_date_id, attendance_remove[i, ]$user_id)
        
        global_refresh$attendance <- global_refresh$attendance + 1
        global_refresh$abos <- global_refresh$abos + 1
      }
    })

  })
}