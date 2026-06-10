choices_day <- setNames(
  c(1:7), # values (what server receives)
  c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
)

mod_courses_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(
      tabPanel(
        title = "Kurse",
        
        tagList(
          tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
          
          div(style = "margin-top: 2rem;"), # top margin
          fluidRow(
            column(
              4,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-users-group"), "Kurs hinzufügen"
                ),
                textInput(ns("kursname"), "Kursname"),
                textInput(ns("location"), "Ort"),
                selectInput(ns("day"), "Wochentag", choices = choices_day),
                tags$hr(),
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-pig-money"), "Preise"
                ),
                numericInput(ns("price_abo_10"), "Preis 10er-Abo", value = 300, step = 10),
                numericInput(ns("price_abo_3"), "Preis 3-Monats-Abo", value = 350, step = 10),
                numericInput(ns("price_abo_6"), "Preis 6-Monats-Abo", value = 700, step = 10),
                actionButton(
                  ns("add"), 
                  tagList(tags$i(class = "ti ti-user-plus"), " Hinzufügen"),
                  class = "btn-primary",
                  disabled = TRUE
                )
              )
            ),
            column(
              8,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-users-group"), "Kurs entfernen"
                ),
                DTOutput(ns("courses_table_edit")),
                div(
                  style = "display:flex; justify-content:flex-end; margin-top:15px;",
                  actionButton(
                    ns("remove"), 
                    tagList(
                      tags$i(class = "ti ti-trash"),
                      " Entfernen"
                    ),
                    disabled = FALSE
                  )
                )
              )
            )
          )
        )
      ),
      
      tabPanel(
        title = "Termine",
        
        tagList(
          tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
          
          div(style = "margin-top: 2rem;"), # top margin
          fluidRow(
            column(
              12,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-users-group"), "Kurs wählen"
                ),
                selectInput(ns("course"), "Kurs wählen", choices = NULL)
              )
            )
          ),
          fluidRow(
            column(
              4,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-calendar-event"), "Termine hinzufügen"
                ),
                dateInput(ns("course_dates_start"), "Ab welchem Tag sollen Termine hinzugefügt werden?", format = "dd.mm.yyyy"),
                
                numericInput(ns("months_to_add"), "Für wie viele Monate sollen Termine hinzugefügt werden?", 12, min = 1, max = 24),
                
                actionButton(
                  ns("course_dates_add"), 
                  tagList(tags$i(class = "ti ti-plus"), "Termine hinzufügen"),
                  class = "btn-primary",
                  disabled = FALSE
                )
              )
            ),
            column(
              8,
              div(
                class = "pc-card",
                tags$p(
                  class = "pc-section-label", 
                  tags$i(class = "ti ti-calendar-event"), "Termine entfernen"
                ),
                DTOutput(ns("course_dates_table_edit")),
                actionButton(
                  ns("course_dates_remove"), 
                  "Termine entfernen", 
                  disabled = FALSE
                )
              )
            )
          )
        )
      )
    )
  )
}


mod_courses_server <- function(id, con, global_refresh) {
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
    
    # update dateInput value based on group
    observeEvent(input$course, {
      req(input$course)
      df.last_course_date <- get_last_course_date_course_id(con, input$course)
      if (nrow(df.last_course_date) == 0) {
        last_course_date <- today()
      } else {
        last_course_date <- as.Date(df.last_course_date$course_date, origin = "1970-01-01") + 7 # dates from SQL come in a special format (days since 1970-01-01) -> need to be converted
      }
      
      updateDateInput(
        session,
        "course_dates_start",
        value = last_course_date
      )
    })
    
    # CREATE course_dates
    observeEvent(input$course_dates_add, {
      req(input$course, input$months_to_add, input$course_dates_start)
      
      date_to_add <- input$course_dates_start
      
      if (input$months_to_add == 12) {
        for (i in 0:51) { # 52 weeks in case 12 months are selected
          insert_course_date(con, input$course, (date_to_add + i*7))
        }
      } else { # month * 4 in case something else is selected
        for (i in 0:(3*input$months_to_add)) {
          insert_course_date(con, input$course, (date_to_add + i*7))
        }
      }
      
      global_refresh$courses <- global_refresh$courses + 1
    })
    
    # DELETE course_dates
    course_dates_data <- reactive({
      req(input$course)
      global_refresh$courses # don't forget this or it won't be reactive
      df <- get_course_dates_course_id(con, input$course)
      data <- df |> select(course_date, course_date_id)
      data$course_date <- format_swiss_date_with_origin(data$course_date) # make dates for table readable
      data # return
    })
    
    output$course_dates_table_edit <- renderDT({
      data <- course_dates_data()
      data_display <- data |> select(course_date)
      data_display <- data_display |> 
        rename(
          "Termin" = course_date
        )
      
      datatable(
        data_display,
        selection = "multiple",
        options = list(
          pageLength = 10,
          language = german_datatable()
        )
      )
    })
    
    observeEvent(input$course_dates_remove, {
      # which are rows selected?
      selected <- input$course_dates_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Termin auswählen", type = "warning")
        return()
      }
      
      # Get the selected row data
      course_dates_remove <- course_dates_data()[selected, ]
      
      for (i in 1:nrow(course_dates_remove)) {
        delete_course_date_id(con, course_dates_remove[i, ]$course_date_id)
        
        global_refresh$courses <- global_refresh$courses + 1
      }
    })
    
    # check if inputs exist to enable button course add
    observe({
      if (nzchar(input$kursname) && length(input$location) > 0) {
        updateActionButton(session, "add", disabled = FALSE)
      } else {
        updateActionButton(session, "add", disabled = TRUE)
      }
    })

    # CREATE course
    observeEvent(input$add, {
      insert_course(con, input$kursname, input$location)
      
      course_id <- get_course_id_kursname(con, input$kursname)$course_id
      
      # insert prices for this course
      insert_abo_price(con, course_id, 10, input$price_abo_10) # 10er Abo
      insert_abo_price(con, course_id, 3, input$price_abo_3) # 3-Monats-Abo
      insert_abo_price(con, course_id, 6, input$price_abo_6) # 6-Monats-Abo

      global_refresh$courses <- global_refresh$courses + 1
    })

    # DELETE course
    courses_data <- reactive({
      global_refresh$courses # don't forget this or it won't be reactive
      df <- get_courses(con)
      data <- df |> select(kursname, location, course_id)
      data # return
    })

    output$courses_table_edit <- renderDT({
      data <- courses_data()
      data_display <- data |> select(kursname, location)
      data_display <- data_display |>
        rename(
          "Kurs" = kursname,
          "Ort" = location
        )
      
      datatable(
        data_display,
        selection = "multiple",
        options = list(
          pageLength = 10,
          language = german_datatable(),
          dom = "tp"
        )
      )
    })

    observeEvent(input$remove, {
      # which are rows selected?
      selected <- input$courses_table_edit_rows_selected

      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Kurs auswählen", type = "warning")
        return()
      }

      # Get the selected row data
      courses_remove <- courses_data()[selected, ]

      for (i in 1:nrow(courses_remove)) {
        delete_course_id(con, courses_remove[i, ]$course_id)

        global_refresh$courses <- global_refresh$courses + 1
      }
    })
  })
}