mod_abos_ui <- function(id) {
  ns <- NS(id)
  
  choices_abos <- setNames(
    c(10,3,6), # values (what server receives)
    c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo") # labels (what user sees)
  )
  
  tagList(
    h3("Abo-Preise"),
    
    hr(),
    
    h4("Abo-Preise anpassen"),
    
    selectInput(ns("course"), "Kurs wählen", choices = NULL),
    
    selectInput(ns("abo"), "Abo wählen", choices = choices_abos),
    
    textOutput(ns("old_price")),
    
    numericInput(ns("new_price"), "Neuer Abo-Preis", value = 300, min = 0, step = 10),
    
    actionButton(ns("update_price"), "Abo-Preis anpassen", disabled = TRUE)

  )
}


mod_abos_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
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

  })
}