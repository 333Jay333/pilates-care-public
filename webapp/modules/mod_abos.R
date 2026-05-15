mod_abos_ui <- function(id) {
  ns <- NS(id)
  
  choices_abos <- setNames(
    c(10,3,6), # values (what server receives)
    c("10er-Abo", "3-Monats-Abo", "6-Monats-Abo") # labels (what user sees)
  )
  
  tagList(
    h3("Abos verwalten"),
    
    h4("Abgelaufene Abos"),
    
    
    
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