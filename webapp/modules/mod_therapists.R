mod_therapists_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Therapeut*innen"),
    
    hr(),
    
    h4("Therapeut*in hinzufügen"),
    
    textInput(ns("vorname"), "Vorname"),
    textInput(ns("name"), "Name"),
    textInput(ns("praxis"), "Praxis"),
    textInput(ns("adresse"), "Adresse"),
    textInput(ns("plz"), "PLZ/Ort"),
    textInput(ns("tel"), "Tel."),
    textInput(ns("mail"), "E-Mail"),
    textInput(ns("zsr"), "ZSR-Nummer"),
    textInput(ns("knr"), "K-Nummer"),
    textInput(ns("emfit"), "EMfit-Nummer"),
    textInput(ns("pilat_nr"), "PilatesCare Mitglieder-Nr."),
    
    actionButton(ns("add"), "Therapeut*in hinzufügen", disabled = TRUE),
    
    hr(),
    
    h4("Therapeut*in entfernen"),
    
    DTOutput(ns("therapists_table_edit")),
    
    actionButton(ns("remove"), "Therapeut*in entfernen", disabled = FALSE)
  )
}


mod_therapists_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    # check if inputs exist to enable button
    observe({
      if (nzchar(input$vorname) && nzchar(input$name) && nzchar(input$praxis) && nzchar(input$adresse) && nzchar(input$plz) && nzchar(input$tel) && nzchar(input$mail) && nzchar(input$zsr) && nzchar(input$knr) && nzchar(input$emfit) && nzchar(input$pilat_nr)) {
        updateActionButton(session, "add", disabled = FALSE)
      } else {
        updateActionButton(session, "add", disabled = TRUE)
      }
    })
    
    # CREATE
    observeEvent(input$add, {
      insert_therapist(con, input$vorname, input$name, input$praxis, input$adresse, input$plz, input$tel, input$mail, input$zsr, input$knr, input$emfit, input$pilat_nr)
      
      global_refresh$therapists <- global_refresh$therapists + 1
    })
    
    # DELETE
    therapists_data <- reactive({
      global_refresh$therapists # don't forget this or it won't be reactive
      df <- get_therapists(con)
      data <- df |> select(vorname, name, user_id)
      data # return
    })
    
    output$therapists_table_edit <- renderDT({
      data <- therapists_data()
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
      selected <- input$therapists_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Therapeut*in auswählen", type = "warning")
        return()
      }
      
      # Get the selected row data
      therapists_remove <- therapists_data()[selected, ]
      
      for (i in 1:nrow(therapists_remove)) {
        delete_therapist_user_id(con, therapists_remove[i, ]$user_id)
        
        global_refresh$therapists <- global_refresh$therapists + 1
      }
    })
  })
}