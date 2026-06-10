mod_therapists_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left: add form
      column(
        4,
        div(
          class = "pc-card",
          tags$p(
            class = "pc-section-label",
            tags$i(class = "ti ti-user-plus"), " Therapeut*in hinzufügen"
          ),
          fluidRow(
            column(
              6,
              textInput(ns("vorname"), "Vorname")
            ),
            column(
              6, 
              textInput(ns("name"), "Name")
            )
          ),
          textInput(ns("praxis"), "Praxis"),
          textInput(ns("adresse"), "Adresse"),
          fluidRow(
            column(
              6,
              textInput(ns("plz"), "PLZ / Ort")
            ),
            column(
              6,
              textInput(ns("tel"), "Tel.")
            )
          ),
          textInput(ns("mail"), "E-Mail"),
          tags$hr(),
          tags$p(
            class = "pc-section-label",
            tags$i(class = "ti ti-id-badge"), " Nummern"
          ),
          fluidRow(
            column(
              6, 
              textInput(ns("zsr"), "ZSR-Nummer")
            ),
            column(
              6, textInput(ns("knr"), "K-Nummer")
            )
          ),
          fluidRow(
            column(
              6, textInput(ns("emfit"), "EMfit-Nummer")
            ),
            column(
              6, textInput(ns("pilat_nr"), "PilatesCare Mitglieder-Nr.")
            ),
          ),
          actionButton(
            ns("add"), 
            tagList(tags$i(class = "ti ti-user-plus"), " Hinzufügen"),
            class = "btn-primary",
            disabled = TRUE
          )
        )
      ),
      
      # Right: table + remove
      column(
        8,
        div(
          class = "pc-card",
          
          tags$p(
            class = "pc-section-label",
            tags$i(class = "ti ti-users"),
            " Therapeut*innen verwalten"
          ),
          
          DTOutput(ns("therapists_table_edit")),
          
          div(
            style = "display:flex; justify-content:flex-end; margin-top:15px;",
            actionButton(
              ns("remove"),
              tagList(
                tags$i(class = "ti ti-trash"),
                " Entfernen"
              ),
              class = "btn-danger"
            )
          )
        )
      )
    )
  )
}

mod_therapists_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # ns function in server -> needed for modals
    
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
    show_delete_modal <- function() {
      showModal(
        modalDialog(
          title = "Therapeut*innen entfernen",
          
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
      selected <- input$therapists_table_edit_rows_selected
      
      # Safety check
      if (length(selected) == 0) {
        showNotification("Bitte Teilnehmer*in auswählen", type = "warning")
        return()
      }
      
      show_delete_modal()
    })
    
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
        options = list(
          pageLength = 10,
          language = german_datatable(),
          dom = "tp"
        )
      )
    })
    
    observeEvent(input$remove_yes, {
      
      # close the previous modal
      removeModal()
      
      # which are rows selected?
      selected <- input$therapists_table_edit_rows_selected
      
      # Get the selected row data
      therapists_remove <- therapists_data()[selected, ]
      
      for (i in 1:nrow(therapists_remove)) {
        delete_therapist_user_id(con, therapists_remove[i, ]$user_id)
        
        global_refresh$therapists <- global_refresh$therapists + 1
      }
    })
  })
}