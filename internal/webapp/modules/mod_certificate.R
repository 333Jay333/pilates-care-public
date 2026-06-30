library(shinyjs) # for disabling the button to prevent double rendering
library(quarto) # for rendering the pdf
library(qpdf) # for merging pdfs

mod_certificate_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),  # important
    
    tabPanel(
      title = "Zertifikat erstellen",
      
      tagList(
        tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
        
        fluidRow(
          column(
            12,
            
            # member list
            div(
              class = "pc-card",
              tags$p(
                class = "pc-section-label", 
                tags$i(class = "ti ti-file-certificate"), "Für welche Teilnehmer*in soll ein Zertifikat erstellt werden?"
              ),
              DTOutput(ns("members_table"))
            ),
            
            # abo list
            div(
              class = "pc-card",
              tags$p(
                class = "pc-section-label", 
                tags$i(class = "ti ti-file-certificate"), "Für welches Abo soll ein Zertifikat erstellt werden?"
              ),
              DTOutput(ns("abos_table"))
            ),
            
            # therapist and make certificate
            div(
              class = "pc-card",
              tags$p(
                class = "pc-section-label", 
                tags$i(class = "ti ti-file-certificate"), "Zertifikat erstellen"
              ),
              selectInput(ns("therapist"), "Therapeut*in", choices = NULL, multiple = FALSE),
              div(
                style = "margin-top:5px",
                actionButton(
                  ns("add_certificate"),
                  tagList(tags$i(class = "ti ti-file-certificate"), "Zertifikat erstellen"),
                  class = "btn-primary", 
                  disabled = FALSE
                )
              ),
              tags$hr(),
              h5(strong("Für folgende Personen wird ein Zertifikat generiert:")),
              tableOutput(ns("certificates_to_generate")),
              actionButton(
                ns("make_certificates"), 
                tagList(tags$i(class = "ti ti-file-plus"), "Zertifikate erstellen"), 
                class = "btn-primary", 
                disabled = TRUE
              )
            )
          )
        )
      )
    )
  )
}


mod_certificate_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # members table
    members_data <- reactive({
      global_refresh$members
      df <- get_members(con)
      data <- df |> select(vorname, name, user_id)
      data # return
    })
    
    output$members_table <- renderDT({
      data <- members_data()
      data_display <- data |> select(vorname, name)
      data_display <- data_display |> 
        rename(
          "Vorname" = vorname,
          "Name" = name
        )
      
      datatable(
        data_display,
        selection = "single",
        options = list(
          pageLength = 5,
          language = german_datatable()
        )
      )
    })
    
    # abos table
    abos_data <- reactive({
      global_refresh$abos
      
      selected <- input$members_table_rows_selected
      
      # Get the selected row data
      member_user_id <- members_data()[selected, ]$user_id
      
      # get abos for this member
      data <- get_abo_user_id(con, member_user_id)
      
      # sort descending according to abo_start
      # IMPORTANT: this needs to happen before the data_display, otherwise what the user sees doesn't match what gets selected
      data <- data |> arrange(desc(abo_start)) 
      
      # return
      data
    })
    
    output$abos_table <- renderDT({
      data <- abos_data()
      
      data_display <- data |> 
        select(abo_type, abo_start, abo_end, abo_status) |> 
        mutate(
          abo_type = abo_type |>
            recode_values(
              10 ~ "10er Abo",
              3 ~ "3-Monats Abo",
              6 ~ "6-Monats Abo"
            )
        ) |> # make the badge for abo_type
        mutate(
          abo_type = sapply(abo_type, abo_badge)
        ) |> 
        mutate(
          abo_start = format_swiss_date_with_origin(abo_start),
          abo_end = format_swiss_date_with_origin(abo_end)
        ) |> 
        mutate(
          abo_status = abo_status |> 
            recode_values(
              "active" ~ "Aktiv",
              "archived" ~ "Archiviert"
            )
        ) |> 
        mutate(
          abo_status = sapply(abo_status, abo_status_badge)
        ) |> 
        rename(
          "Abo Typ" = abo_type,
          "Abo Start" = abo_start,
          "Abo Ende" = abo_end,
          "Abo Status" = abo_status
        )
      
      datatable(
        data_display,
        selection = "single",
        escape = FALSE, # enable HTML rendering
        options = list(
          pageLength = 5,
          language = german_datatable()
        )
      )
    })
    
    # variable for storing members for which certificate shall be generated
    certificate_list <- reactiveVal(
      data.frame(
        member_user_id = integer(),
        abo_id = integer(),
        vorname = character(),
        name = character(),
        therapist_user_id = integer()
      )
    )
    
    # add certificate to list
    observeEvent(input$add_certificate, {
      req(input$therapist)
      
      # get the selected row from abo list
      selected_abo <- input$abos_table_rows_selected
      
      # Safety check
      if (length(selected_abo) == 0) {
        showNotification("Bitte Teilnehmer*in und/oder Abo auswählen", type = "warning")
        return()
      }
      
      # Get the selected row data
      selected_abo_data <- abos_data()[selected_abo, ]
      selected_abo_data_abo_id <- selected_abo_data$abo_id
      selected_abo_data_user_id <- selected_abo_data$user_id
      member <- get_member_user_id(con, selected_abo_data_user_id)
      
      # save in certificate list
      certificate_list(
        rbind(
          certificate_list(),
          data.frame(
            member_user_id = selected_abo_data_user_id,
            abo_id = selected_abo_data_abo_id,
            vorname = member$vorname,
            name = member$name,
            therapist_user_id = input$therapist
          )
        )
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
    
    # make certificates
    observeEvent(input$make_certificates, {
      # ---- disable button immediately ----
      shinyjs::disable("make_certificates")
      
      # ensure re-enable of the button no matter what happens
      on.exit(shinyjs::enable("make_certificates"), add = TRUE)
      
      # make certificates
      make_certificates(
        con,
        therapist_user_ids = certificate_list()$therapist_user_id,
        members_user_ids = certificate_list()$member_user_id,
        abo_id = certificate_list()$abo_id
      )
    })
  })
}