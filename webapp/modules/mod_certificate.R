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
                  ns("certificate_make"),
                  tagList(tags$i(class = "ti ti-file-certificate"), "Zertifikat erstellen"),
                  class = "btn-primary", 
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
    
    
    # make certificates
    observeEvent(input$make, {
      req(input$therapist)
      
      # ---- disable button immediately ----
      shinyjs::disable("make")
      
      # ensure re-enable of the button no matter what happens
      on.exit(shinyjs::enable("make"), add = TRUE)
      
      therapist <- get_therapist_user_id(con, input$therapist)
      
      file_path_template <- here("template", "pdf-output.qmd")
      time <- str_replace(substring(now(), first = 12, last = 16), pattern = ":", replacement = "_")
      file_path_output <- here("Zertifikate", paste(format(today(), "%d_%m_%Y"), time, sep = "_"))
      
      dir.create(file_path_output, showWarnings = FALSE)
      
      withProgress(message = "Zertifikate werden erstellt...\n", value = 0, {
        n <- length(input$members)

        for (i in seq_along(input$members)) {

          member <- input$members[i]
          member_data <- get_member_user_id(con, member)
          abo_data <- get_abo_user_id(con, member) # THIS WILL NEED TO BE CHANGED NOW THAT A USER CAN HAVE MULTIPLE ABOS
          course_membership <- get_course_membership_user_id(con, member)$course_id
          abo_price <- get_abo_price(con, course_membership, abo_data$abo_type)

          incProgress(1/n, detail = paste("Erstelle Zertifikat für", paste(member_data$vorname, member_data$name)))
          
          quarto_render(
            input = file_path_template,
            output_format = "pdf",
            output_file = paste0("Zert_", member_data$vorname, "_", member_data$name, ".pdf"),
            execute_params = list(
              pt_vorname = therapist$vorname,
                  pt_name = therapist$name,
                  pt_praxis = therapist$praxis,
                  pt_adresse = therapist$adresse,
                  pt_plz = therapist$plz,
                  pt_tel = therapist$tel,
                  pt_mail = therapist$mail,
                  pt_zsr = therapist$zsr,
                  pt_knr = therapist$knr,
                  pt_emfit = therapist$emfit,
                  pt_pilat_nr = therapist$pilat_nr,
                  abo_type = abo_data$abo_type,
                  abo_start = format_swiss_date(abo_data$abo_start),
                  abo_end = format_swiss_date(abo_data$abo_end), # need to adjust this for abo 10
                  abo_price = abo_price,
                  mem_kk = member_data$kk,
                  mem_zv = member_data$zv,
                  mem_vnr = member_data$vnr,
                  mem_name = member_data$name,
                  mem_vorname = member_data$vorname,
                  mem_adresse = member_data$adresse,
                  mem_plz = member_data$plz,
                  mem_mail = member_data$mail
            ),
            quarto_args = c("--output-dir", file_path_output) # pass output directory
          )
        }
        
        # merge all pdfs in folder
        if (n > 1) {
          files <- list.files(file_path_output, pattern = "\\.pdf$", full.names = TRUE)
          
          pdf_combine(
            input = files,
            output = paste(file_path_output, "alle_Zertifikate.pdf", sep = "/")
          )
        }
      })
    })
    
    
    # check if inputs exist to enable button
    observe({
      if (!is.null(input$therapist) &&
          nzchar(input$therapist) &&
          length(input$members) > 0) {
        updateActionButton(session, "make", disabled = FALSE)
      } else {
        updateActionButton(session, "make", disabled = TRUE)
      }
    })
    
  })
}