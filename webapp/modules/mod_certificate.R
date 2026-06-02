library(shinyjs) # for disabling the button to prevent double rendering
library(quarto) # for rendering the pdf
library(qpdf) # for merging pdfs

mod_certificate_ui <- function(id, con) {
  ns <- NS(id)
  
  therapists <- get_therapists(con)
  choices_therapists <- setNames(
    therapists$user_id, # values (what server receives)
    therapists$vorname # labels (what user sees)
  )
  
  active_members <- get_members(con)
  choices_members <- setNames(
    active_members$user_id,  # values (what server receives)
    paste(active_members$vorname, active_members$name)  # labels (what user sees)
  )
  
  tagList(
    useShinyjs(),  # important
    
    h3("Zertifikate"),
    
    hr(),
    
    h4("Zertifikate erstellen"),
    
    selectInput(ns("therapist"), "Therapeut*in", choices = choices_therapists, multiple = FALSE),
    
    selectInput(ns("members"), "Teilnehmende", choices = choices_members, multiple = TRUE),
    
    actionButton(ns("make"), "Zertifikate erstellen", disabled = TRUE)
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
    
    # update choices in case new members get added
    observeEvent(global_refresh$members, {
      active_members <- get_members(con)
      choices_members <- setNames(
        active_members$user_id,  # values (what server receives)
        paste(active_members$vorname, active_members$name)  # labels (what user sees)
      )
      
      updateSelectInput(
        session,
        "members",
        choices = choices_members
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