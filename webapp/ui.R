navbarPage(
  title = "PilatesCare",
  
  # enable icons
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/tabler-icons.min.css"
    )
  ),
  
  tabPanel(
    title = "Anwesenheit",
    
    mod_attendance_ui("attendance", db)
  ),
  
  tabPanel(
    title = "Abo-Dashboard",
    
    mod_abo_dashboard_ui("abo_dash")
  ),
  
  tabPanel(
    title = "Abos verwalten",
    
    mod_abos_ui("abos")
  ),
  
  tabPanel(
    title = "Teilnehmende",
    
    mod_members_ui("members")
  ),
  
  tabPanel(
    title = "Zertifikate",
    
    # this will be shown during rendering to let the user know that something is happening
    tags$style(HTML("
      .shiny-busy::after {
      content: '⏳ Wird verarbeitet...';
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      background: white;
      padding: 20px;
      border: 1px solid #ccc;
      z-index: 9999;
      }")
    ),
    
    mod_certificate_ui("certificates", db)
  ),
  
  tabPanel(
    title = "Termine & Kurse",
    
    mod_courses_ui("courses", db)
  ),
  
  tabPanel(
    title = "Therapeut*innen",
    
    mod_therapists_ui("therapists")
  )
  
)