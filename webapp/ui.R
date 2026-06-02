page_navbar(
  title = "PilatesCare",
  
  nav_panel(
    title = "Anwesenheit",
    
    mod_attendance_ui("attendance", db)
  ),
  
  nav_panel(
    title = "Abo-Dashboard",
    
    mod_abo_dashboard_ui("abo_dash")
  ),
  
  nav_panel(
    title = "Abos verwalten",
    
    mod_abos_ui("abos")
  ),
  
  nav_panel(
    title = "Teilnehmende",
    
    mod_members_ui("members", db)
  ),
  
  nav_panel(
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
  
  nav_panel(
    title = "Termine & Kurse",
    
    mod_courses_ui("courses", db)
  ),
  
  nav_panel(
    title = "Therapeut*innen",
    
    mod_therapists_ui("therapists")
  )
  
)