function(input, output, session) {
  
  # global reactive trigger
  global_refresh <- reactiveValues(
    therapists = 0,
    members = 0,
    courses = 0,
    attendance = 0,
    abos = 0,
    abo_price = 0
  )
  
  # modules
  mod_therapists_server("therapists", db, global_refresh) # id needs to match the id passed to the ui
  
  mod_members_server("members", db, global_refresh)
  
  mod_certificate_server("certificates", db, global_refresh)
  
  mod_courses_server("courses", db, global_refresh)
  
  mod_attendance_server("attendance", db, global_refresh)
  
  mod_abo_dashboard_server("abo_dash", db, global_refresh)
  
  mod_abos_server("abos", db, global_refresh)
  
  # quit app
  observeEvent(input$quit_app, {
    showModal(modalDialog(
      title = "App beenden",
      "Möchten Sie die App wirklich beenden?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("quit_confirm", "Beenden", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$quit_confirm, {
    removeModal()
    shiny::stopApp()
  })
}
