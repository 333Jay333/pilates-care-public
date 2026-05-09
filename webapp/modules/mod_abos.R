mod_abos_ui <- function(id) {
  ns <- NS(id)
  
  choices_still_left <- setNames(
    c(3:1), # what server receives
    c("3 oder weniger", "2 oder weniger", "1 oder weniger") # what user sees
  )
  
  choices_weeks_still_left <- setNames(
    c(6:1), # what server receives
    c(paste(rep(6:1), "oder weniger")) # what user sees
  )
  
  tagList(
    h3("10er Abos"),
    
    hr(),
    
    selectInput(ns("still_left"), "Wie viele Termine noch übrig?", choices = choices_still_left, selected = 3),
    
    dataTableOutput(ns("abo_10_soon_done")),
    
    hr(),
    
    h3("3-Monats-Abo und 6-Monats-Abo"),
    
    selectInput(ns("weeks_still_left"), "Wie viele Wochen noch übrig?", choices = choices_weeks_still_left, selected = 6),
    
    dataTableOutput(ns("abo_month_soon_done"))
  )
}


mod_abos_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    data_abo_10 <- reactive({
      req(input$still_left)
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      members <- get_members_abo_10(con)
      attended <- members |> select(user_id, vorname, name)
      attended$n <- 0
      
      for (i in 1:nrow(members)) {
        n_attended <- nrow(get_attendance_user_id(con, members[i,1]))
        attended[i,ncol(attended)] <- n_attended # last column is for storing number of courses attended
      }
      
      # delete duplicate rows from attended
      attended <- distinct(attended)
      
      # add still_left column to attended
      attended <- attended |> mutate(
        still_left = 10 - n
      )
      
      # filter according to user input how many should be left
      attended <- attended |> filter(still_left <= input$still_left) 
      
      # make a nice version of the df for the ui
      attended_display <- attended |> 
        select(name, vorname, still_left) |> 
        arrange(still_left) |> 
        rename(
          "Name" = name,
          "Vorname" = vorname,
          "Übrige Termine" = still_left
        )
      
      attended_display # return nice version
    })
    
    output$abo_10_soon_done <- renderDT({
      req(input$still_left)
      
      data_abo_10()
    })
    
    data_abo_month <- reactive({
      req(input$weeks_still_left)
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      
      members <- get_members_abo_month(con)
      members$abo_end <- as.Date(members$abo_end)
      
      still_left <- today() + 7 * as.integer(input$weeks_still_left)
      
      members_filtered <- members |> 
        filter(abo_end <= still_left) |> 
        arrange(abo_end) |> 
        mutate(abo_end = format(abo_end, "%d.%m.%Y")) |> 
        select(vorname, name, abo_end) |> 
        rename(
          "Vorname" = vorname,
          "Name" = name,
          "Abo gültig bis" = abo_end
        )
      
      members_filtered # return
    })
    
    output$abo_month_soon_done <- renderDT({
      req(input$weeks_still_left)
      
      data_abo_month()
    })
  })
}