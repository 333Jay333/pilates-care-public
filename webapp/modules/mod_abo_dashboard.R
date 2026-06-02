choices_still_left <- setNames(
  c(3:1), # what server receives
  c("3 oder weniger", "2 oder weniger", "1 oder weniger") # what user sees
)

choices_weeks_still_left <- setNames(
  c(6:1), # what server receives
  c(paste(rep(6:1), "oder weniger")) # what user sees
)

mod_abo_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    
    fluidRow(
      # Left column
      column(
        6,
        div(class = "pc-card",
        
          tags$p(
            class = "pc-section-label", 
            tags$i(class = "ti ti-clipboard-text"), "10er Abos"
          ),
          selectInput(ns("still_left"), "Wie viele Termine noch übrig?", choices = choices_still_left, selected = 3),
          dataTableOutput(ns("abo_10_soon_done")),
        )
      ),
      
      # Right column
      column(
        6,
        div(
          class = "pc-card",
          
          tags$p(
            class = "pc-section-label",
            tags$i(class = "ti ti-clipboard-text"), "3-Monats-Abo und 6-Monats-Abo"
          ),
          
          selectInput(ns("weeks_still_left"), "Wie viele Wochen noch übrig?", choices = choices_weeks_still_left, selected = 6),
          dataTableOutput(ns("abo_month_soon_done"))
        )
      )
    )
  )
}


mod_abo_dashboard_server <- function(id, con, global_refresh) {
  moduleServer(id, function(input, output, session) {
    
    data_abo_10 <- reactive({
      req(input$still_left)
      
      global_refresh$abos   # important: without this, it doesn't get refreshed when abos change
      
      attended_courses <- get_attended_courses_abo_10(con)
      
      # filter
      attended_courses <- attended_courses |> filter(still_left <= as.integer(input$still_left))
      
      # make a nice version of the df for the ui
      attended_display <- attended_courses |> 
        select(vorname, name, still_left) |> 
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
        mutate(abo_end = format_swiss_date(abo_end)) |> 
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