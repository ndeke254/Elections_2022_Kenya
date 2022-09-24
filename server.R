# application server

server <- function(input, output, session) {
  #Update the select search
  updateSelectizeInput(
    session,
    inputId = 'search_name',
    server=TRUE,
    choices = results$county,
    selected = ''
  )  
  selected <- reactive({
    results |> filter(county %in% input$search_name)
  })
  output$county_name <- renderInfoBox({
    infoBox(
   title = 'COUNTY',
    value = paste(selected() $sno,input$search_name),
    icon = icon("credit-card")
    )
  })
  output$registered <- renderValueBox({
    valueBox(
      'REGISTERED',
      value = selected() $registered,
      icon = icon("bar-chart")
    )
  })
  
}