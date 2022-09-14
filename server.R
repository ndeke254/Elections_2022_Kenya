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
}