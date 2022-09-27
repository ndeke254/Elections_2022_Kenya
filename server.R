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
  # county selection
  county_selection <- input$search_name
  
  # when a county is selected
  if(isTruthy(county_selection)) { 
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
      value = selected() $registered |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$clock<-renderEcharts4r({
    per <- (selected() $valid + selected() $rejected) / selected() $registered
    turnout <- round(per*100,2)
    e_charts() |> 
      e_gauge(turnout, "% PERCENT") |> 
      e_animation(duration = 4000)|>
      e_title('% TURNOUT',left='center')
  })
  output$valid <- renderValueBox({
    valueBox(
      'VALID',
      value = selected() $valid |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$rejected <- renderValueBox({
    valueBox(
      'REJECTED',
      value = selected() $rejected |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$raila <- renderValueBox({
    valueBox(
      'RAILA',
      value = selected() $raila |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$ruto <- renderValueBox({
    valueBox(
      'RUTO',
      value = selected() $ruto |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$waihiga <- renderValueBox({
    valueBox(
      'WAIHIGA',
      value = selected() $waihiga |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  output$wajackoya <- renderValueBox({
    valueBox(
      'WAJACKOYA',
      value = selected() $wajackoya |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("bar-chart")
    )
  })
  # county graph data
  data_county <- reactive ({ 
    data.frame(
    COUNTY = c("% VOTES","% VOTES","% VOTES","% VOTES"),
    CANDIDATE = c('RAILA','RUTO','WAIHIGA','WAJACKOYA'),
    PERCENTAGE= c(selected() $raila_per,selected() $ruto_per,
                  selected() $waihiga_per,selected() $wajackoya_per)
    )
    })
  # plot graph
  output$graph <- renderEcharts4r({
    data_county() |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTY) |>
      e_bar(PERCENTAGE) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% VOTES GARNERED')|> 
      e_tooltip(trigger='item')|>
      e_toolbox_feature(feature = "saveAsImage") |>
      e_color(my_colors2022)
    }) 
  }
  # when the selection is empty 
  else {
  output$livemap <- renderLeaflet({
    leaflet(county_shp) |>
      setView(lng=37.9083,lat=0.1769,zoom = 6) |>
      addPolygons(data =county_shp,
                  color = "brown",
                  layerId = county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col2022,
                  highlightOptions = highlightOptions(
                    color = "black",
                    weight = 2,
                    bringToFront = TRUE),
                  label = paste(
                    "<strong>COUNTY:</strong>",county_shp$name
                  ) |>
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "font-weight" = "normal",
                      padding = "3px 8px"),
                    textsize = "13px", 
                    direction = "auto")
                  ) |>
      addLegend(
        layerId="key",
        position = "topright",
        colors=c('blue','yellow'),
        labels = c('AZIMIO','UDA'),
        opacity = 3,
        title ='POLITICAL PARTY',
        className = "info legend")
  })
  }
}