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
  observeEvent(input$search_name,{
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
      icon = icon("pencil")
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
      icon = icon("check")
    )
  })
  output$rejected <- renderValueBox({
    valueBox(
      'REJECTED',
      value = selected() $rejected |>
        prettyNum(big.mark =',', scientific = FALSE),
      icon = icon("xmark")
    )
  })
  output$raila <- renderValueBox({
    valueBox(
      'RAILA',
      value = selected() $raila |>
        prettyNum(big.mark =',', scientific = FALSE),
      color = 'blue',
      icon = icon("signal")
    )
  })
  output$ruto <- renderValueBox({
    valueBox(
      'RUTO',
      value = selected() $ruto |>
        prettyNum(big.mark =',', scientific = FALSE),
      color = 'yellow',
      icon = icon("bar-chart")
    )
  })
  output$waihiga <- renderValueBox({
    valueBox(
      'WAIHIGA',
      value = selected() $waihiga |>
        prettyNum(big.mark =',', scientific = FALSE),
      color = 'red',
      icon = icon("line-chart")
    )
  })
  output$wajackoya <- renderValueBox({
    valueBox(
      'WAJACKOYA',
      value = selected() $wajackoya |>
        prettyNum(big.mark =',', scientific = FALSE),
      color = 'green',
      icon = icon("pie-chart")
    )
  })
  # county graph data
  data_county <- reactive ({ 
    data.frame(
    COUNTY = c('% Votes','% Votes','% Votes','% Votes'),
    CANDIDATE = c('Raila','Ruto','Waihiga','Wajackoya'),
    PERCENTAGE= c(selected() $raila_per,selected() $ruto_per,
                  selected() $waihiga_per,selected() $wajackoya_per)
    )
    })
  # plot graph
  output$graph <- renderEcharts4r({
    data_county() |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTY) |>
      e_bar(PERCENTAGE, 
            emphasis = list(
        focus = "item"),
        itemStyle = list(
          shadowBlur = 0.5,
          shadowColor = '#4f1721',
          shadowOffsetX = 0.5)) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% Votes')|> 
      e_tooltip(backgroundColor='#e6ffff') |>
      e_toolbox_feature(feature = c("restore","saveAsImage")) |>
      e_legend(orient = 'vertical',right = '5', top = '15%') |>
      e_datazoom(type = 'inside') |>
      e_grid(show = TRUE)|>
      e_color(my_colors2022) |>
      e_title(text = paste(selected() $sno,input$search_name,'County'),
              subtext="Analyst: Jefferson Ndeke  Data: IEBC Kenya",left='center',top=1,
              sublink = 'https://github.com/ndeke254',
              textStyle = list(fontWeight = 'normal'))|>
      e_x_axis(splitLine=list(
        lineStyle=list(
          type='dashed'))) |>
      e_y_axis(scale=TRUE,
               splitLine=list(
                 lineStyle=list(
                   type='dashed')))
    }) 
  # map
  name_1 <- input$search_name
  cnt1 <- county_shp@data%>%filter(name%in%name_1)
  col <- cnt1 |> 
    select(col2022)
  col_name <- col[[1]]
  county_shp@data <- county_shp@data |> 
    mutate (cols=case_when(name%in%name_1 ~ col_name, TRUE~'white' ))
  #plot map
  output$livemap <- renderLeaflet({
    leaflet(county_shp,options = leafletOptions(zoomControl = FALSE ,
                                                scrollWheelZoom ='center',
                                                dragging = FALSE,
                                                maxZoom = 6,
                                                minZoom = 6)) |>
      setView(lng=37.9083,lat=0.1769,zoom = 6) |>
      addPolygons(data =county_shp,
                  color = "brown",
                  layerId = county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$cols,
                  highlightOptions = highlightOptions(
                    color = "black",
                    weight = 2,
                    bringToFront = TRUE),
                  label = paste(
                    "<strong>County:</strong>",county_shp$code,county_shp$name
                  ) |>
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "font-weight" = "normal",
                      color = 'red',
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
  # when the selection is empty 
  else {
    output$county_name <- renderInfoBox({
      infoBox(
        title = 'OVERALL',
        value = 'Kenya',
        icon = icon("credit-card")
      )
    })
    output$registered <- renderValueBox({
      valueBox(
        'REGISTERED',
        value = totals $registered[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("pencil")
      )
    })
    output$clock<-renderEcharts4r({
      per <- (totals $valid[2] + totals $rejected[2] ) / totals $registered[2]
      turnout <- round(per*100,2)
      e_charts() |> 
        e_gauge(turnout, "% PERCENT") |> 
        e_animation(duration = 4000)|>
        e_title('% TURNOUT',left='center')
    })
    output$valid <- renderValueBox({
      valueBox(
        'VALID',
        value = totals $valid[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("check")
      )
    })
    output$rejected <- renderValueBox({
      valueBox(
        'REJECTED',
        value = totals $rejected[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("xmark")
      )
    })
    output$raila <- renderValueBox({
      valueBox(
        'RAILA',
        value = totals $raila[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'blue',
        icon = icon("signal")
      )
    })
    output$ruto <- renderValueBox({
      valueBox(
        'RUTO',
        value = totals $ruto[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'yellow',
        icon = icon("bar-chart")
      )
    })
    output$waihiga <- renderValueBox({
      valueBox(
        'WAIHIGA',
        value = totals $waihiga[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'red',
        icon = icon("line-chart")
      )
    })
    output$wajackoya <- renderValueBox({
      valueBox(
        'WAJACKOYA',
        value = totals $wajackoya[2] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'green',
        icon = icon("pie-chart")
      )
    })
    # county graph data
    data_county <- reactive ({ 
      data.frame(
        COUNTY = c('% Votes','% Votes','% Votes','% Votes'),
        CANDIDATE = c('Raila','Ruto','Waihiga','Wajackoya'),
        PERCENTAGE= c(totals $raila_per[2],totals $ruto_per[2],
                      totals $waihiga_per[2],totals $wajackoya_per[2])
      )
    })
    
    # plot graph
    output$graph <- renderEcharts4r({
      data_county() |> 
        group_by(CANDIDATE) |> 
        e_chart(COUNTY) |>
        e_bar(PERCENTAGE,
              emphasis = list(
                focus = "item"),
              itemStyle = list(
                shadowBlur = 0.5,
                shadowColor = '#4f1721',
                shadowOffsetX = 0.5)) |>
        e_animation(duration = 4000) |>
        e_axis_labels(x='',y = '% Votes') |> 
        e_tooltip(backgroundColor='#e6ffff') |>
        e_toolbox_feature(feature = c("restore","saveAsImage")) |>
        e_legend(orient = 'vertical',right = '5', top = '15%') |>
        e_datazoom(type = 'inside') |>
        e_grid(show = TRUE)|>
        e_title(text = paste('Overall: Kenya'),
                subtext="Analyst: Jefferson Ndeke  Data: IEBC Kenya",left='center',top=1,
                sublink = 'https://github.com/ndeke254',
                textStyle = list(fontWeight = 'normal'))|>
        e_x_axis(splitLine=list(
          lineStyle=list(
            type='dashed'))) |>
        e_y_axis(scale=TRUE,
                 splitLine=list(
                   lineStyle=list(
                     type='dashed'))) |>
        e_color(my_colors2022)
    }) 
  output$livemap <- renderLeaflet({
    leaflet(county_shp,options = leafletOptions(zoomControl = FALSE ,
                                                scrollWheelZoom ='center',
                                                dragging = FALSE,
                                                maxZoom = 6,
                                                minZoom = 6)) |>
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
                    "<strong>County:</strong>",county_shp$code,county_shp$name
                  ) |>
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "font-weight" = "normal",
                      color = 'red',
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
  })
  #checkbox ticked
  observeEvent(input$check, {
    if(isTruthy(input$check) & isTruthy(input$search_name)) {
      click <- input$search_name
      idx <- which(county_shp$name == input$search_name)
      name1 <-county_shp$name[[idx]]
      cnt <- county_shp@data%>%filter(name%in%name1)
      county_shp@data <- cnt
      county_shp@polygons <-list(county_shp@polygons[[idx]])
      mapInd <-maps::map(county_shp,fill = TRUE, plot = FALSE)
      output$livemap <- renderLeaflet({
        leaflet(county_shp,options = leafletOptions(
          zoomControl = FALSE ,
          scrollWheelZoom ='center',
          dragging = FALSE,
          maxZoom = county_shp$zoom,
          minZoom = county_shp$zoom)) |>
          setView(lng=37.9083,lat=0.1769,zoom = 6) |>
        addPolygons(data = county_shp,
                    color = "brown",
                    layerId= county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2022,
                    highlightOptions = highlightOptions(color = "black",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = paste(
                      "<strong>County:</strong>",cnt$code,cnt$name
                    ) %>%
                      lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto")
        )%>%
        setView(lng = ((mapInd$range[[1]] + mapInd$range[[2]])/2),
                lat = ((mapInd$range[[3]] + mapInd$range[[4]])/2),
                zoom = county_shp$zoom)%>%
        addLegend(
                  layerId="key",
                  position = "topright",
                  colors=c('blue','yellow'),
                  labels = c('AZIMIO','UDA'),
                  opacity = 3,
                  title ='POLITICAL PARTY',
                  className = "info legend"
                )
      })
    } else if (!isTruthy(input$check) & isTruthy(input$search_name)) {
      # map
      name_1 <- input$search_name
      cnt1 <- county_shp@data%>%filter(name%in%name_1)
      col <- cnt1 |> 
        select(col2022)
      col_name <- col[[1]]
      county_shp@data <- county_shp@data |> 
        mutate (cols=case_when(name%in%name_1 ~ col_name, TRUE~'white' ))
      #plot map
      output$livemap <- renderLeaflet({
        leaflet(county_shp,options = leafletOptions(zoomControl = FALSE ,
                                                    scrollWheelZoom ='center',
                                                    dragging = FALSE,
                                                    maxZoom = 6,
                                                    minZoom = 6)) |>
          setView(lng=37.9083,lat=0.1769,zoom = 6) |>
          addPolygons(data =county_shp,
                      color = "brown",
                      layerId = county_shp$name,
                      weight = 1,
                      smoothFactor = 0.5,
                      opacity = 3,
                      fillOpacity = 2,
                      fillColor = county_shp$cols,
                      highlightOptions = highlightOptions(
                        color = "black",
                        weight = 2,
                        bringToFront = TRUE),
                      label = paste(
                        "<strong>County:</strong>",county_shp$code,county_shp$name
                      ) |>
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list(
                          "font-weight" = "normal",
                          color = 'red',
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
    } else {
      return()
    }
  })
  # diaspora vote
  observeEvent(input$diaspora, {
    output$county_name <- renderInfoBox({
      infoBox(
        title = 'OTHERS',
        value = 'Diaspora',
        icon = icon("credit-card")
      )
    })
    output$registered <- renderValueBox({
      valueBox(
        'REGISTERED',
        value = totals $registered[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("pencil")
      )
    })
    output$clock<-renderEcharts4r({
      per <- (totals $valid[1] + totals $rejected[1] ) / totals $registered[1]
      turnout <- round(per*100,2)
      e_charts() |> 
        e_gauge(turnout, "% PERCENT") |> 
        e_animation(duration = 4000)|>
        e_title('% TURNOUT',left='center')
    })
    output$valid <- renderValueBox({
      valueBox(
        'VALID',
        value = totals $valid[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("check")
      )
    })
    output$rejected <- renderValueBox({
      valueBox(
        'REJECTED',
        value = totals $rejected[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        icon = icon("xmark")
      )
    })
    output$raila <- renderValueBox({
      valueBox(
        'RAILA',
        value = totals $raila[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'blue',
        icon = icon("signal")
      )
    })
    output$ruto <- renderValueBox({
      valueBox(
        'RUTO',
        value = totals $ruto[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'yellow',
        icon = icon("bar-chart")
      )
    })
    output$waihiga <- renderValueBox({
      valueBox(
        'WAIHIGA',
        value = totals $waihiga[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'red',
        icon = icon("line-chart")
      )
    })
    output$wajackoya <- renderValueBox({
      valueBox(
        'WAJACKOYA',
        value = totals $wajackoya[1] |>
          prettyNum(big.mark =',', scientific = FALSE),
        color = 'green',
        icon = icon("pie-chart")
      )
    })
    # county graph data
    data_county <- reactive ({ 
      data.frame(
        COUNTY = c('% Votes','% Votes','% Votes','% Votes'),
        CANDIDATE = c('Raila','Ruto','Waihiga','Wajackoya'),
        PERCENTAGE= c(totals $raila_per[1],totals $ruto_per[1],
                      totals $waihiga_per[1],totals $wajackoya_per[1])
      )
    })
    
    # plot graph
    output$graph <- renderEcharts4r({
      data_county() |> 
        group_by(CANDIDATE) |> 
        e_chart(COUNTY) |>
        e_bar(PERCENTAGE,
              emphasis = list(
                focus = "item"),
              itemStyle = list(
                shadowBlur = 0.5,
                shadowColor = '#4f1721',
                shadowOffsetX = 0.5)) |>
        e_animation(duration = 4000) |>
        e_axis_labels(x='',y = '% Votes') |> 
        e_tooltip(backgroundColor='#e6ffff') |>
        e_toolbox_feature(feature = c("restore","saveAsImage")) |>
        e_legend(orient = 'vertical',right = '5', top = '15%') |>
        e_datazoom(type = 'inside') |>
        e_grid(show = TRUE)|>
        e_title(text = paste('Others: Diaspora'),
                subtext="Analyst: Jefferson Ndeke  Data: IEBC Kenya",left='center',top=1,
                sublink = 'https://github.com/ndeke254',
                textStyle = list(fontWeight = 'normal'))|>
        e_x_axis(splitLine=list(
          lineStyle=list(
            type='dashed'))) |>
        e_y_axis(scale=TRUE,
                 splitLine=list(
                   lineStyle=list(
                     type='dashed'))) |>
        e_color(my_colors2022)
    }) 
    output$livemap <- renderLeaflet({
      leaflet(county_shp,options = leafletOptions(zoomControl = FALSE ,
                                                  scrollWheelZoom ='center',
                                                  dragging = FALSE,
                                                  maxZoom = 6,
                                                  minZoom = 6)) |>
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
                      "<strong>County:</strong>",county_shp$code,county_shp$name
                    ) |>
                      lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list(
                        "font-weight" = "normal",
                        color = 'red',
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
  })
}