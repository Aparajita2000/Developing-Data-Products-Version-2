library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  MurderInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(murderLocation[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      lat >= latRng[1] & lat <= latRng[2] &
        lng >= lngRng[1] & lng <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- len(MurderInBounds()$State)

  #output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    #if (nrow(MurderInBounds()) == 0)
    #  return(NULL)

    #hist(MurderInBounds()$Victim.Count,
    #  breaks = centileBreaks,
    #  main = "SuperZIP score (visible zips)",
    #  xlab = "Percentile",
    #  xlim = range(MurderInBounds()$State),
    #  col = '#00DD00',
     # border = 'white')
 # })

  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size



    #if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
    #  radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #} else {
    #  radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #}

    leafletProxy("map", data = murderLocation) %>%
      clearShapes() %>%
      addCircles(~lng, ~lat, radius=5, layerId=~State,
        stroke=FALSE, fillOpacity=0.4 )
 
  })

  # Show a popup at the given location
  showZipcodePopup <- function(murderLocation, lat, lng) {
    #selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:")
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = State)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })


}
