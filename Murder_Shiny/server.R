library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    set.seed(100)
    #LatLng <- read.csv("./data/LatLng.csv")
    #murderTotals <- read.csv("./data/MurderTotals.csv")

    murders2plot <- filteredData()
    maxlat <- max(murders2plot$lat)
    minlat <- min(murders2plot$lat)
    maxlng <- max(murders2plot$lng)
    minlng <- min(murders2plot$lng)
    murders2plot  %>% leaflet() %>%
      addTiles(
        attribution = 'Data by <a href="https://www.kaggle.com/murderaccountability/homicide-reports">The Murder Accountability Project</a>'
      ) %>%
      addMarkers(label=murders2plot$title) %>%
      addCircles(weight = 1, radius = sqrt(murders2plot$Victim.Count) * 500)  %>%
      fitBounds(~min(minlng), ~min(minlat), ~max(maxlng), ~max(maxlat))
  })

  filteredData <- reactive({
    minYear <- as.integer(input$range[1])
    maxYear <- as.integer(input$range[2])
    updateMurders <- murderTotals[murderTotals$Year >= minYear,]
    updateMurders <- updateMurders[murderTotals$Year <= maxYear,]
    if (input$state!="" & input$state!="All")
    {
      updateMurders <- updateMurders[updateMurders$State==input$state,]
    }


    byCity <- aggregate(Victim.Count ~ City+State,updateMurders,sum)
    murderLocation <- merge(byCity,latlng,by=c("City","State"),all.x = TRUE)
    murderLocTotals <- aggregate(Victim.Count ~ City+State+lat+lng,murderLocation,sum)
    plotdata <- tail(murderLocTotals[with(murderLocTotals, order(Victim.Count)),],input$toplot)
    plotdata$title <- paste(plotdata$City,'(',plotdata$State,') - ',plotdata$Victim.Count)

    plotdata
  })

  observe({

    newData <- filteredData()
    maxlat <- max(newData$lat)
    minlat <- min(newData$lat)
    maxlng <- max(newData$lng)
    minlng <- min(newData$lng)
    leafletProxy("map", data = newData) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addMarkers(label=newData$title) %>%
      addCircles(weight = 1, radius = sqrt(newData$Victim.Count) * 500)  %>%
      fitBounds(~min(minlng), ~min(minlat), ~max(maxlng), ~max(maxlat))
 
  })






}
