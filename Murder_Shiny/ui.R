library(leaflet)

# State List
states<- rbind(c(add="All"),cbind(abb=state.abb))



navbarPage("Murder Map", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Murder Data Explorer"),

        sliderInput("range", "Date Range:",
                    min = 1980, max = 2014, value = c(1980,2014)),
        selectInput("state", "State:", states, selected = "All"),
        sliderInput("toplot", "Top Cities To Plot:",
                    min = 1, max = 2000, value = 40),
        submitButton("Update View", icon("refresh"))


        #plotOutput("histCentile", height = 200),
        #plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('The Murder Accountability Project'), ' @ www.murderdata.org'
      )
    )
    
  ),
  tabPanel("About",
           
           h1("Murder Data Explorer"),
           p(),
           h2("Background"),
             
           verbatimTextOutput("The Murder Accountability Project is the most complete database of homicides 
in the United States currently available. This dataset includes murders from the FBIs Supplementary Homicide Report from 1976 to the present and Freedom of Information Act data on more than 22,000 homicides that were not reported to the Justice Department. This dataset includes the age, race, sex, ethnicity of victims and perpetrators, in addition to the relationship between the victim and perpetrator and weapon used."),
           
           h2("How To Use"),
           
           h3("The Application has four controls"),
           
           tags$ul(
             tags$li("The date range of murders to view (start & end year)"),
             tags$li("The state to be viewed (all selects all states)"),
             tags$li("The number of city/counties to plot (1 to 1500)"),
             tags$li("Update button"),
           
           verbatimTextOutput("Select the date range, state and number of cities"),
           verbatimTextOutput("Then click update to make changes")),
           
           
           h2("Navigating the Map"),
           
           h3("The map can be navigated with mouse"),
           tags$ul(
             tags$li("Click and drag to move location"),
             tags$li("Use scroll wheel to zoom"),
             tags$li("Use Arrow keys")),
           
           h2("Viewing the Data"),
           
           h3("There is a pin on the map for each data point"),
           tags$ul(
             tags$li("The size of the circle indicates the victim count"),
             tags$li("The pin is the average of the zip codes for that county"),
             tags$li("The pin has a hover which will display the county/state & victim count"))
           
           
  )


)
