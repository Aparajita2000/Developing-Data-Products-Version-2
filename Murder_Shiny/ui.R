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
  )


)
