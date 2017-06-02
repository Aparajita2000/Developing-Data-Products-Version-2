Murder Data Explorer
========================================================
author: Mark Davey
date: June 2nd 2017
autosize: true

Background
========================================================

The Murder Accountability Project is the most complete database of homicides in the United States currently available. This dataset includes murders from the FBI's Supplementary Homicide Report from 1976 to the present and Freedom of Information Act data on more than 22,000 homicides that were not reported to the Justice Department. This dataset includes the age, race, sex, ethnicity of victims and perpetrators, in addition to the relationship between the victim and perpetrator and weapon used. [Source](https://www.kaggle.com/murderaccountability/homicide-reports)

How To Use
========================================================

The Application has four controls

[Application](https://kariato.shinyapps.io/murder_data_explorer/)
- The date range of murders to view (start & end year)
- The state to be viewed (all selects all states)
- The number of city/counties to plot (1 to 1500)
- Update button

Select the date range, state and number of cities
Then click update to make changes

Navigating the Map
========================================================

[Application](https://kariato.shinyapps.io/murder_data_explorer/)
The map can be navigated with mouse
- Click and drag to move location
- Use scroll wheel to zoom
- Use Arrow keys

Viewing the Data
========================================================

[Application](https://kariato.shinyapps.io/murder_data_explorer/)
There is a pin on the map for each data point
- The size of the circle indicates the victim count
- The pin is the average of the zip codes for that county
- The pin has a hover which will display the COunty/State & Victim Count
