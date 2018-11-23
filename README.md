# WSPD Scanner

This is a shiny application whose purpose is to allow some exploration and prediction of different call types that occur in the city of Winston-Salem, NC.

## Data Sources

Data were retrieved from the Winston-Salem Police Department website that listed the calls by day as well as the call type. These calls were then geocoded using the US Census provided API. After the calls were geocoded the calls were joined to data from the American Community Survey in order to tie demographic information to the locations where the calls occured.

## Packages

If you try and run this package locally, the required packages to be installed are below:

`shiny`
`shinydashboard`
`leaflet`
`tidyverse`
`DT`
`arm`
`plotly`
`lme4`
`leaflet`
`sf`
`htmltools`
`sjPlot`

Additionaly, I recommend using the full size browser rather than the RStudio viewer pane.