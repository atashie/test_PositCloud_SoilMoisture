#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
#library(leafem)# for addImageQuery
#library(data.table)
library(sf)
library(mapiso)
library(terra)
sf_use_s2(FALSE)


# Define UI
ui <- fluidPage(
  titlePanel("Soil Moisture: % Saturation and % Difference from Climatology"),
  hr(),
  fluidRow(
    column(5,
           selectInput("monthsOut", "Months Out:",
                       c("End of Current Month"=1,"Next Month"=2,"2-3 Months"=3,"3-4 Months"=4,"4-5 Months"=5,"5-6 Months"=6))
    ),
    column(2
    ),
#    column(3,
#           sliderInput("opacity", "Opacity", 0, 1, value=0.7, step=0.1)
#    ),
    column(5,
           fluidRow(
             selectInput("mapType", "Map Display Type:",
                         c("Contour Map"=2, "High Impact Dryness"=4, "Pixels ('Heat Map')"=1, "States Map"=3)),
             selectInput("stressThresh", "High Impact Dryness Selector (only updates 'High Impact Dryness' map):",
                         c("Somewhat Drier (< -4%)"=-4, "Drier (< -8%)"=-8, "Much Drier (< -12%)"=-12))
           )
    ),
  ),
  hr(),
  fluidRow(
   column(leafletOutput("mymapAbs"), width=6),
   column(leafletOutput("mymapDif"), width=6)
  )
#  fluidRow(
#    column(12,
#      plotOutput("barPlot")
#    )
#  )
)

# Define server logic
server <- function(input, output, session) {
  summaryOutput_rsStack_abs = raster::brick(paste0("./SoilMoisture_projectionOutput_month-all.tif"))
  summaryOutput_rsStack_dif = raster::brick(paste0("./SoilMoisture_projectionOutput_month-all_dif.tif"))
  thisCountry = sf::st_read("./provinceLevelForecast.shp")
  
  initialLat = -30
  initialLng = 138

  pal1 = colorNumeric("RdYlGn", domain = c(0,100), na.color = "transparent")
  pal2 = colorNumeric("RdBu", domain = c(-20,20), na.color = "transparent")
  
  output$mymapAbs <- renderLeaflet({
    theseMonthsOut = as.numeric(input$monthsOut)
    thisBrkLayer = summaryOutput_rsStack_abs[[theseMonthsOut]]
    isoHets = mapiso(x = terra::rast(thisBrkLayer), nbreaks=6)
    isoHets$isomean = (isoHets$isomin + isoHets$isomax) / 2
    thisCountry$plotCol_abs = as.data.frame(thisCountry)[,which(names(thisCountry) == paste0("abs_",input$monthsOut))]
  
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(thisBrkLayer, color=pal1, opacity = 0.6 * ifelse(input$mapType %in% c(1,4), 1, 0)) %>%
      addPolygons(data=isoHets, color="transparent", weight=0, fillColor=~pal1(isomean), fillOpacity = 0.6 * ifelse(input$mapType == 2, 1, 0)) %>%
      addPolygons(data=thisCountry, color="grey10", weight=0.1, fillColor=~pal1(plotCol_abs), fillOpacity = 0.6 * ifelse(input$mapType == 3, 1, 0)) %>%
      setView(lng = initialLng, lat = initialLat, zoom = 4) %>%
      addLegend(data=isoHets, "bottomleft", title="Soil </br> % Saturation", pal=pal1, values=~isomean, labFormat=labelFormat(suffix="%"), opacity=1) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>%
      htmlwidgets::onRender(
        'function(el, x) {
          var map = this;
          map.on("zoomend", function(e) {
            var center = map.getCenter();
            Shiny.setInputValue("mymapAbs_zoom", e.target._zoom);
            Shiny.setInputValue("mymapAbs_lat", center.lat);
            Shiny.setInputValue("mymapAbs_lng", center.lng);
          });
        }'
      )
  })
  
  rv = reactiveValues(zoom=10, lng=initialLng, lat=initialLat)

  output$barPlot = renderPlot(
    barplot(1:10,6:15)
  )

  output$mymapDif <- renderLeaflet({
    theseMonthsOut = as.numeric(input$monthsOut)
    thisBrkLayer = summaryOutput_rsStack_dif[[theseMonthsOut]]
    isoHets = mapiso(x = terra::rast(thisBrkLayer), nbreaks=6)
    isoHets$isomean = (isoHets$isomin + isoHets$isomax) / 2
    thisCountry$plotCol_dif = as.data.frame(thisCountry)[,which(names(thisCountry) == paste0("dif_",input$monthsOut))]
    thisBrkLayer_highStress = thisBrkLayer; thisBrkLayer_highStress[thisBrkLayer > as.numeric(input$stressThresh)] = 0
    
#    zoomLevel(input$mymapAbs_zoom)
#    centerCoords(c(input$mymapAbs_lng, lat=input$mymapAbs_lat))

    leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addRasterImage(thisBrkLayer, opacity=0.6 * ifelse(input$mapType == 1, 1, 0), colors = pal2) %>%
      addPolygons(data=isoHets, color="transparent", fillColor=~pal2(isomean), fillOpacity=0.6 * ifelse(input$mapType == 2, 1, 0),) %>%
      addPolygons(data=thisCountry, color="grey10", weight=0.1, fillColor=~pal2(plotCol_dif), fillOpacity=0.6 * ifelse(input$mapType == 3, 1, 0)) %>%
      addRasterImage(thisBrkLayer_highStress, colors=pal2, opacity=0.6 * ifelse(input$mapType == 4, 1, 0)) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = initialLng, lat = initialLat, zoom = 4) %>%
      addLegend(data=isoHets, "bottomleft", title="% Difference</br>from</br>Climatology", pal=pal2, values=~isomean, labFormat=labelFormat(suffix="%"), opacity=1)
  })
#  observe({
#    # Update reactive values when input changes
#    rv$mymapAbs_zoom <- input$mymapAbs_zoom
#    rv$mymapAbs_lat <- input$mymapAbs_lat
#    rv$mymapAbs_lng <- input$mymapAbs_lng
    
    # Use rv$map_zoom, rv$map_lat, and rv$map_lng in your logic
#    cat("Zoom level:", rv$mymapAbs_zoom, "\n")
#    cat("Latitude:", rv$mymapAbs_lat, "\n")
#    cat("Longitude:", rv$mymapAbs_lng, "\n")
#  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
