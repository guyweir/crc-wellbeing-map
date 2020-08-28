# Mapping local area wellbeing

library(tidyverse)

#### Data prep ####

wb.data <- read.csv("ec5cc81b-85df-40c0-ba03-82dd96aaacc7.csv", stringsAsFactors = F)

#filter the data to most recent

max(wb.data$time)
wb.data <- wb.data %>% filter(`time` == max(time))
#pivot long
wb.data <- wb.data %>% rename(`Score` = V4_3)
wb.data <- pivot_wider(wb.data,id_cols = c(`admin.geography`,`geography`), names_from = c(`allmeasuresofwellbeing`,`estimate`), 
                        values_from = `Score`)

wb.data <- wb.data %>% select(admin.geography,geography, contains("average"))



#### MAPS!!! #####

suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

#' go fetch boundary files and centriods to merge in
LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson")
LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")

#' merge data in
LADbounds <- merge(LADbounds, allgva,by.x = "lad19cd", by.y = "LAD code", all.y = T)

LSOAcentroids <- merge(LSOAcentroids, IMD19all, by.x = "lsoa11cd", by.y = "LSOA code (2011)")
LSOAcentroids <- LSOAcentroids %>% filter(`IMD decile (1 is most deprived)` == 1)  # select just most deprived 10%

#' colour pallete


factpal <- colorFactor("BuPu",domain = as.factor(LADbounds$gvaquins),n = 5 ,ordered = TRUE )
binpal <- colorQuantile("Blues", LADbounds$`2018`, n = 5)

#' hover labels
labels <- sprintf("<strong>%s</strong><br/>%s GVA (£ mil)<sup></sup>",
                  LADbounds$lad19nm,
                  format(LADbounds$`2018`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

####function for circles to additional legend####
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.7, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-5,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}


#map element
m2 <- leaflet(LADbounds, height = "600px", options = list(padding = 100)) %>% setView(-3.5,53.2, 5.5) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  addMapPane(name = "toplayer", zIndex = 420) %>% #layer orders to make sure LSOA markers render on top.
  addMapPane(name = "nottoplayer", zIndex = 410) %>% 
  
  addPolygons(fillColor = ~factpal(LADbounds$gvaquins),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9) %>% 
  
  addPolygons(label = labels, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              options = leafletOptions(pane = "nottoplayer")) %>%
  
  addCircleMarkers(data = LSOAcentroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>% 
  
  addLegendCustom(colors = c("#00E1BA"), 
                  labels = c("Most deprived 10% n'hood"),
                  
                  sizes = c(10), position = "bottomright" ) %>% 
  
  addLegend(pal = factpal, values = LADbounds$gvaquins, labels = levels(LADbounds$gvaquins), position = "bottomright", title = "GVA Quintiles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2



#' other web page elements
#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("Gross Value Added (GVA) by Local Authority and most deprived neighbourhoods,<br> 2018, England and Wales</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Sources: Regional gross value added (balanced) by industry: local authorities by NUTS1 region,ONS; <br> Indices of Multiple Deprivation, MHCLG; Welsh Index of Multiple Deprivation 2019<br> 
                        Analysis: WPI Economics on behalf of CRC <br>
                         Note: Deprivation ranks are relative to England and Wales separately"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)


combo <- htmltools::tagList(title,m2,sources) #I think this makes a combined html object
browsable(combo)
htmltools::save_html(combo, "index.html", background = "#FFFCF1") 
