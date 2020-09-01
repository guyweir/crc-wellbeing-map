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

#perth and kinross S12000024 on wb.data, S12000048	on boundary data
#fife S12000015 to S12000047
wb.data$admin.geography[wb.data$admin.geography == "S12000024"] <- "S12000048"
wb.data$admin.geography[wb.data$admin.geography == "S12000015"] <- "S12000047"
#### MAPS!!! #####

suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

#' go fetch boundary files and centriods to merge in
#LADbounds <- geojson_sp("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") #ultra gen 2019 LAD boundaries
LADbounds <- geojson_sp("https://opendata.arcgis.com/datasets/604fc1fbe022460eaac4758c7521c3e7_0.geojson") # ultra gen 2018
#LADbounds2 <- geojson_sf("https://opendata.arcgis.com/datasets/54b65ffb42c2480b88a20899aff750de_0.geojson")
#' merge data in
LADbounds <- merge(LADbounds, wb.data,by.x = "lad18cd", by.y = "admin.geography", all.y = T)

#' colour pallete
factpal1 <- colorQuantile("Greens",domain = LADbounds$`Life Satisfaction_Average (mean)`,n = 5, reverse = T)
factpal2 <- colorQuantile("Greens",domain = LADbounds$`Worthwhile_Average (mean)`,n = 5, reverse = T)
factpal3 <- colorQuantile("Greens",domain = LADbounds$`Happiness_Average (mean)`,n = 5, reverse = T)
factpal4 <- colorQuantile("Greens",domain = LADbounds$`Anxiety_Average (mean)`,n = 5)

factpalLEG <- colorFactor("Greens", domain = 1:5)

#' hover labels
labels1 <- sprintf("<strong>%s</strong><br/>%s Life satisfaction<sup></sup>",
                  LADbounds$lad18nm,
                  format(LADbounds$`Life Satisfaction_Average (mean)`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

labels2 <- sprintf("<strong>%s</strong><br/>%s Worthwhile<sup></sup>",
                   LADbounds$lad18nm,
                   format(LADbounds$`Worthwhile_Average (mean)`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

labels3 <- sprintf("<strong>%s</strong><br/>%s Happiness<sup></sup>",
                   LADbounds$lad18nm,
                   format(LADbounds$`Happiness_Average (mean)`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

labels4 <- sprintf("<strong>%s</strong><br/>%s Anxiety<sup></sup>",
                   LADbounds$lad18nm,
                   format(LADbounds$`Anxiety_Average (mean)`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)



#map element
m2 <- leaflet(LADbounds, height = "600px", options = list(padding = 100)) %>% setView(-3.5,55.2, 5.4) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 

  #LS
  addPolygons(fillColor = ~factpal1(LADbounds$`Life Satisfaction_Average (mean)`),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9, group = "Life Satisfaction") %>% 
  
  addPolygons(label = labels1, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Life Satisfaction") %>%
  #Worthwhile
  addPolygons(fillColor = ~factpal2(LADbounds$`Worthwhile_Average (mean)`),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9, group = "Worthwhile") %>% 
  
  addPolygons(label = labels2, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
               group = "Worthwhile") %>%
  #Happiness
  addPolygons(fillColor = ~factpal3(LADbounds$`Happiness_Average (mean)`),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9, group = "Happiness") %>% 
  
  addPolygons(label = labels3, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Happiness") %>%
  
  #Anxiety
  addPolygons(fillColor = ~factpal4(LADbounds$`Anxiety_Average (mean)`),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9, group = "Anxiety") %>% 
  
  addPolygons(label = labels4, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Anxiety") %>%
  #add legends
  
  
  # addLegend(pal = factpal3, values = LADbounds$`Happiness_Average (mean)`,group = "Happiness",
  #           labels = c(1:5),
  #           position = "bottomright", title = "Quintiles", ) %>% 

addLegend(colors = c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C"), 
          labels = c(1:5),
          position = "bottomright", 
          title = "Quintiles (5 = worst)") %>% 
  
  addLayersControl(
    baseGroups = c("Life Satisfaction", "Worthwhile", "Happiness", "Anxiety"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  
  hideGroup(c("Worthwhile", "Happiness", "Anxiety")) %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2



#' other web page elements
#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("Wellbeing measures by Local Authority,<br> April 2019 to March 2020, Great Britain</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Note: Average scores from survey scales self rated for respondants from 0 to 10<br>
Sources: Annual personal well-being estimates, ONS<br> 
                        Analysis: WPI Economics on behalf of CRC"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)


combo <- htmltools::tagList(title,m2,sources) #I think this makes a combined html object
browsable(combo)
htmltools::save_html(combo, "index.html", background = "#FFFCF1") 
