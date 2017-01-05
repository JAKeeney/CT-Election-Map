library(tmap)
library(leaflet)
library(scales)

datafile <- "CTElectionResults.xlsx"
ctdata <- rio::import(datafile)
ctdata <- ctdata[,c("County", "Clinton", "Trump")]

ctdata$ClintonMarginVotes <- ctdata$Clinton - ctdata$Trump
ctdata$ClintonPct <- (ctdata$Clinton) / (ctdata$Clinton + ctdata$Trump)
ctdata$TrumpPct <- (ctdata$Trump) / (ctdata$Clinton + ctdata$Trump)
ctdata$ClintonMarginPctgPoints <- ctdata$ClintonPct - ctdata$TrumpPct

usshapefile <- "cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

ctgeo <- usgeo[usgeo@data$STATEFP=="09",]
ctgeo@data$NAME <- as.character(ctgeo@data$NAME)
ctgeo <- ctgeo[order(ctgeo@data$NAME),]
ctdata <- ctdata[order(ctdata$County),]

ctmap <- append_data(ctgeo, ctdata, key.shp = "NAME", key.data="County")

tm_shape(ctmap) +
  tm_fill("ClintonMarginVotes", title="Clinton Margin, Total Votes", palette = "Blues") +
  tm_borders(alpha=.5) + tm_text("NAME", size=0.8)

minpct <- min(c(ctmap$`ClintonPct`, ctmap$`TrumpPct`))
maxpct <- max(c(ctmap$`ClintonPct`, ctmap$`TrumpPct`))

winner <- c("Clinton", "Clinton", "Trump", "Clinton", "Clinton", "Clinton", "Clinton", "Trump")


trumpPalette <- colorNumeric(palette = "Reds", domain=c(minpct, maxpct))
clintonPalette <- colorNumeric(palette = "Blues", domain = c(minpct, maxpct))
winnerPalette <- colorFactor(palette=c("blue3", "red2"), domain = winner)

ctpopup <- paste0("County: ", ctmap@data$NAME, "\n",
                  "Winner: ", winner, "\n", "\n",
                  "Trump: ", percent(ctmap@data$`TrumpPct`),"\n",
                  "Clinton: ", percent(ctmap@data$`ClintonPct`))
datafile <- "CTElectionResults.xlsx"
ctdata <- rio::import(datafile)
ctdata <- ctdata[,c("County", "Clinton", "Trump")]

ctdata$ClintonMarginVotes <- ctdata$Clinton - ctdata$Trump
ctdata$ClintonPct <- (ctdata$Clinton) / (ctdata$Clinton + ctdata$Trump)
ctdata$TrumpPct <- (ctdata$Trump) / (ctdata$Clinton + ctdata$Trump)
ctdata$ClintonMarginPctgPoints <- ctdata$ClintonPct - ctdata$TrumpPct

usshapefile <- "cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

ctgeo <- usgeo[usgeo@data$STATEFP=="09",]
ctgeo@data$NAME <- as.character(ctgeo@data$NAME)
ctgeo <- ctgeo[order(ctgeo@data$NAME),]
ctdata <- ctdata[order(ctdata$County),]

ctmap <- append_data(ctgeo, ctdata, key.shp = "NAME", key.data="County")

tm_shape(ctmap) +
  tm_fill("ClintonMarginVotes", title="Clinton Margin, Total Votes", palette = "Blues") +
  tm_borders(alpha=.5) + tm_text("NAME", size=0.8)

minpct <- min(c(ctmap$`ClintonPct`, ctmap$`TrumpPct`))
maxpct <- max(c(ctmap$`ClintonPct`, ctmap$`TrumpPct`))

winner <- c("Clinton", "Clinton", "Trump", "Clinton", "Clinton", "Clinton", "Clinton", "Trump")

trumpPalette <- colorNumeric(palette = "Reds", domain=c(minpct, maxpct))
clintonPalette <- colorNumeric(palette = "Blues", domain = c(minpct, maxpct))
winnerPalette <- colorFactor(palette=c("blue3", "red2"), domain = winner)

ctpopup <- paste0("<b>County: ", ctmap@data$NAME, "<br/>",
                  "Winner: ", winner, "</b><br/>", "<br/>",
                  "Trump: ", percent(ctmap@data$`TrumpPct`),"<br/>",
                  "Clinton: ", percent(ctmap@data$`ClintonPct`))

ctFinalMap <- leaflet(ctmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=ctpopup, 
              color= ~winnerPalette(winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", colors=c("red", "blue"), labels=c("Trump", "Clinton"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=ctpopup, 
              color= ~trumpPalette(ctmap@data$`TrumpPct`),
              group="Trump"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=ctpopup, 
              color= ~clintonPalette(ctmap@data$`ClintonPct`),
              group="Clinton"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Winners", "Trump", "Clinton"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

ctFinalMap <- leaflet(ctmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=ctpopup, 
              color= ~winnerPalette(winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", colors=c("red", "blue"), labels=c("Trump", "Clinton"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=ctpopup, 
              color= ~trumpPalette(ctmap@data$`TrumpPct`),
              group="Trump"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=ctpopup, 
              color= ~clintonPalette(ctmap@data$`ClintonPct`),
              group="Clinton"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Winners", "Trump", "Clinton"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

# # #

ui <- fluidPage(leafletOutput("MyMap"), p())
 
server <- function(input,output,session) {output$MyMap <- renderLeaflet(ctFinalMap)}

shinyApp(ui,server)