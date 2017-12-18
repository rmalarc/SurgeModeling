#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
attach('model.bin')
attach('metadata.bin')
library(FedData)
#library(magrittr)
library("lme4")
library(lubridate)
library(leaflet)
library(DT)

event_stations <- merge(merge(events,stations_model[which(!is.na(stations_model$event)),],by="event"),buoy_location,by="id")
event_stations_complete <- event_stations[which(event_stations$id %in% unique(events_dataset$station_id)),]

headings <- data.frame(heading=c("North","Northeast","East","Southeast","South","Southwest","West","Northwest"),
                       heading_course=seq(0,359,by=45),
                      due_r=c("East","Southeast","South","Southwest","West","Northwest","North","Northeast"),
                      due_l=c("West","Northwest","North","Northeast","East","Southeast","South","Southwest"),
                      stringsAsFactors = F
)

tideDataUrl <- function(station_id,date0,date1,product,interval="h"){
  paste0("https://tidesandcurrents.noaa.gov/api/datagetter?product=",product,"&application=NOS.COOPS.TAC.WL&begin_date=",date0,"&end_date=",date1,"&datum=MLLW&station=",station_id,"&time_zone=LST&units=english&format=csv&interval=",interval)
}

getBuoyDataPredictOnly <- function(date0,date1,station_id,interval="h"){
  cat(paste0(", ",station_id))
  
  pred <- read.csv(url(tideDataUrl(station_id,date0,date1,"predictions",interval=interval)))
  #product <- ifelse(interval=="h","hourly_height","water_level")
  #obs <- read.csv(url(tideDataUrl(station_id,date0,date1,product,interval=interval)))
  
  # winds <- read.csv(url(metDataUrl(station_id,date0,date1,"wind",interval)))
  #  air_pressure <- read.csv(url(metDataUrl(station_id,date0,date1,"air_pressure",interval=interval)))
  #  air_temperature<-read.csv(url(metDataUrl(station_id,date0,date1,"air_temperature",interval=interval)))
  #  ocean_temperature<-read.csv(url(metDataUrl(station_id,date0,date1,"water_temperature","NOS.COOPS.TAC.PHYSOCEAN",interval=interval)))
  
  buoy_data<-pred
  
  buoy_data$dtm <- as.POSIXct( buoy_data$Date.Time)
  
  buoy_data$hour <- hour(buoy_data$dtm)
  
  buoy_data$date_serial <- format(buoy_data$dtm,"%Y%m%d")
  
  if (nrow(buoy_data) ==0){
    cat("No data here!")
    return(data.frame())
  }

  buoy_data$Prediction0 <- c(NA,buoy_data$Prediction[-length(buoy_data$Prediction)])
  #  buoy_data$delta0 <- c(NA,buoy_data$delta[-length(buoy_data$delta)])
  buoy_data$tide_tangent <- buoy_data$Prediction - buoy_data$Prediction0
  buoy_data$station_id <- as.character(station_id)
  colnames(buoy_data) <- make.names(names=names(buoy_data), unique=TRUE, allow_ = TRUE)
  
  buoy_data
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$Hurricanes <- renderUI({
    selectInput("Hurricane", "Hurricane:",
                sort(unique(event_stations_complete$event)),selected = "Irma",multiple=F)
    
  })

  output$stormHeading <- renderUI({
    selectInput("stormHeading", "Storm Heading:",
                headings$heading,selected = "North",multiple=F)
    
  })
  output$scenarios <- renderUI({
    req(input$stormHeading,cancelOutput = TRUE)
    h <- headings[which(headings$heading==input$stormHeading),]
    vals <- c("8724580","8723970","8726607")
    names(vals) <- c(
              paste0("Cat 4 - Heading ",h$heading,", 10mi off to the ",h$due_r),
              paste0("Cat 4 - Heading ",h$heading,", 15mi off to the ",h$due_l),
              paste0("Cat 1 - Heading ",h$heading,", 15mi off to the ",h$due_r))
    selectInput("scenario", "Scenario:",
              vals,selected="8723970")
  })
  
  output$stormHeadings <- renderUI({
    selectInput("Heading", "Heading:",
                headings$heading,selected = "North",multiple=F)
    
  })  
  output$AllBuoys <- renderUI({
    menuEvents <-  stations_model
    buoys <- menuEvents$id
    names(buoys) <- menuEvents$name
    #    browser()
    selectInput("PredictionBuoy", "Buoy:",
                buoys[order(sapply(strsplit(names(buoys), ",(?=[^,]+$)", perl=TRUE),function(r) {paste(rev(r),collapse="")}))],
                selected = "8726667",
                multiple=F)
    
  })
  
  output$Buoys <- renderUI({
    menuEvents <-  event_stations_complete[which(event_stations_complete$event==input$Hurricane),]
    buoys <- menuEvents$id
    names(buoys) <- menuEvents$name.x
#    browser()
    selectInput("Buoy", "Buoy:",
                buoys[order(names(buoys))],
                selected = "8726667",
                multiple=F)
    
  })
  
  output$distPlot <- renderPlot({
    req(input$Buoy,cancelOutput = TRUE)
    plotEvents <- events_dataset[which(events_dataset$station_id==input$Buoy),]
    s_max <- max(plotEvents$Prediction + plotEvents$delta_pred,plotEvents$Prediction + plotEvents$delta,na.rm=T)+1
    s_min <- min(plotEvents$Prediction + plotEvents$delta_pred,plotEvents$Prediction + plotEvents$delta,na.rm=T)-1
    plot(Prediction + delta~dtm,plotEvents,ylim=c(s_min,s_max),col="black",main=paste0("Observed Vs Predicted Tide Delta\n"),xlab="Date",ylab="Tide Delta (Ft)",type="l")
#    plotEvents$event <- r['event.y']
    lines(plotEvents$dtm,plotEvents$Prediction + plotEvents$delta_pred,col="red")
    legend("bottomleft",legend=c("Observed","Predicted"), lty=c(1,1),col=c("black","red"),   inset = c(0.1, 0.1),pch=c(1,1))
  })
  
  projectionsDS <- reactive({
    req(input$PredictionBuoy,cancelOutput = TRUE)
    req(input$scenario,cancelOutput = TRUE)
    req(input$stormHeading,cancelOutput = TRUE)

    cat(paste0("getting data",input$PredictionBuoy))
    predictions <- getBuoyDataPredictOnly(format(Sys.Date(),"%Y%m%d"),format(Sys.Date()+days(9),"%Y%m%d"),input$PredictionBuoy)

    predictions <- predictions[which(complete.cases(predictions)),]
    
    scenarioData <- events_dataset[which(events_dataset$station_id==input$scenario),]
    
    # scale the wind speed
    
    if (input$est_sustained_wind_speed){
      max_ws <- max(scenarioData$Speed)
      factor <- ifelse(input$scenario=="8724580",120/max_ws,ifelse(input$scenario=="8723970",110/max_ws,1.24))
      scenarioData$Speed <- scenarioData$Speed * factor
    }
    
    # adjust the storm trajectory
    #browser()
    hdg <- headings[which(headings$heading==input$stormHeading),]$heading_course
    scenarioData$quadrant <- as.integer(scenarioData$quadrant) + hdg
    scenarioData$quadrant <- ifelse(scenarioData$quadrant>=360,scenarioData$quadrant-360,scenarioData$quadrant)
    scenarioData$quadrant <- as.character(scenarioData$quadrant)
    
    
    predictions$date_serial <- as.integer(predictions$date_serial) - min(as.integer(predictions$date_serial))
    scenarioData$date_serial <- as.integer(scenarioData$date_serial) - min(as.integer(scenarioData$date_serial))
    #browser()
    plotEvents <- merge(predictions[,c("dtm","date_serial","hour","station_id","tide_tangent","Prediction")],scenarioData[c("date_serial","hour","Speed","gust_delta","quadrant")])
    
    #browser()
    plotEvents$delta_pred <- predict(model,plotEvents)
    
    
    cat(paste0("Max Inundation: ",max(plotEvents$delta_pred+plotEvents$Prediction),"\n"))
    cat(paste0("Max Wind Speed: ",max(plotEvents$Speed),"\n"))
    
    #browser()
    plotEvents[order(plotEvents$dtm),]
    
  })
  
  
  output$forecastPlot <- renderPlot({
    req(input$PredictionBuoy,cancelOutput = TRUE)
    req(projectionsDS(),cancelOutput = T)
    plotEvents <- projectionsDS()
    #browser()
    s_max <- max(c(plotEvents$Prediction + plotEvents$delta_pred,plotEvents$Prediction),na.rm=T)+1
    s_min <- min(c(plotEvents$Prediction + plotEvents$delta_pred,plotEvents$Prediction),na.rm=T)-1
    plot(Prediction~dtm,plotEvents,ylim=c(s_min,s_max),col="black",main=paste0("Tide Predictions\n"),xlab="Date",ylab="Tide Level (Ft)",type="l")
    #    plotEvents$event <- r['event.y']
    lines(plotEvents$dtm,plotEvents$Prediction + plotEvents$delta_pred,col="red")
    legend("bottomleft",legend=c("Normal Tide","Prediction"), lty=c(1,1),col=c("black","red"),   inset = c(0.1, 0.1),pch=c(1,1))
  })
  
  output$forecastInundationPlot <- renderLeaflet({
    req(input$PredictionBuoy,cancelOutput = TRUE)
    req(projectionsDS(),cancelOutput = TRUE)
    plotEvents <- projectionsDS()

    p <- NA
    withProgress(message = 'Rendering Plots: ', value = 0, {

#      max_observed <- max(plotEvents$Water.Level,na.rm=T) * 0.3048
      max_predicted <- max(plotEvents$Prediction + plotEvents$delta_pred,na.rm=T) * 0.3048
      
      lat <- buoy_location[which(buoy_location$id==input$PredictionBuoy),]$lat
      lon <- buoy_location[which(buoy_location$id==input$PredictionBuoy),]$lon

      vepPolygon <- polygon_from_extent(raster::extent( lon+0.4, lon-0.4,lat+0.2, lat-0.2),
                                        proj4string = "+proj=longlat +datum=NAD83 +zone=12")
      
      incProgress(1/4, detail = paste("Downloading Map Data"))
      # Get the NED (USA ONLY)
      # Returns a raster
      NED <- get_ned(template = vepPolygon,
                     label = paste0(input$PredictionBuoy,input$scenario,input$heading,input$est_sustained_wind_speed))
      incProgress(1/4, detail = paste("Processing Map Data"))
#      values(NED) <- sapply(values(NED),function (v) {if (is.na(v)) 0 else v})
      ned_inundation <- NED
      values(ned_inundation) <- ifelse(values(NED)<max_predicted& values(NED)>=0.01,1,NA)
      incProgress(1/4, detail = paste("Rendering Maps"))
      p<- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=lon, lat=lat, popup=paste0("Buoy: ",input$PredictionBuoy)) %>%
        addRasterImage(ned_inundation,opacity=0.6,colors=colorNumeric(c("blue"), c(1), na.color = "transparent"))

      
      # make the pallete breaks for elevation
      #browser()
      #max_ned <- log10(max(values(NED),na.rm=T))
      #steps <- (max_ned+0.01)/6
      #b <- c(-2,seq(0.01,max_ned+steps,by=steps))
      #breaks <- c(-100,round(10^(b[1:8])))+0.01
      

      #x_range <- c(xmin(NED),xmax(NED))
      #y_range <- c(ymin(NED),ymax(NED))
      #plot(NED, col = c("royalblue",terrain.colors(8)),breaks=breaks,axes=FALSE, box=FALSE,legend=FALSE,main="Estimated Flooding")
      #plot(ned_inundation,add=T,col=c("green","lightskyblue"),legend=FALSE, axes=FALSE, box=FALSE)
      #lines(c(lon,lon),c(lat-0.01,lat+0.01),col="black",lwd=2)
      #lines(c(lon-0.01,lon+0.01),c(lat,lat),col="black",lwd=2)

      #legend(xmax(NED)+0.005,ymax(NED),c("Sea Level","Flooding",paste0(round(round(10^(b))*3.28084),"'")), fill=c("royalblue","lightskyblue",terrain.colors(8)),pch=16,bty="n",xpd=NA)
      
      incProgress(1/4, detail = paste("Processing Chart"))
    })      
    # Plot with raster::plot
    p
  })
  
  
  output$coverageMap <- renderLeaflet({
    coverage <- merge(buoy_metrics,buoy_location,by="id")
    coverage$quality <- ifelse(coverage$r2<0.25,"poor",ifelse(coverage$r2<0.5,"medium","good"))
    pal <- colorFactor(c("navy","yellow", "red"), domain = c("good", "medium","poor"))
    
    leaflet(data = coverage) %>% addTiles() %>%
      addCircleMarkers(~lon, ~lat,color = ~pal(quality),popup = ~paste0("<b>",name.x,"</b><br>Mean Error (Ft): ",as.character(me),"<br>Pearson Correlation: ",as.character(r2)))
  })
  
  output$animatedMap <- renderUI({
    url <- tags$iframe(src="https://public.tableau.com/profile/mauricio.alarcon#!/vizhome/irma_2/EffectofIrmainWaterLevels?:embed=true",width="100%", height=800)
    url
  })
  
  output$buoyPerformance <- DT::renderDataTable({
    coverage <- merge(buoy_metrics,buoy_location,by="id")
    coverage$quality <- ifelse(coverage$r2<0.25,"poor",ifelse(coverage$r2<0.5,"medium","good"))
    coverage <- coverage[,c("id","name.x","me","r2","vulnerable_quadrant")]
    colnames(coverage) <- c("Buoy ID","Buoy Name","Mean Error (Ft)","Pearson Correlation","Vulnerable Wind Flank (Degrees)")
    coverage
  })
  
  output$inundationPlot <- renderPlot({
    req(input$Buoy,cancelOutput = TRUE)
    p <- recordPlot()
    withProgress(message = 'Rendering Plots: ', value = 0, {
      plotEvents <- events_dataset[which(events_dataset$station_id==input$Buoy),]
      max_observed <- max(plotEvents$Water.Level,na.rm=T) * 0.3048
      max_predicted <- max(plotEvents$Prediction + plotEvents$delta_pred,na.rm=T) * 0.3048

      lat <- event_stations_complete[which(event_stations_complete$id==input$Buoy),]$lat
      lon <- event_stations_complete[which(event_stations_complete$id==input$Buoy),]$lon
  #      browser()
      vepPolygon <- polygon_from_extent(raster::extent( lon+0.2, lon-0.2,lat+0.2, lat-0.2),
                                        proj4string = "+proj=longlat +datum=NAD83 +zone=12")
      
      incProgress(1/4, detail = paste("Downloading Map Data"))
      # Get the NED (USA ONLY)
      # Returns a raster
      NED <- get_ned(template = vepPolygon,
                     label = input$Buoy)
      incProgress(1/4, detail = paste("Processing Map Data"))
      values(NED) <- sapply(values(NED),function (v) {if (is.na(v)) 0 else v})
      ned_inundation <- NED
      values(ned_inundation) <- ifelse(values(NED)<max_observed& values(NED)>=0.01,1,NA)
  
      ned_inundation_predicted <- NED
      values(ned_inundation_predicted) <- ifelse(values(NED)<max_predicted& values(NED)>=0.01,1,NA)

      incProgress(1/4, detail = paste("Rendering Maps"))

      # make the pallete breaks for elevation
      #browser()
      max_ned <- log10(max(values(NED),na.rm=T))
      #max_ned <- max(values(NED),na.rm=T)
      steps <- (max_ned+0.01)/6
      b <- c(-2,seq(0.01,max_ned+steps,by=steps))
      #b <- seq(0.01,max_ned+steps,by=steps)
      breaks <- c(-100,round(10^(b[1:8])))+0.01

#      browser()
#      layout(matrix(c(1,1,2,2), ncol=2, byrow=TRUE), heights=c(2, 1))
      
      x_range <- c(xmin(NED),xmax(NED))
      y_range <- c(ymin(NED),ymax(NED))
      par(mfcol=c(1,2), mai = c( 0.1, 0.1,1,0.1),oma=c(0,0,0,5),xpd=NA)
      #par(mai=c( 0.1, 0.1,0.1,0.1))
      #browser()
      plot(NED, col = c("royalblue",terrain.colors(8)),breaks=breaks,axes=FALSE, box=FALSE,legend=FALSE,main="Observed")
      plot(ned_inundation,add=T,col=c("green","lightskyblue"),legend=FALSE, axes=FALSE, box=FALSE)
      lines(c(lon,lon),c(lat-0.01,lat+0.01),col="black",lwd=2)
      lines(c(lon-0.01,lon+0.01),c(lat,lat),col="black",lwd=2)
      plot(NED, col = c("royalblue",terrain.colors(8)),breaks=breaks,axes=FALSE, box=FALSE,legend=FALSE,main="Predicted")
      plot(ned_inundation_predicted,add=T,col=c("green","lightskyblue"),legend=FALSE, axes=FALSE, box=FALSE)
      lines(c(lon,lon),c(lat-0.01,lat+0.01),col="black",lwd=2)
      lines(c(lon-0.01,lon+0.01),c(lat,lat),col="black",lwd=2)
      legend(xmax(NED)+0.005,ymax(NED),c("Sea Level","Flooding",paste0(round(round(10^(b))*3.28084),"'")), fill=c("royalblue","lightskyblue",terrain.colors(8)),pch=16,bty="n",xpd=NA)
      
#      par(mai=c(0,0,0,0))
      
      incProgress(1/4, detail = paste("Processing Chart"))
    })      
    # Plot with raster::plot
    p
  }, execOnResize = TRUE)
  output
  
})
