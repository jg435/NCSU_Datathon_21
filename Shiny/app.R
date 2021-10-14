options(warn = -1)

Packages <- c("tidyverse", "skimr", "lubridate", "ggplot2", "gganimate", "RColorBrewer", "DT", "readr", "maptools","rmapshaper",
              "randomForest", "dplyr", "janitor", "grid", "scales", "leaflet", "leaflet.extras","sf","htmltools", "rgdal", "shiny",
              "shinyalert", "shinyjs", "shinythemes", "shinyWidgets", "ECharts2Shiny")


## Install New Packages (uncomment and run this if needed)
new.packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load Packages
lapply(Packages, library, character.only = TRUE)

# library(shiny)
# library(tidyverse)
# library(shinythemes)
# library(leaflet)
# library(geosphere)
# library(lubridate)
# library(shinyWidgets)
# library(DT)
# library(generics)
# library(shinyalert)
# library(shinyjs)


colors <- c("red", "orange","beige", "green", 
            "blue", "purple", "pink",
            "darkgreen", "lightgreen", 
            "darkred", "lightred", 
            "darkblue", "lightblue",
            "darkpurple", "cadetblue")

DATA <- readRDS(file = "data/data_zips.rds")
RESTAURANTS <- read.csv("data/Restaurants_in_Wake_County.csv")
DISTRICTS <- readRDS("data/districts.rds")
ZIPS <- readRDS("data/raleigh_zipcode_shp.rds")
PSTATIONS <- readRDS("data/police_stations.rds")
ZIPS$GEOID10 <- as.numeric(ZIPS$GEOID10)

CATEGORIES <- sort(unique(DATA$crime_category))
CATEGORIES <- c(CATEGORIES)


by_hour <- readRDS("data/by_hour.rds")
by_day <- readRDS("data/by_day.rds")


bars <- RESTAURANTS %>% 
  filter(str_detect(str_to_lower(NAME), "bar|pub|brew|tavern|wine")) %>%
  filter(X != 0)

bar_url <- "https://upload.wikimedia.org/wikipedia/commons/7/7f/Aiga_bar.svg"
bar_icon <- makeIcon( iconUrl = bar_url, iconWidth = 10, iconHeight = 10 )

police_url <- "https://upload.wikimedia.org/wikipedia/commons/3/38/Map_icon_-_Police.png"
police_icon <- makeIcon( iconUrl = police_url, iconWidth = 20, iconHeight = 20 )

sidebarPanel2 <- function (..., out = NULL, width = 2) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}



ui <- fluidPage(theme = shinytheme("lumen"),
                loadEChartsLibrary(),
                useShinyalert(),
                useShinyjs(),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                ),
                navbarPage(title = "Info",
                           tabPanel(title = "Heat Map", 
                                    sidebarLayout(
                                      sidebarPanel2(fluid = FALSE,
                                        sliderTextInput("chosen_year","Select Year" , 
                                                        choices = c(2014:2021, "ALL"), 
                                                        selected = 2014,
                                                        animate = TRUE, width = "100%")
                                      ,
                                      radioButtons("chosen_cat","Select Category", choices=CATEGORIES, selected = CATEGORIES[1])
                                  
                                      # out = h4(("Bars and drunk crime over the years."))
                                      ),
                                      mainPanel(
                                        leafletOutput(outputId = "heatMapSliderText", width = 1000, height = 800)
                                      )
                                    )
                           ),
                           tabPanel(title = "Pie Chart",
                                    sidebarLayout(
                                      sidebarPanel2(fluid = FALSE,
                                                    radioButtons("chosen_cat2","Select Category", choices=c("ALL",CATEGORIES), selected = "ALL")
                                                    
                                                    # out = h4(("Bars and drunk crime over the years."))
                                      ),
                                    mainPanel(
                                      htmlOutput("pie1_title"),
                                      tags$div(id="pie1", style="width:100%;height:400px;"),
                                      deliverChart(div_id = "pie1"),
                                      htmlOutput("pie2_title"),
                                      tags$div(id="pie2", style="width:100%;height:400px;"),
                                      deliverChart(div_id = "pie2")
                                    )
                                    )
                           )
                )
)             
                


server <- function(input, output) {
  
  #### Information
  
  output$info <- renderText({
    info
  })
  
  output$pie1_title <- renderText({
    paste("<h4><center><b><font color=\"#052360\">", "Crime by hour for", chosen_cat2())
  })
  
  output$pie2_title <- renderText({
    paste("<h4><center><b><font color=\"#052360\">", "Crime by day of week for", chosen_cat2())
  })
  
  #### "Heat Map"
  
  chosen_year <- eventReactive(input$chosen_year,{    # Date input
    input$chosen_year
  })
  
  chosen_cat <- eventReactive(input$chosen_cat,{    # Date input
    input$chosen_cat
  })
  
  chosen_cat2 <- eventReactive(input$chosen_cat2,{    # Date input
    input$chosen_cat2
  })

  output$heatMapSliderText <- renderLeaflet({
    drunk <- DATA %>%
      # filter(crime_description == "Traffic/DWI (Driving While Impaired)") %>%
      {if(chosen_year() != "ALL") filter(., reported_year == as.numeric(chosen_year() )) else .} %>% 
      # filter(reported_year == chosen_year()) %>%
      {if(chosen_cat() != "ALL") filter(., crime_category %in% chosen_cat()) else .}
    
    districts <- DISTRICTS
    zips <- ZIPS
    
    districts_drunk <- drunk %>% 
      group_by(district) %>% 
      count() %>%
      full_join(.,districts@data, by = c("district" = "DISTRICT"))
    districts@data <- districts_drunk
    
    # zips_drunk <- drunk %>% 
    #   group_by(zips) %>% 
    #   count() %>%
    #   full_join(.,zips@data, by = c("zips" = "GEOID10"))
    # zips@data <- zips_drunk
    
    
    
    max <- max(districts$n)
    min <- min(districts$n)
    quint <- (max-min)/5
    bins <- c(min, min+quint, min+2*quint,min+3*quint,min+4*quint, max)
    pal <- colorBin("YlOrRd", domain = districts$n, bins = bins)
    
    # max <- max(zips$n)
    # min <- min(zips$n)
    # quint <- (max-min)/5
    # bins2 <- c(min, min+quint, min+2*quint,min+3*quint,min+4*quint, max)
    # pal2 <- colorBin("YlOrRd", domain = zips$n, bins = bins2)
    
    dlabels <- sprintf(
      "<strong>%s</strong><br/>%g crimes",
      districts$district, districts$n
    ) %>% lapply(htmltools::HTML)
    
    # zlabels <- sprintf(
    #   "<strong>%s</strong><br/>%g crimes",
    #   zips$zips, zips$n
    # ) %>% lapply(htmltools::HTML)
    
    
    bar_labels <- sprintf("%s", bars$NAME) %>% lapply(htmltools::HTML)
    drunk_labels <- sprintf("%s", drunk$reported_date) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = districts, color = "black", fillColor = ~pal(n), fillOpacity = 0.3, label = dlabels, group = "districts") %>% 
      # addPolygons(data = zips, color = "black", fillColor = ~pal2(n), fillOpacity = 0.3, label = zlabels, group = "zips") %>% 
      addMarkers(lng=bars$X, lat=bars$Y, icon = bar_icon, popup = bar_labels, group = "bars") %>% 
      addMarkers(lng=PSTATIONS@coords[,1], lat=PSTATIONS@coords[,2], icon = police_icon, group = "police") %>%
      addCircleMarkers(lng=drunk$X, lat=drunk$Y, color = "red", radius = 1, opacity = .3, popup = drunk_labels, group = "individual") %>% 
      # addLayersControl(baseGroups = c("Counties","Regions"), options = layersControlOptions(collapsed = F), overlayGroups = "Region Overlay")%>%
      addLayersControl(position = "topright",
        baseGroups = c("districts","individual"),
        overlayGroups = c("bars", "police"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # addLegend(color = "blue", labels = "Bars", group = "bars", position = "topright") %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
      # addLegend(color = "red", labels = "Drunk", group = "drunk", position = "bottomright")
  })
  
  
  observeEvent(c(chosen_cat2()), {
    renderPieChart(div_id = "pie1",
                   data = by_hour %>% 
                     filter(crime_category == chosen_cat2()) %>%
                     select(reported_hour, n) %>%
                     mutate(reported_hour = as.character(reported_hour)) %>%
                     rename(name = reported_hour, value = n)
    )
    
    renderPieChart(div_id = "pie2",
                   data = by_day %>% 
                     filter(crime_category == chosen_cat2()) %>%
                     select(reported_dayofwk, n) %>%
                     rename(name = reported_dayofwk, value = n)
    )
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)