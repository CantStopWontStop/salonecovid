library(tidyverse)
library(magrittr)
library(shiny)
library(lubridate)
library(ggthemes)
library(extrafont)
library(leaflet)
library(rgdal)
library(highcharter)
library(formattable)

ui <- fluidPage(
  tags$style(type = "text/css", "#mapPlot {height: calc(100vh - 80px) !important;}"), 
  fluidRow(
    tabsetPanel(
      tabPanel("New Cases by Day", 
               highchartOutput("trendPlot")
               ), 
      tabPanel("Cases by District", 
               fluidRow(
                 column(8, leafletOutput("mapPlot")), 
                 column(4,  tableOutput("tablePlot"))
                 )
               )
      )
  )
)


thm <- hc_theme(
  colors = c("#9a2828", "#000000", "#767676", "#E4E4E4"),
  chart = list(
    backgroundColor = "#FFFFFF",
    style = list(
      fontFamily = "Roboto",
      color = "#000000"
    )
  ),
  title = list(
    align = "Center",
    style = list(
      fontFamily = "Roboto",
      color = "#000000",
      fontWeight = "bold"
    )
  ),
  subtitle = list(
    align = "left",
    style = list(
      fontFamily = "Roboto",
      color = "#000000",
      fontWeight = "bold"
    )
  ),
  xAxis = list(
    lineColor = "#000000",
    lineWidth = 2,
    tickColor = "#000000",
    tickWidth = 2,
    labels = list(
      style = list(
        color = "black"
      )
    ),
    title = list(
      style = list(
        color = "black"
      )
    )
  ),
  yAxis = list(
    opposite = TRUE,
    # gridLineDashStyle = "Dot",
    gridLineWidth = 2,
    gridLineColor = "#F3F3F3",
    lineColor = "#CEC6B9",
    minorGridLineColor = "#CEC6B9",
    labels = list(
      align = "left",
      style = list(
        color = "black"
      ),
      x = 0,
      y = -2
    ),
    tickLength = 0,
    tickColor = "#CEC6B9",
    tickWidth = 1,
    title = list(
      style = list(
        color = "black"
      )
    )
  ),
  tooltip = list(
    backgroundColor = "#FFFFFF",
    style = list(
      color = "#000000"
    )
  ),
  legend = list(
    layout = "horizontal",
    align = "left",
    verticalAlign = "top",
    itemStyle = list(
      color = "#3C3C3C"
    ),
    itemHiddenStyle = list(
      color = "#606063"
    )
  ),
  credits = list(
    style = list(
      color = "#666"
    )
  ),
  labels = list(
    style = list(
      color = "#D7D7D8"
    )
  ),
  
  drilldown = list(
    activeAxisLabelStyle = list(
      color = "#F0F0F3"
    ),
    activeDataLabelStyle = list(
      color = "#F0F0F3"
    )
  ),
  
  navigation = list(
    buttonOptions = list(
      symbolStroke = "#DDDDDD",
      theme = list(
        fill = "#505053"
      )
    )
  ),
  legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
  background2 = "#505053",
  dataLabelsColor = "#B0B0B3",
  textColor = "#C0C0C0",
  contrastTextColor = "#F0F0F3",
  maskColor = "rgba(255,255,255,0.3)"
)

# Server logic
server <- function(input, output) {
  output$trendPlot <- renderHighchart({

WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 
salone <- WHO_COVID_19_global_data %>%
      filter(Country == 'Sierra Leone') %>%
      filter(Date_reported > ymd(20200331)) %>% 
      mutate(Date =ymd(Date_reported))

througDate <- max(salone$Date_reported)
dataAs <- today()

salonePlot <- hchart(salone, "column", hcaes(x= Date, y=New_cases))%>% 
      hc_yAxis(
        title = list(text = "New Cases"),
        max = 100,
        opposite = FALSE
      ) %>% 
      hc_title(
        text = "Trend in COVID-19 Cases in Sierra Leone",
        align = "center") %>%
      hc_caption(
        text = paste0("Data Through: ", througDate,"<br>Data As Of: ", dataAs,"<br>Data Source: World Health Organization<br>Powered by Afromation"),
        style = list(fontSize = "12px")
      ) 
    
   salonePlot <-  salonePlot %>% 
      hc_add_theme(thm)
    
    salonePlot
  })
  
  output$mapPlot <- renderLeaflet({
    saloneMapData <- read_csv('saloneMapData.csv') 
    
  salonePoly <- readOGR('https://raw.githubusercontent.com/skokenes/sierra-leone-district-geojson/master/SLE_adm2.json')
    
  saloneMap <-  leaflet(salonePoly) %>% 
      addProviderTiles('CartoDB.Positron')  %>%
      setView(-11.79931640625, 8.504970203442133, zoom = 8) %>% 
      addLabelOnlyMarkers(
        data = saloneMapData, ~Longitude, ~Latitude, 
        label =~saloneMapData$`Map Label`, 
        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
    if (childCount < 100) {  
      c = 'rgba(154,40,40, 0.5);'
    } else if (childCount < 1000) {  
      c = 'rgba(240, 194, 12, 1);'  
    } else { 
      c = 'rgba(241, 128, 23, 1);'  
    }    
    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

  }")),
        labelOptions = labelOptions(
          noHide = T, 
          direction = 'auto',
          style = list(
            "color" = "black",
            "font-family" = "arial",
            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
            "font-size" = "12px",
            "border-color" = "rgba(0,0,0,0.5)"
          )
        )
      ) %>% 
      addPolygons( weight = .8,  color = "#5c636b" ,fill = FALSE, stroke = TRUE, smoothFactor = 0.8) 
  })
  
  output$tablePlot<- renderTable({
    saloneMapData <- read_csv('saloneMapData.csv')
    saloneMapData <- saloneMapData %>%
      select(District,Cases,`Cases per 100,000`)
    })
}

# Complete app with UI and server components
shinyApp(ui, server)
