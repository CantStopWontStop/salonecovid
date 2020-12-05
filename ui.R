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
library(rsconnect)

ui <- fluidPage(
  tags$style(type = "text/css", "#mapPlot {height: calc(100vh - 80px) !important;}"), 
  fluidRow(
    tabsetPanel(
      tabPanel("New Cases by Day", 
               highchartOutput("casesPlot")
      ),
      tabPanel("New Deaths by Day", 
               highchartOutput("deathPlot")
      ), 
      tabPanel("Cases by District", 
               fluidRow(
                 column(8, 
                        fluidRow(
                          leafletOutput("mapPlot")
                          ),
                        fluidRow(
                          column(1,
                                 HTML("")
                          ),
                          column(11,
                                 HTML("  Data as of 4 Dec 2020 | Source: Sierra Leone Ministry of Information and Communication")
                          )
                        )
                      ), 
                 column(4,  tableOutput("tablePlot"))
            )
      )
    )
  )
)
