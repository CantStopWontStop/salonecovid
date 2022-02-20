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
  tags$style("h4{color: #145389; font-size: 40px; text-align:center; margin-bottom:2px}"),
  tags$style("h5{color: #161616; font-size: 18px; text-align:center}"),
  tags$style("h3{color: #161616; text-align:center}"),
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
                                 HTML("  Data as of 10 Jan 2021 | Source: Sierra Leone Ministry of Information and Communication")
                                 )
                          )
                        ),
                 column(4,  tableOutput("tablePlot"))
                 )
               ),
      tabPanel("Summary",
               fluidRow(h3("")),
               fluidRow(h5("Total Cases"),
                        h4(textOutput("casesOutput"))
                        ),
               fluidRow(h5("New Cases - Last 24 Hours"),
                        h4(textOutput("casesNewOutput"))
                        ),
               fluidRow(h5("New Cases - 7-Day Average"),
                        h4(textOutput("avgCaseNewOutput"))
                        ),
               fluidRow(h5("Total Deaths"),
                        h4(textOutput("deathsOutput"))
                        ),
               fluidRow(h5("New Deaths - Last 24 Hours"),
                        h4(textOutput("deathsNewOutput"))
                        ),
               fluidRow(h5("New Deaths - 7-Day Average"),
                        h4(textOutput("avgDeathNewOutput"))
                        )
               )
      )
    )
  )
  
