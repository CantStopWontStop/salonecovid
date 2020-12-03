library(tidyverse)
library(magrittr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(lubridate)
library(ggthemes)
library(extrafont)
library(highcharter)


WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 

# select Salone data
salone <- WHO_COVID_19_global_data %>%
  filter(Country == 'Sierra Leone') %>%
  filter(Date_reported > ymd(20200331)) %>% 
  mutate(Date =ymd(Date_reported))

saloneMap <- saloneMapData %>% 
  group_by(Province) %>%
  summarise(Province, sum(saloneMapData$Cases)) %>% 
  
  sum(Cases)

pepper <- WHO_COVID_19_global_data %>%
  filter(Country == 'Sierra Leone' | Country == 'Liberia' | Country == 'Guinea') %>%
  filter(Date_reported > ymd(20200331))

data(salone)
dat <- data_to_boxplot(pokemon, height)
highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat)
dat <- data_to_boxplot(pokemon, height, type_1, name = "height in meters")
highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat)

 output
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
    text = "Data Source: World Health Organization",
    style = list(fontSize = "12px")
  )
 

  salonePlot %>% 
    hc_add_theme(hc_theme_bloom())
  
  mapdata <- get_data_from_map(download_map_data("countries/sl/sl-all"))
  
  glimpse(mapdata)
  
  hcmap("countries/sl/sl-all")
  