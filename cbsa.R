# load packages
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
library(maps)
library(leaflet)
#font_import()


cbsaPopRaw<- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv") 

cities <- c('New York-Newark-Jersey City, NY-NJ-PA','Dallas-Fort Worth-Arlington, TX','Houston-The Woodlands-Sugar Land, TX','Washington-Arlington-Alexandria, DC-VA-MD-WV','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Atlanta-Sandy Springs-Alpharetta, GA','Charlotte-Concord-Gastonia, NC-SC'
)

cbsaPop <- cbsaPopRaw %>% 
  filter(NAME %in% cities) %>% 
  mutate(Population = POPESTIMATE2019)%>% 
  select(CBSA,NAME,Population,STCOU) %>% 
  arrange(desc(Population))


cbsaCaseRaw <- read_csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')
cbsaDeathsRaw <- read_csv('https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv')

codes <- cbsaPop$CBSA


counties <- cbsaPopRaw %>% 
  filter(CBSA %in% codes) %>% 
  filter( !is.na(STCOU)) %>% 
  mutate(Population = POPESTIMATE2019)%>% 
  select(CBSA, STCOU,NAME,Population) %>% 
  left_join(cbsaPop, by = c("CBSA"='CBSA')) %>% 
  mutate(FIPS = as.numeric(STCOU.x))%>% 
  rename(County=NAME.x,'County Pop'= Population.x, City = NAME.y,"City Pop"=Population.y) %>% 
  select(-STCOU.y)


state <- state

cbsaCase <- counties %>% 
  left_join(cbsaCaseRaw, by =c("FIPS"="countyFIPS")) %>% 
  gather(-c(1:10),key ='Date', value = "Cases") %>% 
  group_by(CBSA, City, Date) %>% 
  mutate(Date =mdy(Date)) 


newCases <- cbsaCase %>% 
  group_by(CBSA, City, Date) %>% 
  summarise(`total case` = sum(Cases)) %>% 
  mutate()

newCases <- newCases %>%
  mutate(`new case` = order_by(Date, `total case`- lag(`total case`))) %>%
  mutate(`new case` = ifelse(is.na(`new case`), 0, `new case`))

names(newCases$City) <- c('Atlanta', 'Charlotte', 'Dallas', 'Houston','New York / New Jersey', 'Philadelphia', 'DC-Maryland-Virgina')




hchart(newCases, "column", hcaes(x= Date, y=`new case`))%>% 
  hc_yAxis(
    title = list(text = "New Cases"),
    max = 100,
    opposite = FALSE
  ) %>% 
  hc_title(
    text = "Trend in COVID-19 Metropolitan Areas",
    align = "center") %>%
  hc_caption(
    #text = paste0("Data Through: ", througDate,"<br>Data As Of: ", dataAs,"<br>Data Source: World Health Organization<br>Powered by Afromation"),
    style = list(fontSize = "12px",
                 align = "right")
  ) 

salonePlot <-  ggplotly(salonePlot, tooltip=c("text")) %>% 
  style(hoverlabel = label) %>%
  layout(font = font) %>%
  config(displayModeBar = FALSE)


leaflet(newCases)

cbsa <- reactive({
  c <- cbsaCase[cbsaCase$CBSA == input$cbsaCode, ]
  c$Date <-c$Date
  c$Name <- c$Name.y
  c$NAME.y <- c$cbsaName
  return(c)
})

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Salone Against COVID"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("News", tabName = "news", icon = icon("newspaper-o")),
      menuItem("FAQs", tabName = "question", icon = icon("question-circle-o")),
      menuItem("Resources", tabName = "resources", icon = icon("calendar"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
          fluidRow(
            box(width= 12, plotlyOutput("salonePlot"))
            ),
          fluidRow(
            tabBox(
              side = "right", height = "250px",
              selected = "Guinea",
              tabPanel("Guinea", plotlyOutput("guineaPlot")),
              tabPanel("Liberia", plotlyOutput("pepperPlot"))
            ),
            box(
              
              plotlyOutput("cbsaPlot")
#              side = "right", height = "250px",
 #             selected = "",
  #            tabPanel("Atlanta", plotlyOutput("atlPlot")),
   #           tabPanel("", plotlyOutput("queenPlot")),
    #          tabPanel(, plotlyOutput("triPlot")),
     #         tabPanel("Philadelphia", plotlyOutput("phillyPlot")),
      #        tabPanel("Washington D.C. / MD / VA", plotlyOutput("dmvPlot"))
            )
          
      ),
      tabItem( tabName = "news",
        fluidRow(
          h2("Dashboard tab content"),
          htmlOutput("frame")
        )
      ),
      tabItem(tabName = "question",
        fluidRow(
          plotlyOutput("plot1")
        )
      )
    )
  )
)
)


# Define server function  
server <- function(input, output) {
  
  WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 
  
  # select Salone data
  salone <- WHO_COVID_19_global_data %>%
    filter(Country == 'Sierra Leone') %>%
    filter(Date_reported > ymd(20200331))
  
  pepper <- WHO_COVID_19_global_data %>%
    filter(Country == 'Liberia') %>%
    filter(Date_reported > ymd(20200331)) 
  
  guinea <- WHO_COVID_19_global_data %>%
    filter(Country == 'Guinea') %>%
    filter(Date_reported > ymd(20200331))
  
  
  font = list(
    family = "DM Sans",
    size = 15,
    color = "white"
  )
  label = list(
    bgcolor = "#232F34",
    bordercolor = "transparent",
    font = font
  )
  
  output$salonePlot <- renderPlotly({
    salonePlot <- salone %>% 
      ggplot(aes(x = Date_reported, y= New_cases, text = paste0("Date: ",Date_reported, "\nCases: ", New_cases))
             ) + 
      geom_col() +
      labs(
        title = "Trend in COVID-19 Cases in Sierra Leone",
        x = "Date",
        y = "New Cases"
      ) +
      theme_tufte()
   
 
    salonePlot <-  ggplotly(salonePlot, tooltip=c("text")) %>% 
      style(hoverlabel = label) %>%
      layout(font = font) %>%
      config(displayModeBar = FALSE)
    

  })
  
  output$pepperPlot <- renderPlotly({
    pepperPlot <- pepper %>% 
      ggplot(aes(x = Date_reported, y= New_cases, text = paste0("Date: ",Date_reported, "\nCases: ", New_cases))
      ) + 
      geom_col() +
      labs(
        title = "Liberia",
        x = "Date",
        y = "New Cases"
      ) +
      theme_tufte()
    
    
    pepperPlot <-  ggplotly(pepperPlot, tooltip=c("text")) %>% 
      style(hoverlabel = label) %>%
      layout(font = font) %>%
      config(displayModeBar = FALSE)
    
    
  })
  
  output$guineaPlot <- renderPlotly({
    guineaPlot <- guinea %>% 
      ggplot(aes(x = Date_reported, y= New_cases, text = paste0("Date: ",Date_reported, "\nCases: ", New_cases))
      ) + 
      geom_col() +
      labs(
        title = "Guinea",
        x = "Date",
        y = "New Cases"
      ) +
      theme_tufte()
    
    
    guineaPlot <-  ggplotly(guineaPlot, tooltip=c("text")) %>% 
      style(hoverlabel = label) %>%
      layout(font = font) %>%
      config(displayModeBar = FALSE)
    
    
  })
  
output$cbsaPlot <- renderPlotly({
  key <- cbsa()$code
  cbsaPlot <- cbsa()@data %>% 
    ggplot(aes(x = Date, y= Cases,key = key, text = paste0("Date: ",Date, "\nCases: ", Cases))
    ) + 
    geom_col() +
    labs(
      title = Name.y,
      x = "Date",
      y = "New Cases"
    ) +
    theme_tufte()
  
  
  cbsaPlot <-  ggplotly(cbsaPlot, tooltip=c("text")) %>% 
    style(hoverlabel = label) %>%
    layout(font = font) %>%
    config(displayModeBar = FALSE)
  cbsaPlot
})
  
output$frame <- renderUI({
  
    tags$iframe(
      src = "https://www.youtube.com/embed/cGWL1sK6OxA", 
      height=600, 
      width=535)
 

})
} 

# server


# Create Shiny object
shinyApp(ui = ui, server = server)

