library(zoo)
WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 

salone <- WHO_COVID_19_global_data %>%
  filter(Country == 'Sierra Leone') %>%
  filter(Date_reported > ymd(20200331)) %>% 
  mutate(Date =ymd(Date_reported)) %>% 
  mutate(seven_day_avg_cases = rollmean(New_cases, 7, align = "right", na.pad = TRUE),
         seven_day_avg_deaths = rollmean(New_deaths, 7, align = "right", na.pad = TRUE)) %>% 
  
  view()


salone %>% 
  ggplot(aes(x = Date_reported)) +
  geom_col(aes(y=New_cases), fill= "#355680") +
  geom_line(aes(y=seven_day_avg_cases), size = 1.5) +
  labs(
    title = "Trend in COVID-19 Cases in Sierra Leone", 
    x = "Date Reported",
    subtitle = format(max(salone$Date_reported), "%b %d,  %Y"),
    y = "New Cases"
  ) +
  scale_x_date(date_labels = "%b %d,  %Y") +
  theme(
    axis.ticks.y.left  = element_blank(),
    axis.text = element_text(size= 12),
    title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  theme_hc()



salone %>% 
  ggplot(aes(x = Date_reported)) +
  geom_col(aes(y=New_deaths), fill= "#5a817e") +
  geom_line(aes(y=seven_day_avg_deaths), size = 1.5) +
  labs(
    title = "Trend in COVID-19 Deaths in Sierra Leone", 
    x = "Date Reported",
    subtitle = format(max(salone$Date_reported), "%b %d,  %Y"),
    y = "New Deaths"
  ) +
  scale_x_date(date_labels = "%b %d,  %Y") +
  theme(
    axis.ticks.y.left  = element_blank(),
    axis.text = element_text(size= 12),
    title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  theme_hc()
