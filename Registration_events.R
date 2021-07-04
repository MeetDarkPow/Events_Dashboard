library(dplyr)
library(countrycode)
library(echarts4r)
library(lubridate)

# reading data
df <- read.csv("participation_data.csv")
df$registration_date <- as.Date(df$registration_date)
df$country <- countrycode(df$country, "country.name", "iso2c")


# timeline - per day registrations
reg_df <- df %>%
  group_by(registration_date, country) %>%
  summarise(Count = n())

reg_df$csum <- ave(reg_df$Count, reg_df$country, FUN=cumsum)


reg_df %>% e_country_names(country) %>% 
  e_charts(country, timeline = TRUE) %>%
  e_map(csum) %>%
  e_visual_map(min =0, max = max(reg_df$csum), 
               color = c("#ba160c","#ce5b54",
                         "#bf716c", "#e89993",
                         "#f0b5b0","#f2c5c1",
                         "#f0d9d7")) %>% 
  e_tooltip() %>%
  e_title("Registrations", "Timeline") 


# cummulative world mapping
cum_df <- df
cum_df <- cum_df %>%
  group_by(country) %>%
  summarise(Count = n())
cum_df %>% e_country_names(country) %>% 
  e_charts(country) %>%
  e_map(Count) %>%
  e_visual_map(min =0, max = max(cum_df$Count), 
               color = c("#ba160c","#ce5b54",
                         "#bf716c", "#e89993",
                         "#f0b5b0","#f2c5c1",
                         "#f0d9d7")) %>% 
  e_tooltip() %>%
  e_title("Registrations", "Timeline") 


# timeline - line chart format
temp_df <- df[-c(1)]

temp_df <- temp_df %>%
  group_by(registration_date) %>%
  summarise(Registration_Count = n())
temp_df$Year <- year(temp_df$registration_date)

mycolor <- c("#4ca0c6","#003636","#a9a9a9", "#91c8c8")
temp_df %>% 
  group_by(Year) %>%
  e_charts(registration_date,timeline = TRUE) %>% 
  e_line(Registration_Count) %>%
  e_title(text = "Registrations", 
          subtext = "Timeline", 
          sublink = "#",
          left = "left", top = 4
  ) %>%
  e_y_axis(
    splitArea = list(show = TRUE),
    axisPointer = list(      show = FALSE, 
                             lineStyle = list(
                               color = "#999999",
                               width = 0.75,
                               type = "dotted"
                             ))
  ) %>%
  e_x_axis(splitArea = list(show = TRUE),
           splitLine = list(show = TRUE),
           axisLabel= list(rotate = 30,interval = 0)) %>%
  e_toolbox_feature(feature = "magicType",
                    type = list("area", "line", "bar")) %>%
  e_toolbox_feature("restore") %>%
  e_toolbox_feature(feature = "reset") %>%
  e_toolbox_feature("dataView") %>%
  e_toolbox_feature("saveAsImage") %>%
  e_animation(duration = 1000) %>%
  e_datazoom() %>%
  e_timeline_opts(
    axis_type = "category",
    playInterval = 1500,
    top = 5,
    right = 50,
    left = 200
  ) %>%
  e_tooltip(trigger = "axis") %>% e_color(mycolor)
