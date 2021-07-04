library(dplyr)
library(countrycode)
library(echarts4r)

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
