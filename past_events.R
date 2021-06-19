# Read dataset
past_event <- readRDS("Data/all_past_R_events.rds")
past_event <- past_event %>%
  distinct(id, .keep_all = TRUE)


# ValueBox data
total_events <- length(past_event$id)
total_rsvp <- sum(past_event$yes_rsvp_count)
total_cities <- length(unique(past_event$venue_city))
total_countries <- length(unique(past_event$venue_country_name))


# R Events Across Regions
temp_df_region <- past_event
temp_df_region$group_region <- sapply(strsplit(temp_df_region$group_region, "/"), "[", 1)
event_by_region <- temp_df_region %>%
  group_by(group_region) %>%
  summarise(Events_freq = n())


# R Event Attendance Across regions by rsvp
attendee_event_by_region <- temp_df_region %>%
  group_by(group_region) %>%
  summarise(attendees = sum(yes_rsvp_count))


# Top 40 destinations for R events
top_dest_city <- past_event %>%
  group_by(venue_city) %>%
  summarise(Events_freq = n())
top_dest_city <- top_dest_city[order(top_dest_city$Events_freq, decreasing = TRUE), ]
top_dest_city <- top_dest_city[complete.cases(top_dest_city), ]
top_dest_city <- head(top_dest_city, 40)


# Cumulative growth of R events
past_event$local_date <- ymd(past_event$local_date)
past_event <- arrange(past_event, local_date)

cumm_event_count <- past_event %>%
  group_by(local_date) %>%
  summarise(Events = n())
cumm_event_count <- cumm_event_count %>% 
  mutate(csum = cumsum(Events))


# Top Destinations for R events | Event count per city per region 
name <- event_by_region$group_region

df1 <- temp_df_region[temp_df_region$group_region==name[1],]
df1 <- df1 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df1 <- df1[complete.cases(df1), ]
df1 <- df1[order(df1$Events_freq, decreasing = FALSE), ]

df2 <- temp_df_region[temp_df_region$group_region==name[2],]
df2 <- df2 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df2 <- df2[complete.cases(df2), ]
df2 <- df2[order(df2$Events_freq, decreasing = FALSE), ]

df3 <- temp_df_region[temp_df_region$group_region==name[3],]
df3 <- df3 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df3 <- df3[complete.cases(df3), ]
df3 <- df3[order(df3$Events_freq, decreasing = FALSE), ]

df4 <- temp_df_region[temp_df_region$group_region==name[4],]
df4 <- df4 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df4 <- df4[complete.cases(df4), ]
df4 <- df4[order(df4$Events_freq, decreasing = FALSE), ]

df5 <- temp_df_region[temp_df_region$group_region==name[5],]
df5 <- df5 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df5 <- df5[complete.cases(df5), ]
df5 <- df5[order(df5$Events_freq, decreasing = FALSE), ]

df6 <- temp_df_region[temp_df_region$group_region==name[6],]
df6 <- df6 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df6 <- df6[complete.cases(df6), ]
df6 <- df6[order(df6$Events_freq, decreasing = FALSE), ]

df7 <- temp_df_region[temp_df_region$group_region==name[7],]
df7 <- df7 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df7 <- df7[complete.cases(df7), ]
df7 <- df7[order(df7$Events_freq, decreasing = FALSE), ]

df8 <- temp_df_region[temp_df_region$group_region==name[8],]
df8 <- df8 %>%
  group_by(venue_city) %>%
  summarise(Events_freq=n())
df8 <- df8[complete.cases(df8), ]
df8 <- df8[order(df8$Events_freq, decreasing = FALSE), ]


# Top Destinations for Event Attendance by rsvp by country
temp_df_country <- past_event
temp_df_country$venue_country_name <- countrycode(temp_df_country$group_country, "iso2c", "country.name")
td_by_country <- temp_df_country %>%
  group_by(venue_country_name) %>%
  summarise(attendees = sum(yes_rsvp_count))
td_by_country <- td_by_country[order(td_by_country$attendees, decreasing = FALSE), ]
top_dest_country <- tail(td_by_country, 20)


# Events by Country
events_by_country <- temp_df_country %>%
  group_by(venue_country_name) %>%
  summarise(Events_freq = n())
events_by_country <- events_by_country[order(events_by_country$Events_freq, decreasing = FALSE), ]
events_by_country <- tail(events_by_country, 20)


# Data-table
past_event$venue_country_name <- countrycode(past_event$group_country, "iso2c", "country.name")
past_event <- past_event %>%
  arrange(local_date)
display_df <- data.frame(Event_name = past_event$name, Date=past_event$created, City=past_event$venue_city, 
                         Country=past_event$venue_country_name, RSVP_count=past_event$yes_rsvp_count)

