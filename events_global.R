#general events
past_events_df <- readRDS(file = "Data/all_past_R_events.rds")


#satRdays events 
library(dplyr)
library(ggplot2)
satrday <- read.csv("Data/pastsatrdays.csv", encoding = "UTF-8")
total_events <- length(satrday)

satrday_year <- satrday %>% group_by(Year) %>% summarise(count = n())
satrday_quarter <- satrday %>% group_by(Quarter) %>% summarise(count = n())
satrday_city <- satrday %>% group_by(City) %>% summarise(count = n())
satrday_country <- satrday %>% group_by(Country) %>% summarise(count = n())
satrday_continent <- satrday %>% group_by(Continent) %>% summarise(count = n())

satrday_sponsors <- satrday$Sponsors
sponsorlist <- paste(satrday_sponsors, collapse = ",")
sponsorvector <- strsplit(sponsorlist, ",")[[1]]
sponsortable <- table(trimws(sponsorvector))
sponsordf <- as.data.frame(sponsortable)
colnames(sponsordf) <- c("sponsor_name", "Freq")
sponsordf <- sponsordf[!(sponsordf$sponsor_name=="NA"),]
number_of_sponsors <- length(sponsordf$sponsor_name)
sponsordf <- sponsordf[order(sponsordf$Freq, decreasing = TRUE), ]
most_active_sponsors <- head(sponsordf,4)

satrday_year.vis <- ggplot(satrday_year,mapping=aes(x=factor(Year,levels=Year),y=count))+
  geom_col(colour = '#AAAAAA', fill = '#0099FF')+
  geom_text(aes(label=count), vjust=-0.1, color="steelblue", size=5) +
  xlab("Year") +
  ylab("Number of Events") +
  ggtitle("Yearly SatRday Events") + theme_classic()
plot(satrday_year.vis)
