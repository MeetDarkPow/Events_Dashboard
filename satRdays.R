#satRdays events 

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

