# Read dataset
upcoming_event <- readRDS("Data/upcoming_revents.rds")
past_event <- past_event %>%
  distinct(id, .keep_all = TRUE)