useR_event <- read.csv("Data/sponsors_updated_v3.csv")
df <- useR_event[-c(2:3, 5:8)]
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
df <- df[df$URL != "",]


library(dplyr)
sponsor_df <- df %>%
  group_by(Sponsor, URL) %>%
  summarise(Count = n())

sponsor_df <- sponsor_df[order(sponsor_df$Count, decreasing = TRUE), ]

library(reactable)
library(htmltools)
sponsor_df %>% 
  reactable(
    .,
    pagination = TRUE,
    showPageSizeOptions = TRUE,
    highlight = TRUE,
    defaultSorted = "Count",
    defaultColDef = colDef(headerClass = "header", align = "left"),
    columns = list(
      Sponsor = colDef(
        name = "Sponsor",
        width = 200,
        defaultSortOrder = "desc",
        filterable = TRUE,
        html = TRUE,
        cell = function(value, index) {
          sprintf('<a href="%s" target="_blank">%s</a>', sponsor_df$URL[index], value)
        }
      ),
      URL = colDef(
        show = FALSE
      ),
      Count = colDef(
        name = "useR! Events Sponsored",
        defaultSortOrder = "desc",
        cell = function(value) {
          width <- paste0(value * 100 / max(sponsor_df$Count), "%")
          value <- format(value, big.mark = ",")
          value <- format(value, width = 9, justify = "right")
          bar <- div(
            class = "bar-chart",
            style = list(marginRight = "6px"),
            div(class = "bar", style = list(width = width, backgroundColor = "#FFA500"))
          )
          div(class = "bar-cell", span(class = "number", value), bar)
        }
      )),
    compact = TRUE,
    bordered = TRUE,
    class = "categories-tbl"
  )
