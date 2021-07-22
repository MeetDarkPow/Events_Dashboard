library(NLP)
library(tm)
library(echarts4r)

# WordClous - Keynotes
keynotes_df <- read.csv("Data/keynotes.csv")

# converting data to string
data <- toString(keynotes_df$keynote)
data <- gsub('[[:punct:] ]+',' ',data)

# cleaning data
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

# creating a matrix for tabulation of word with respective frequency 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
wdcld_keynotes <- data.frame(word = names(v),freq=v)
wdcld_keynotes %>% 
  head(100) %>%
  e_color_range(freq, color, colors = "#435f9c") %>% 
  e_charts() %>% 
  e_cloud(word, freq, color, shape = "circle")

#######################################################################################################

# WordClous - Tutorials
tutorials_df <- read.csv("Data/tutorials.csv")

# converting data to string
data <- toString(tutorials_df$title)
data <- gsub('[[:punct:] ]+',' ',data)

# cleaning data
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

# creating a matrix for tabulation of word with respective frequency 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
wdcld_tutorials <- data.frame(word = names(v),freq=v)
wdcld_tutorials %>% 
  head(100) %>%
  e_color_range(freq, color, colors = "#435f9c") %>% 
  e_charts() %>% 
  e_cloud(word, freq, color, shape = "circle")
