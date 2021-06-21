library(readr)
rgroups2 <- read_csv("https://raw.githubusercontent.com/benubah/r-community-explorer/master/docs/data/rugs.csv")
rugs_delete <- read.csv("Data/rugs_delete.csv")

id_delete <- rugs_delete$X
rgroups2 <- rgroups2[-c(id_delete),]
