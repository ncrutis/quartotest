setwd("/home/ni3853/Documents/quartoproj")


library(dplyr)
library(readr)
starwars <- starwars
#raw data
starwars <- starwars %>% filter(name != "Jabba Desilijic Tiure")

#do some cleaning


#save to clean data folder
write_csv(starwars, file = "./data/clean_data/starwars.csv")

