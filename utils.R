try(library(tidyverse))
try(library(rstan))
try(library(RPostgreSQL))
try(library(dplyr))
try(library(tidyr))
try(library(lubridate))
try(library(pitchRx))
try(library(data.table))
try(library(dplyr))
try(library(ggplot2))
# library(ngspatial)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

####Loading into the database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = '192.168.1.71', user="kevin")





# Functions ----


convert_outcomes <- function(event) {
  
  case_when(
    event == "Walk" ~ "BB",
    event == "Strikeout" ~ "K",
    event == "Flyout" ~ "F",
    event == "Home Run" ~ "HR",
    event == "Groundout" ~ "G",
    event == "Lineout" ~ "F",
    event == "Pop Out" ~ "F",
    event == "Hit By Pitch" ~ "HBP",
    event == "Single" ~ "1B",
    event == "Grounded Into DP" ~ "G",
    # event == "Catcher Interference" ~ NA,
    # event == "Fan interference" ~ NA,
    event == "Sac Fly" ~ "F",
    event == "Double" ~ "2B",
    # event == "Runner Out" ~ NA,
    # event == "Field Error" ~ "E",
    event == "Forceout" ~ "G",
    event == "Sac Fly DP" ~ "F",
    event == "Double Play" ~ "G",
    event == "Triple" ~ "3B",
    event == "Fielders Choice" ~ "G", # this means the batter is safe and the runner is out
    event == "Fielders Choice Out" ~ "G",
    # event == "Bunt Groundout" ~ NA,
    event == "Sac Bunt" ~ "G",
    # event == "Batter Interference" ~ NA,
    event == "Strikeout - DP" ~ "K",
    # event == "Bunt Pop Out" ~ NA,
    event == "Intent Walk" ~ "BB",
    # event == "Bunt Lineout" ~ NA,
    event == "Triple Play" ~ "G"
  )
}
