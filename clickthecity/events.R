setwd('/Users/maggiesaavedra/Moonlight/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)

# url
url <- 'https://www.clickthecity.com/events'

# read wholepage 
webpage <- read_html(url)

title_data <- html_nodes(webpage, "p")
title_ <- html_attrs(title_data)
