setwd('/Users/maggiesaavedra/Moonlight/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)

# url
url <- 'https://www.clickthecity.com/movies/upcoming-movies.php'

# read wholepage 
webpage <- read_html(url)

title_data <- html_nodes(webpage, "#maincontent h3 a")
title_ <- html_attrs(title_data)

movies_upcoming <- c()

for (i in 1:length(title_)){
  title <- ifelse( test = length((title_[[i]])['title'])!=0 , 
                   yes = (title_[[i]])['title'] , 
                   no = 'NULL' ) # get title 
  # print(title)
  
  hr <- ifelse( test = length((title_[[i]])['href'])!=0, 
                yes = (title_[[i]])['href'], 
                no = 'NULL')
  href <- hr # get href
  
      subpage <- read_html(hr)
      genre <- ifelse( test = length(html_text(html_nodes(subpage, ".genre")))!=0, 
                               yes = html_text(html_nodes(subpage, ".genre")),
                               no = 'NULL') # get mtcb_rating  
      if(genre=='NULL'){break}
      score <- html_text(html_node(subpage, "#details div:nth-child(1)")) # get score
      
      opening_date <- html_text(html_node(subpage, "#details div:nth-child(2)")) # get score
      
      credits <- ifelse(test = length(html_text(html_nodes(subpage, ".moviedetail"))),
                        yes = html_text(html_nodes(subpage, ".moviedetail")),
                        no = 'NULL')
      
      photo_data <- ((html_attrs(html_nodes(subpage, "#poster .placeholder")))[[1]])['style']
      photo <- ifelse(test = length(substring(((strsplit((strsplit(photo_data, 'url'))$style[2], ') '))[[1]])[1], 2)),
                      yes = substring(((strsplit((strsplit(photo_data, 'url'))$style[2], ') '))[[1]])[1], 2),
                      no = 'NULL')
      
  
  movies <- as.data.table(cbind(cinema_name = '',
                                theater_info = '',
                                phone = '',
                                address = '',
                                title, 
                                genre,
                                details = '',
                                mtrcb_ratings = '',
                                score, 
                                credits,
                                photo,
                                opening_date,
                                status = 'upcoming'))
  
  movies_upcoming <- rbind(movies_upcoming, movies)
}

fwrite(movies_upcoming, 'directory/movie_upcoming.csv', row.names = FALSE, append = FALSE)
