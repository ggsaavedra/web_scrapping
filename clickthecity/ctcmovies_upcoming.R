setwd('/Users/maggiesaavedra/GitHub/web_scrapping/clickthecity/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

# url
url <- 'https://www.clickthecity.com/movies/upcoming-movies.php'
api_url = 'http://database.gekkowebhosting.com/api-mega/post-movie'

# read previous files
prev <- fread('src/movies_upcoming.csv')

# read wholepage 
webpage <- read_html(url)

title_data <- html_nodes(webpage, "#maincontent h3 a")
title_ <- unique(html_attrs(title_data))

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
                               no = 'NULL')
      if(genre=='NULL'){break}
      score <- html_text(html_node(subpage, "#details div:nth-child(1)")) # get score
      
      opening_date <- html_text(html_node(subpage, "#details div:nth-child(2)")) # get score
      
      credits <- ifelse(test = length(html_text(html_nodes(subpage, ".moviedetail"))),
                        yes = html_text(html_nodes(subpage, ".moviedetail")),
                        no = 'NULL')
      
      credits <- gsub('Main Cast', 'Main Cast: ', credits)
      credits <- gsub('Director', 'Director: ', credits)
      credits <- gsub('Writer', 'Writer: ', credits)
      credits <- gsub('Released By', 'Released By: ', credits)
      
      image_data <- ((html_attrs(html_nodes(subpage, "#poster .placeholder")))[[1]])['style']
      image <- ifelse(test = length(substring(((strsplit((strsplit(image_data, 'url'))$style[2], ') '))[[1]])[1], 2)),
                      yes = substring(((strsplit((strsplit(image_data, 'url'))$style[2], ') '))[[1]])[1], 2),
                      no = 'NULL')
      
      details <- 'NA'
      status <- 'upcoming'
      cinema_name <- 'NA'
      mtrcb_ratings <- 'NA'
      trailer_link <- 'NA'
      
      # put in one data table 
      movies <- as.data.table(cbind(title,
                                    genre,
                                    image,
                                    details,
                                    credits,
                                    opening_date,
                                    cinema_name,
                                    status,
                                    mtrcb_ratings,
                                    score,
                                    trailer_link))
      movies_upcoming <- rbind(movies_upcoming, movies)
      
      if (is.na(match(title, prev$title))){
        
        title <- gsub(' ', '%20', title)
        genre <- gsub(' ', '%20', genre)
        image <- image
        details <- 'NA'
        credits <- gsub('\n', '%0A', (gsub(' ', '%20', credits)))
        opening_date <- gsub(' ', '%20', opening_date)
        cinema_name <- '%20'
        status <- 'upcoming'
        mtrcb_ratings <- 'NA'
        score <- gsub(' ', '%20', score)
        trailer_link <- 'NA'
        
        POST(url = paste0(api_url,
                          '?title=', title,
                          '&genre=', genre,
                          '&image=', image,
                          '&details=', details,
                          '&credits=', credits,
                          '&opening_date=', opening_date,
                          '&cinema_name=', cinema_name,
                          '&status=', status,
                          '&mtrcb_ratings=', mtrcb_ratings,
                          '&score=', score,
                          '&trailer_link=', trailer_link,
                          '&secret_key=POST-megadb-547778-452220-870001'))
        
        print(paste('done with NEW MOVIE entitled', (title_[[i]])['title']))
        
      }else{
        print(paste((title_[[i]])['title'], 'already exists'))
      }
}

fwrite(movies_upcoming, 'directory/movie_upcoming.csv', row.names = FALSE, append = FALSE)
