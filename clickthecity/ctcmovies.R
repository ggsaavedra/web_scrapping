setwd('/Users/maggiesaavedra/Moonlight/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)

# url
url <- 'https://www.clickthecity.com/movies/now-showing.php'

# read wholepage
webpage <- read_html(url)

title_data <- html_nodes(webpage, "#maincontent h3 a")
title_ <- html_attrs(title_data)

genre_data <- html_nodes(webpage, "h3+ div")
genre_ <- c(html_text(genre_data))

details_data <- html_nodes(webpage, "div+ span , li:nth-child(3) h3+ div span")
details_ <- c(html_text(details_data))

movies_final <- c()

  for (i in 1:length(title_)){
    title <- ifelse( test = length((title_[[i]])['title'])!=0 , 
                        yes = (title_[[i]])['title'] , 
                        no = 'NULL' ) # get title 
    # print(title)
    
    genre <- genre_[i]
    
    details <- details_[i]
    
    hr <- ifelse( test = length((title_[[i]])['href'])!=0, 
                  yes = (title_[[i]])['href'], 
                  no = 'NULL')
    href <- hr # get href
    # print(href)
    
      subpage <- read_html(hr)
      mtrcb_ratings <- ifelse( test = length(html_text(html_nodes(subpage, ".mtrcb")))!=0, 
                                  yes = html_text(html_nodes(subpage, ".mtrcb")),
                                  no = 'NULL') # get mtcb_rating
      score <- html_text(html_node(subpage, "#details div:nth-child(2)")) # get score
      # print(score)
      
      credits <- ifelse(test = length(html_text(html_nodes(subpage, ".moviedetail"))),
                        yes = html_text(html_nodes(subpage, ".moviedetail")),
                        no = 'NULL')
      
      photo_data <- ((html_attrs(html_nodes(subpage, "#poster .placeholder")))[[1]])['style']
      photo <- ifelse(test = length(substring(((strsplit((strsplit(photo_data, 'url'))$style[2], ') '))[[1]])[1], 2)),
                         yes = substring(((strsplit((strsplit(photo_data, 'url'))$style[2], ') '))[[1]])[1], 2),
                         no = 'NULL')

      cinema_data <- ifelse( test = length((html_attrs(html_nodes(subpage, ".menu:nth-child(3) li:nth-child(1) a"))))!=0,
                             yes = html_attrs(html_nodes(subpage, ".menu:nth-child(3) li:nth-child(1) a")),
                             no = 'NULL')
      
      if(cinema_data=='NULL'){
        
        cinemas <- cbind(cinema_name='NULL', theater_info='NULL', phone='NULL', address='NULL')
        
      }else{
        ch <- paste0('https://www.clickthecity.com', cinema_data[[1]])
        cinema_href <- ch # get cinema href
        # print(cinema_href)
        
        cinema_page <-  read_html(ch)
        cinema_data <- html_attrs(html_nodes(cinema_page, "#cinemas a"))  # get cinema_data
        cinema_href <- paste0('https://www.clickthecity.com/movies/', (html_attrs(html_nodes(cinema_page, "#cinemas a"))))
        
        cinemas <- NULL
        for(j in 1:length(cinema_href)){
          cinema_page <- read_html(cinema_href[j])
          cinema_name <- ifelse(test = length(html_text(html_nodes(cinema_page, '#details h1')))!=0,
                                yes = html_text(html_node(cinema_page, '#details h1')),
                                no = 'NULL')
          
          if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Theater Info' &
             (html_text(html_nodes(cinema_page, 'dt'))[2])=='Phone' & 
             (html_text(html_nodes(cinema_page, 'dt'))[3])=='Address'){
            
            theater_info <- (html_text(html_nodes(cinema_page, 'dd')))[1]
            phone <- (html_text(html_nodes(cinema_page, 'dd')))[2]
            address <- (html_text(html_nodes(cinema_page, 'dd')))[3]
            
          }else if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Phone' &
                   (html_text(html_nodes(cinema_page, 'dt'))[2])=='Address'){
            
            theater_info <- 'NULL'
            phone <- (html_text(html_nodes(cinema_page, 'dd')))[1]
            address <- (html_text(html_nodes(cinema_page, 'dd')))[2]
            
          }else if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Address'){
            
            theater_info <- 'NULL'
            phone <- 'NULL'
            address <- (html_text(html_nodes(cinema_page, 'dd')))[1]
            
          }else {}
          
          cinemas <- rbind(cinemas, cbind(cinema_name, theater_info, phone, address))
          
        }
      }
            
      movies <- as.data.table(cinemas) 
      movies$title <- title
      movies$genre <- genre
      movies$details <- details
      movies$mtrcb_ratings <- mtrcb_ratings
      movies$score <- score
      movies$credits <- credits
      movies$photo <- photo
      
      movies_final <- rbind(movies_final, movies)
            
  }

movies_final$opening_date <- 'opened'
movies_final$status <- 'showing'

fwrite(movies_final, 'directory/movie_nowshowing.csv', row.names = FALSE, append = FALSE)
