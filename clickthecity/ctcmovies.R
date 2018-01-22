setwd('/Users/maggiesaavedra/GitHub/web_scrapping/clickthecity/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

# url
url <- 'https://www.clickthecity.com/movies/now-showing.php'
api_url <- 'http://database.gekkowebhosting.com/api-mega/post-movie'
api_mh <- 'http://database.gekkowebhosting.com/api-mega/post-movie-house'

# read previous files
prev <- fread('src/movies_showing.csv')
movie_house <- fread('src/moviehouse.csv')

# read wholepage
webpage <- read_html(url)

title_data <- html_nodes(webpage, "#maincontent h3 a")
title_ <- html_attrs(title_data)


details_data <- html_nodes(webpage, "div+ span , li:nth-child(3) h3+ div span")
details_ <- c(html_text(details_data))

movies_final <- c()

  for (i in 1:length(title_)){
    title <- ifelse( test = length((title_[[i]])['title'])!=0 , 
                        yes = (title_[[i]])['title'] , 
                        no = 'NULL' ) # get title 
    # print(title)
    
    details <- details_[i]
    
    hr <- ifelse( test = length((title_[[i]])['href'])!=0, 
                  yes = (title_[[i]])['href'], 
                  no = 'NULL')
    href <- hr # get href
    # print(href)
    
      subpage <- read_html(hr)
      
      genre <- ifelse( test = length(html_text(html_nodes(subpage, ".genre")))!=0, 
                       yes = html_text(html_nodes(subpage, ".genre")),
                       no = 'NULL') 
      
      mtrcb_ratings <- ifelse( test = length(html_text(html_nodes(subpage, ".mtrcb")))!=0, 
                                  yes = html_text(html_nodes(subpage, ".mtrcb")),
                                  no = 'NULL') # get mtcb_rating
      score <- html_text(html_node(subpage, "#details div:nth-child(2)")) # get score
      # print(score)
      
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

      cinema_link <- ifelse( test = length((html_attrs(html_nodes(subpage, ".menu:nth-child(3) li:nth-child(1) a"))))!=0,
                             yes = html_attrs(html_nodes(subpage, ".menu:nth-child(3) li:nth-child(1) a")),
                             no = 'NULL')
      
      if(cinema_link=='NULL'){
        
      }else{
        ch <- paste0('https://www.clickthecity.com/', cinema_link[[1]])

        cinema_page <-  read_html(ch)
        cinema_data <- html_attrs(html_nodes(cinema_page, "#cinemas a"))  # get cinema_data
        cinema_href <- paste0('https://www.clickthecity.com/movies/', (html_attrs(html_nodes(cinema_page, "#cinemas a"))))
        
        cinemas <- NULL
        for(j in 1:length(cinema_href)){
          cinema_page <- read_html(cinema_href[j])
          cinema_name_ <- ifelse(test = length(html_text(html_nodes(cinema_page, '#details h1')))!=0,
                                yes = html_text(html_node(cinema_page, '#details h1')),
                                no = ' ')
          cinemas[j] <- cinema_name_
          if(is.na(match(cinema_name_, movie_house))){
            
            if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Theater Info' &
               (html_text(html_nodes(cinema_page, 'dt'))[2])=='Phone' & 
               (html_text(html_nodes(cinema_page, 'dt'))[3])=='Address'){
              
              theater_info <- (html_text(html_nodes(cinema_page, 'dd')))[1]
              phone <- (html_text(html_nodes(cinema_page, 'dd')))[2]
              address <- (html_text(html_nodes(cinema_page, 'dd')))[3]
              
            }else if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Phone' &
                     (html_text(html_nodes(cinema_page, 'dt'))[2])=='Address'){
              
              theater_info <- ' '
              phone <- (html_text(html_nodes(cinema_page, 'dd')))[1]
              address <- (html_text(html_nodes(cinema_page, 'dd')))[2]
              
            }else if((html_text(html_nodes(cinema_page, 'dt'))[1])=='Address'){
              
              theater_info <- ' '
              phone <- ' '
              address <- (html_text(html_nodes(cinema_page, 'dd')))[1]
              
            }else {}
            
            fwrite(as.data.table(cbind(cinema_name_, theater_info, phone, address)), 'src/moviehouse.csv', append = TRUE)
            
            cinema_name <- gsub(' ', '%20', cinema_name_)
            theater_info <- gsub('\n', '%0A', (gsub(' ', '%20', theater_info)))
            phone <- gsub(' ', '%20', phone)
            address <- gsub('\n', '%0A', (gsub(' ', '%20', address)))
            
            POST(url = paste0(api_mh,
                              '?cinema_name=', cinema_name,
                              '&address=', address,
                              '&phone=', phone,
                              '&theater_info=', theater_info,
                              '&secret_key=POST-megadb-547778-452220-870001'))
            print(paste(cinema_name_, 'WAS ADDED to the directory'))
            
          }else{
            print(paste(cinema_name_, 'ALREADY EXIST in the directory'))
          }
          Sys.sleep(3)
        }
      }
      
      opening_date <- 'opened'
      status <- 'showing'
      trailer_link <- 'NA'
      cinema_name <- (paste(cinemas, sep = ',', collapse = ','))
      
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
      
      movies_final <- rbind(movies_final, movies)
      
      if (is.na(match(title, prev$title)) & is.na(match(cinema_name, prev$cinema_name))){
        
        title <- gsub(' ', '%20', title)
        genre <- gsub(' ', '%20', genre)
        image <- image
        details <- gsub('\n', '%0A', (gsub(' ', '%20', details)))
        credits <- gsub('\n', '%0A', (gsub(' ', '%20', credits)))
        opening_date <- 'opened'
        cinema_name <- gsub(' ', '%20', (paste(cinemas, sep = ',', collapse = ',')))
        status <- 'showing'
        mtrcb_ratings <- mtrcb_ratings
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
        print(paste('DONE WITH MOVIE ENTITLED', (title_[[i]])['title']))
        
      }
     
     Sys.sleep(3)
  }

fwrite(movies_final, 'src/movies_showing.csv', row.names = FALSE, append = TRUE)
