#setwd('~/clickthecity')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

api_moviehouse <- 'http://database.gekkowebhosting.com/api-mega/post-movie-house'
api_sched <- 'http://database.gekkowebhosting.com/api-mega/post-movie-house-schedule'

mv_cinem <- fread('src/cinemas.csv')

mh_links <- c(caloocan <- 'https://www.clickthecity.com/movies/theaters.php?cid=6',
              laspinas <- 'https://www.clickthecity.com/movies/theaters.php?cid=7',
              makati <- 'https://www.clickthecity.com/movies/theaters.php?cid=8',
              mandaluyong <- 'https://www.clickthecity.com/movies/theaters.php?cid=10',
              manila <- 'https://www.clickthecity.com/movies/theaters.php?cid=11',
              marikina <- 'https://www.clickthecity.com/movies/theaters.php?cid=12',
              muntinlupa <- 'https://www.clickthecity.com/movies/theaters.php?cid=13',
              paranaque <- 'https://www.clickthecity.com/movies/theaters.php?cid=15',
              pasay <- 'https://www.clickthecity.com/movies/theaters.php?cid=16',
              pasig <- 'https://www.clickthecity.com/movies/theaters.php?cid=17',
              quezonct <- 'https://www.clickthecity.com/movies/theaters.php?cid=19',
              sanjuan <- 'https://www.clickthecity.com/movies/theaters.php?cid=21',
              taguig <- 'https://www.clickthecity.com/movies/theaters.php?cid=23',
              valenzuala <- 'https://www.clickthecity.com/movies/theaters.php?cid=25'
)
exemp <- c('Greenbelt', 'SM Mall of Asia', 'Eastwood', 'SM City North EDSA', 'Greenhills')

for (i in 1:length(mh_links)){
  download.file(mh_links[i], destfile = 'src/tmp2.html')
  url <- read_html('src/tmp2.html')
  name <- html_text(html_nodes(url, 'h1+ ul a:nth-child(1)'))
  href_list <- html_attrs(html_nodes(url, 'h1+ ul a:nth-child(1)'))
  
  if (length(name) != 0){
    for (k in 1:length(name)){
      
      if ( is.na(match(name[k], exemp)) ){
        href <- paste0('https://www.clickthecity.com/movies/', href_list[k][[1]]['href'])
        download.file(href, destfile = 'src/tmp5.html')
        sched_href <- read_html('src/tmp5.html')
        
        if (length(html_text(html_nodes(sched_href, '#cinemas em')))!=0){
          
          cinema_link <- html_attrs(html_nodes(sched_href, '#cinemas h2 a'))
            
            for (j in 1: length(cinema_link)){
              download.file(cinema_link[[j]]['href'], destfile = 'src/tmp7.html')
              cinema_page <- read_html('src/tmp7.html')
              
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
                
              }else {
                print('no info')
              }
              
              cinema_name_ <- cinema_link[[j]]['alt']
              movie_house <- gsub("(.+?)(\\\n.*)", "\\1", address)
              address <- gsub("(.+?)(\\\n.*)", "\\2", address)
              fwrite(as.data.table(cbind(cinema_name_, movie_house, theater_info, phone, address)), 'src/cinemas.csv', append = TRUE)
              
              cinema_name <- gsub(' ', '%20', cinema_name_)
              movie_house <- gsub(' ', '%20', movie_house)
              theater_info <- gsub('\n', '%0A', (gsub(' ', '%20', theater_info)))
              phone <- gsub(' ', '%20', phone)
              address <- gsub('\n', '%0A', (gsub(' ', '%20', address)))
              
              POST(url = paste0(api_cinema,
                                '?cinema_name=', cinema_name,
                                '&movie_house=', movie_house,
                                '&address=', address,
                                '&phone=', phone,
                                '&theater_info=', theater_info,
                                '&secret_key=POST-megadb-547778-452220-870001'))
              print(paste(cinema_name_, 'WAS ADDED to the directory'))
            }
          
        }else{
          print(paste('No cinemas in', name[k]))
        }
        print(toupper(paste('Done with', name[k])))
        
      }else{
        # name_child <- html_text(html_nodes(url, '#maincontent a+ a'))
      }
    }
  }else{}
  print(paste('Done with', i))
  Sys.sleep(5)
}
