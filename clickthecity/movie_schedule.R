#setwd('/Users/maggiesaavedra/GitHub/web_scrapping/clickthecity/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

api_moviehouse <- 'http://database.gekkowebhosting.com/api-mega/post-movie-house'
api_sched <- 'http://database.gekkowebhosting.com/api-mega/post-movie-house-schedule'
delete_api <- 'http://database.gekkowebhosting.com/api-mega/reset-schedules?secret_key=POST-megadb-547778-452220-870001'

POST(url = delete_api)

prev_mh <- fread('src/moviehouse.csv')
prev_sched <- fread('src/moviehouse_schedule.csv')

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
          
          movie_house <- html_text(html_nodes(sched_href, '#theatersArea h2'))
          address <- html_text(html_nodes(sched_href, 'address span'))[1]
          phone_number <- ' '
          lat <- ' '
          long <- ' '
          icon_link <- ifelse( test = length(html_attrs(html_nodes(sched_href, "#poster img")))!=0, 
                               yes = html_attrs(html_nodes(sched_href, "#poster img"))[[1]]['src'],
                               no = 'NA')
          website <- ' '
          
          movie_house <- as.data.table(cbind(name[k], phone_number, address, lat, long, icon_link, website))
          fwrite(movie_house, 'src/moviehouse.csv', row.names = FALSE, append = TRUE)
          
          name_ <- gsub(' ', '%20', name[k])
          phone_number_ <- gsub(' ', '%20', phone_number)
          address_ <- gsub(' ', '%20', address)
          lat_ <- gsub(' ', '%20', lat)
          long_ <- gsub(' ', '%20', long)
          website_ <- gsub(' ', '%20', website)
          
          POST(url = paste0(api_moviehouse,
                            '?name=', name_,
                            '&phone_number=', phone_number_,
                            '&address=', address_,
                            '&lat=', lat_,
                            '&long=', long_,
                            '&icon_link', icon_link,
                            '&website=', website_,
                            '&secret_key=POST-megadb-547778-452220-870001'))
          
          
          if (name[k] == 'Cash and Carry'){
            cn <- paste(html_text(html_nodes(sched_href, '#cinemas em')))
          }else{
            cn <- paste(name[k], html_text(html_nodes(sched_href, '#cinemas em')))
          }
          mn <- html_text(html_nodes(sched_href, '#cinemas span a span'))
          sc <- html_text(html_nodes(sched_href, 'span div'))
          du <- html_text(html_nodes(sched_href, '.running_time'))
          
          ## check is there are multiple movies in one cinema
          num <- match(html_text(html_nodes(sched_href, '#cinemas em')), html_text(html_nodes(sched_href, '#cinemas span a span , #cinemas em')))
          
          out <- NULL
          for (j in 1:length(num)){
            
            if (j < length(num)){
              if( ((num[j+1] - num[j]) == 2) ){
                cinema_name <- cn[j]
                movie_name <- mn[j]
                schedule <- sc[j]
                duration <- du[j]
                movie_house <- name[k]
                out_ <- as.data.table(cbind(cinema_name, movie_house, movie_name, schedule, duration))
                out <- rbind(out, out_)
                fwrite(out_, 'src/moviehouse_schedule.csv', row.names = FALSE, append = TRUE)
                print(paste('Done with', cinema_name, ' SCHEDULE'))
                
                cinema_name_ <- gsub(' ', '%20', cinema_name)
                movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))
                movie_house <- gsub(' ', '%20', movie_house)
                schedule_ <- gsub(' ', '%20', schedule)
                duration_ <- gsub(' ', '%20', duration)
                
                POST(url = paste0(api_sched,
                                  '?cinema_name=', cinema_name_,
                                  '&movie_house=', movie_house,
                                  '&movie_name=', movie_name_,
                                  '&schedule=', schedule_,
                                  '&duration=', duration_,
                                  '&secret_key=POST-megadb-547778-452220-870001'))
                
                
              }else if( (num[j+1] - num[j]) == 3 ){
                cinema_name <- rep(cn[j], 2)
                movie_name <- mn[j:(j+1)]
                schedule <- sc[j:(j+1)]
                duration <- du[j:(j+1)]
                movie_house <- rep(name[k], 2)
                out_ <- as.data.table(cbind(cinema_name, movie_house, movie_name, schedule, duration))
                out <- rbind(out, out_)
                fwrite(out_, 'src/moviehouse_schedule.csv', row.names = FALSE, append = TRUE)
                print(paste('Done with', cinema_name, ' SCHEDULE'))
                
                cinema_name_ <- gsub(' ', '%20', cinema_name[1])
                movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))[1]
                movie_house <- gsub(' ', '%20', movie_house[1])
                schedule_ <- gsub(' ', '%20', schedule[1])
                duration_ <- gsub(' ', '%20', duration[1])
                
                POST(url = paste0(api_sched,
                                  '?cinema_name=', cinema_name_,
                                  '&movie_house=', movie_house,
                                  '&movie_name=', movie_name_,
                                  '&schedule=', schedule_,
                                  '&duration=', duration_,
                                  '&secret_key=POST-megadb-547778-452220-870001'))
                
                cinema_name_ <- gsub(' ', '%20', cinema_name[2])
                movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))[2]
                movie_house <- gsub(' ', '%20', movie_house[2])
                schedule_ <- gsub(' ', '%20', schedule[2])
                duration_ <- gsub(' ', '%20', duration[2])
                
                POST(url = paste0(api_sched,
                                  '?cinema_name=', cinema_name_,
                                  '&movie_house=', movie_house,
                                  '&movie_name=', movie_name_,
                                  '&schedule=', schedule_,
                                  '&duration=', duration_,
                                  '&secret_key=POST-megadb-547778-452220-870001'))
                
              }else if ( (num[j+1] - num[j]) == 1 ){
                print('no movies is showing in this cinema')
              }else{
                print('cant recognize error')
              }
              
            }else if( j == length(num)){
              
              if (!is.null(nrow(out))) {
                if ( nrow(out) == length(mn) ){
                  cinema_name <- cn[j]
                  movie_name <- ' '
                  schedule <- ' '
                  duration <- ' '
                  movie_house <- name[k]
                  out_ <- as.data.table(cbind(cinema_name, movie_house, movie_name, schedule, duration))
                  out <- rbind(out, out_)
                  fwrite(out_, 'src/moviehouse_schedule.csv', row.names = FALSE, append = TRUE)
                  print(paste(cinema_name, 'HAS NO SCHEDULE'))
                  
                  inema_name_ <- gsub(' ', '%20', cinema_name)
                  movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))
                  movie_house <- gsub(' ', '%20', movie_house)
                  schedule_ <- gsub(' ', '%20', schedule)
                  duration_ <- gsub(' ', '%20', duration)
                  
                  POST(url = paste0(api_sched,
                                    '?cinema_name=', cinema_name_,
                                    '&movie_house=', movie_house,
                                    '&movie_name=', movie_name_,
                                    '&schedule=', schedule_,
                                    '&duration=', duration_,
                                    '&secret_key=POST-megadb-547778-452220-870001'))
                  
                }else if( nrow(out) < length(mn) ){
                  
                  for(l in (nrow(out)+1):length(mn)){
                    cinema_name <- cn[j]
                    movie_name <- mn[l]
                    schedule <- sc[l]
                    duration <- du[l]
                    movie_house <- name[k]
                    out_ <- as.data.table(cbind(cinema_name, movie_house, movie_name, schedule, duration))
                    out <- rbind(out, out_)
                    fwrite(out_, 'src/moviehouse_schedule.csv', row.names = FALSE, append = TRUE)
                    print(paste('Done with', cinema_name, ' SCHEDULE'))
                    
                    inema_name_ <- gsub(' ', '%20', cinema_name)
                    movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))
                    movie_house <- gsub(' ', '%20', movie_house)
                    schedule_ <- gsub(' ', '%20', schedule)
                    duration_ <- gsub(' ', '%20', duration)
                    
                    POST(url = paste0(api_sched,
                                      '?cinema_name=', cinema_name_,
                                      '&movie_house=', movie_house,
                                      '&movie_name=', movie_name_,
                                      '&schedule=', schedule_,
                                      '&duration=', duration_,
                                      '&secret_key=POST-megadb-547778-452220-870001'))
                    
                  }
                }else {
                  print('UNIDENTIFIED ERROR')
                }
              }else {
                
                for(l in 1:length(mn)){
                  cinema_name <- cn[j]
                  movie_name <- mn[l]
                  schedule <- sc[l]
                  duration <- du[l]
                  movie_house <- name[k]
                  out_ <- as.data.table(cbind(cinema_name, movie_house, movie_name, schedule, duration))
                  out <- rbind(out, out_)
                  fwrite(out_, 'src/moviehouse_schedule.csv', row.names = FALSE, append = TRUE)
                  print(paste('Done with', cinema_name, ' SCHEDULE'))
                  
                  inema_name_ <- gsub(' ', '%20', cinema_name)
                  movie_name_ <- gsub(' ', '%20', gsub('&', '%26', movie_name))
                  movie_house <- gsub(' ', '%20', movie_house)
                  schedule_ <- gsub(' ', '%20', schedule)
                  duration_ <- gsub(' ', '%20', duration)
                  
                  POST(url = paste0(api_sched,
                                    '?cinema_name=', cinema_name_,
                                    '&movie_house=', movie_house,
                                    '&movie_name=', movie_name_,
                                    '&schedule=', schedule_,
                                    '&duration=', duration_,
                                    '&secret_key=POST-megadb-547778-452220-870001'))
                  
                }
                
              }
              
              
            }
            
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
