setwd('/Users/maggiesaavedra/GitHub/web_scrapping/clickthecity/')

# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

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

out <- NULL
for (i in 1:length(mh_links)){
  url <- read_html(mh_links[i])
  movie_house <- html_text(html_node(url, '#maincontent a'))
  
  if (length(movie_house) != 0){
    href <- paste0('https://www.clickthecity.com/movies/', html_attrs(html_node(url, '#maincontent a'))['href'])
    
    sched_href <- read_html(href)
      if (length(html_text(html_nodes(sched_href, '#cinemas em')))!=0){
        cinema_name <- paste(movie_house, html_text(html_nodes(sched_href, '#cinemas em')))
        schedule <- html_text(html_nodes(sched_href, 'span div'))    
        duration <- html_text(html_nodes(sched_href, '.running_time'))    
        out <- as.data.table(rbind(out, cbind(cinema_name, schedule, duration)))
        print(paste('Done with', movie_house))
      }else{}
  }else{}
  
  Sys.sleep(.1)
}
  
