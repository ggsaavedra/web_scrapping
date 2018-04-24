

library(rvest)
url <- 'https://www.bloomberg.com/search?query=philippines&endTime=2018-04-24T03:18:40.983Z&page='

i=0
repeat{
  i = i+1
  download.file(paste0(url,i), 'tmp1')
  url <- read_html('tmp1')
  date <- html_text(html_nodes(url, '.published-at'))
  headline <- html_text(html_nodes(url, '.search-result-story__headline a'))
  
  content <- NULL
  href <- html_attrs(html_nodes(url, '.search-result-story__headline a'))
  for (j in 1:length(href)){
    download.file(href[[j]]['href'], 'tmp2')
    body_url <- read_html('tmp2')
    content[j] <- html_text(html_nodes(body_url, '.fence-body'))
    Sys.sleep(5)
  }

  
}


