

library(rvest)
library(data.table)
link <- 'https://www.bloomberg.com/search?query=philippines&endTime=2018-04-25T03:18:40.983Z&page='

for (i in 1292:1363){
  url <- read_html(paste0(link,i))
  date <- as.POSIXct(strptime(html_text(html_nodes(url, '.published-at')), format = ' %b %d, %Y '))
  headline <- html_text(html_nodes(url, '.search-result-story__headline a'))
  df <- data.table(date,headline)
  fwrite(df, 'bloomberg_articles.csv', append = TRUE, row.names = FALSE)
  # content <- NULL
  # href <- html_attrs(html_nodes(url, '.search-result-story__headline a'))
  # for (j in 1:length(href)){
  #   download.file(href[[j]]['href'], 'tmp2')
  #   body_url <- read_html('tmp2')
  #   content[j] <- html_text(html_nodes(body_url, '.fence-body'))
  #   Sys.sleep(sample(10:100, 1))
  # }
  Sys.sleep(sample(5:10, 1))
  print(paste('Done with page', i))
}


