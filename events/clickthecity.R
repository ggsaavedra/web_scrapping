library(rvest)
ht <- read_html('https://www.google.co.in/search?q=events+manila+2018')
links <- ht %>% html_nodes(xpath='//h3/a') %>% html_attr('href')
gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))

# to get next pages links
next_page <- html_attrs(html_nodes(ht, '#nav .fl'))

for (i in 1:length(next_page)){
  links <- ht %>% html_nodes(xpath='//h3/a') %>% html_attr('href')
  gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))
  
}