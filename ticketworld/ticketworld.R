
# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)

# ticketnet entertainment section
url <- 'https://www.ticketworld.com.ph/Online/default.asp?doWork::WScontent::loadArticle=Load&BOparam::WScontent::loadArticle::article_id=C4E88A87-6815-4979-975C-AE8577C0A4DA&menu_id=5FDB87FA-2953-4458-B493-298CDFE28504&sToken=1%2C67e508c9%2C5a2e916d%2CE5C2A0E3-BCE5-421A-B05C-6FCAC8BEBF1C%2CL0008nr1M55183bjmArkxDxWr44%3D'

# read wholepage or entertainment section
webpage <- read_html(url)

# get only the buytckets node to redirect to the url inside tat node
link_hmtl_data <- html_nodes(webpage,'item-name.selectorgadget_selected')
attr_data <- html_attrs(link_hmtl_data)

