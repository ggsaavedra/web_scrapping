
# Set locall time
Sys.setlocale("LC_COLLATE", "C")

# Scrapping in R using rvest
library(rvest)
library(lubridate)
library(data.table)
library(httr)

# ticketnet entertainment section
url <- 'https://www.ticketnet.com.ph/index.php/entertainment'

#POST API
api <- "http://database.gekkowebhosting.com/api-mega/post-event"

# read wholepage or entertainment section
webpage <- read_html(url)

# get only the buytckets node to redirect to the url inside tat node
link_hmtl_data <- html_nodes(webpage,'.btn.btn-default')
attr_data <- html_attrs(link_hmtl_data)

names <- c('id',  'name',  'rate',  'marked',  'phonenumber',  'smsnumber',  'email',  'first_name',  'last_name',  'category',  'category1',  'sub_category',  'description',  'summary',  'special_merchant',  'long_banner',  'logo',  'address',  'latitude',  'longitude',  'other_photos1',  'other_photos2',  'other_photos3',  'list_banner',  'discount',  'gift_certificate',  'user_id',  'status',  'progressive_status',  'MondayAM1',  'created_at',  'updated_at',  'website',  'encoded_by',  'salesperson_id',  'slug',  'proposal_status',  'get_newsletter',  'preference_id',  'msc_directory_vid',  'is_distributor',  'crown',  'vouchers',  'loyalty',  'advertiser',  'led',  'is_client',  'visamastercard',  'rank_order',  'quantity',  'delivered',  'called',  'proposal',  'follow',  'closed',  'archived',  'premium')
event_data <- setNames(data.frame(matrix(ncol = 57, nrow = 0)), names)

event_data  <- NULL
for (i in 1:length(attr_data)){
  
  print(paste('iteration number', i))
  print((attr_data[[i]])['href'])
  if (is.na((attr_data[[i]])['href'])){
    
    print('no extratable link')
    
  } else if(substr((attr_data[[i]])['href'], 1, 6) == "/index" ){
    
    print(paste('link is invalid:', (attr_data[[i]])['href'], 'The link might not have the same format than the usual'))
    
    
  } else {
    webpage_href <- read_html((attr_data[[i]])['href'])
    
    #get name of the event
    name_data_html <- html_node(webpage_href, 'h2')
    name_data <- html_text(name_data_html)
    print(name_data)
    
    location_data_html <- html_node(webpage_href, '#performance-details .twelve.columns span')
    location_data <- html_text(location_data_html)
    print(location_data)
    
    sched_data_html <- html_node(webpage_href, '#performance-details .time')
    sched_tmp <- gsub("AddThisEvent.*", "", html_text(sched_data_html))
    sched_data <- as.character(strptime(gsub('\n\\s+', '', sched_tmp), '%B %e, %Y%I:%M %p'))
    print(sched_data)
    
    details_data_html <- html_nodes(webpage_href, "div#performance-details .event_details")
    details_data_ <- gsub(pattern = '\n\\s+|\n', replacement = "", details_data_html)
      
      if (length(details_data_)){
        details_data_ <- gsub(pattern = '<.*?>', replacement = "|", details_data_)
        details_data <- gsub('^.|.{2}$', '', details_data_)
        print(details_data)
      } else (details_data <- 'No details')
    
    # price_data_ <- (unlist(strsplit(details_data_, '\\|\\|')))[1]
    # price_data_ <- data.frame(unlist(strsplit(price_data, '\\|')))
    # colnames(price_data_) <- price_data_[1,]
    # price_data_ <- price_data_[-1,]
    # 
    # saledate_data <- (unlist(strsplit(details_data_, '\\|\\|')))[2]
    
    image_data_html <- html_node(webpage_href, '#performance-details .description.row img')
    image_data <- html_attr(image_data_html, 'src')
    print(image_data)
    
    res <- c(name_data, location_data, sched_data, details_data, image_data)
    print(res)
    event_data <- rbind(event_data, res)
    Sys.sleep(0.001)

# pushin to the post API
    title <- gsub('\n', '%0A', (gsub(' ', '%20', name_data)))
    address <- gsub('\n', '%0A', (gsub(' ', '%20', location_data)))
    schedule <- gsub('\n', '%0A', (gsub(' ', '%20', sched_data)))
    details <- gsub('\n', '%0A', (gsub(' ', '%20', details_data)))
    long_image <- gsub('\n', '%0A', (gsub(' ', '%20', image_data)))
    
    POST(url = paste0(api,
                      '?title=', title,
                      '&address=', address,
                      '&schedule=', schedule,
                      '&details=', details,
                      '&long_image=', long_image,
                      '&secret_key=POST-megadb-547778-452220-870001'))
    
    #delete naming history
    rm(name_data, location_data, sched_data, details_data, image_data)
  }
}

event_data_unique <- as.data.table(event_data[!duplicated(event_data),])
names(event_data_unique) <- c("name", "location", "sched", "details", "image")
<<<<<<< HEAD
fwrite(event_data_unique, 'Moonlight/events/ticketnet/entertainment.csv')
=======
fwrite(event_data_unique, '/home/events/ticketnet.csv')
>>>>>>> 64e2e29d06e54966ca8d67907791117781398e8b
