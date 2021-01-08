library(tidyverse)
library(rvest)


cap_url <- "https://www.uscp.gov/media-center/weekly-arrest-summary"

cap_links <- cap_url %>% 
  read_html() %>% 
  html_nodes(".file a") %>% 
  html_attr("href") %>% 
  data.frame()

cap_links <- cap_links %>% 
  mutate(link=paste0("https://www.uscp.gov", `.` )) %>% 
  mutate(file=gsub(".*wysiwyg_uploaded/", "", link)) %>%
  slice(2:nrow(cap_links)) 

for (i in 1:nrow(cap_links)) {
  url <-cap_links$link[i]
  
  file_name <- paste0("reports/", cap_links$file[i])
  
    tryCatch(download.file(url, file_name), 
             error = function(e) print(paste(file, 'did not work out')))   
    
  print(i)
  Sys.sleep(.5)
}



#https://www.judiciary.senate.gov/

#url <- "https://www.judiciary.senate.gov/download/distcol-phillips-senate-questionnaire-final&download=1"
#download.file(url, "test.pdf")

#saved <- read_html(url) %>% 

#  html_attr("href")
#main_column a