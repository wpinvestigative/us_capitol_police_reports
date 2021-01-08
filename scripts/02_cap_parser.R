library(tidyverse)
library(pdftools)

reports <- list.files("reports")

months <- c("01", "02", "03", "04", "05",
            "06", "07", "08", "09", "10",
            "11", "12")

for (i in 1:length(reports)) {
  file_location <- paste0("reports/", reports[i])
  txt <- pdf_text(file_location)
  pages <- length(txt)
  
    page <- paste(txt, collapse="")
    page <- gsub(".*119 D Street, NE, for processing.\n", "", page)
  
    if (!grepl("No arrests were made during this period", page)) {
    #violation
    #date
    #time
    #id
    #narrative
      items <- str_split(page, "\n") %>% data.frame()
      colnames(items) <- "text"
      
      narrative <- ""
      
      for (y in 1:nrow(items)) {
        
        line <- str_trim(items$text[y])
  
        line_no_weird <- gsub("\\/", "", line)
        line_no_weird <- gsub(":", "", line_no_weird)
        first_two <- substr(line_no_weird, 0,2)
        if (grepl("ADDITIONAL CHARGE", line_no_weird)) {
          line_no_weird <- str_to_title(line_no_weird)
        }
        line_no_weird <- gsub("\\(b\\)", "(B)", line_no_weird)
        
  
        if (line_no_weird==str_to_upper(line_no_weird) & grepl("\\D", first_two)) {
          
          if (y>1) {
            df <- data.frame(violation, date, narrative)
            if (!exists("df_mega")) {
              df_mega <- df
            } else {
              df_mega <- rbind(df_mega, df)
            }
            narrative <- ""
          } 
          
          violation <- line
          date <- str_trim(items$text[y+1])
          
        } else if (line_no_weird!=str_to_upper(line_no_weird) & grepl("\\D", first_two)) {
          narrative <- paste(narrative, line)
        }
        
        if (y==nrow(items)) {
          df <- data.frame(violation, date, narrative)
          if (exists("df_mega")) {
          df_mega <- rbind(df_mega, df)
          } else { df_mega <- df }
        }
      }
      
      
    df_mega$pdf <- reports[i]
    
    if (i==1) {
      df_combined <- df_mega
      rm(df_mega)
    } else {
      df_combined <- rbind(df_combined, df_mega)
      rm(df_mega)
    }
    print(paste0(i, " of ", length(reports)))
  } 
}


# clean up
df_combined_copy <- df_combined

for (i in 1:nrow(df_combined)) {
  first_two <- substr(df_combined$date[i], 0,2)
  
  if (grepl("\\D", first_two)) {
    
    df_combined$narrative[i-1] <- paste(df_combined$narrative[i-1], df_combined$violation[i], df_combined$narrative[i])
    print("yes")
  }
  
}


df_combined$two <-  substr(df_combined$date, 0,2)
df_combined <- df_combined %>% 
  filter(!grepl("\\D", two))

library(lubridate)

df_combined <- df_combined %>% 
  mutate(id=str_trim(gsub(".*         ", "", date)),
         time=gsub(" 202.*", "", date),
         time=gsub(" 210.*", "", time),
         time=gsub(" 201.*", "", time),
         time=gsub(" 200.*", "", time),
         time=str_trim(gsub(" CFN.*", "", time)),
         time=gsub(".*          ", "", time),
         date=str_trim(gsub("         .*", "", date)))

df_combined <- df_combined %>% 
  mutate(id=gsub("CFN: ", "", id),
         date=mdy(date))
df_combined$two <- NULL

df_combined <- df_combined %>% 
  mutate(additional_charges=gsub("R/O.*", "", narrative),
         additional_charges=gsub("ADDITIONAL CHARGES: ", "", additional_charges),
         additional_charges=gsub("ADDITIONAL CHARGE: ", "", additional_charges),
         charges=paste0(violation, "; ", additional_charges),
         charges=gsub("\\(b\\)\\(4\\)", "", charges))

library(muckrakr)

capitol <- untangle(data=df_combined, x="charges", pattern="[;]", verbose=TRUE)

capitol <- capitol %>% 
  pivot_longer(cols=9:157, names_to="charge", values_to="value")

capitol <- capitol %>% 
  filter(value!=0) %>% 
  mutate(charge=gsub("_", " ", charge))

charge_count <- count(capitol, charge, sort=T)


capitol <- capitol %>% 
  mutate(charge=case_when(
    grepl("bench warrant", charge) ~ "bench warrant - misdemeanor",
    grepl("carrying a pistol", charge) ~ "carrying a pistol w/o a license - felony",
    grepl("assault on", charge) ~ "assault on a police officer - misdemeanor",
    grepl("assault –", charge) ~ "assault – simple - misdemeanor",
    grepl("assault–", charge) ~ "assault – simple - misdemeanor",
    grepl("simple assault", charge) ~ "assault – simple - misdemeanor",
    grepl("crowding", charge) ~ "crowding, obstructing, or incommoding dc code 22-1307",
    
    TRUE ~ charge
  ))