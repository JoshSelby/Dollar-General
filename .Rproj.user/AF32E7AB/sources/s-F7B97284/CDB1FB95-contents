library(readxl)
library(rvest)
library(tidyverse)
library(doParallel)

stores <- read_excel("DOLLAR_GENERAL_STORE_LIST.xlsx")

stores <- stores %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "+", .), City %>% gsub(" ", "+", .), State %>% gsub(" ", "+", .), sep=",+"),
         link = gsub("\\+HWY\\+", "-", link))


zipScrape <- function(link) {
  fullAddress <- read_html(paste0("https://www.google.com/maps/place/", link)) %>% 
    html_nodes("head meta") %>%
    html_attr("content") %>%
    .[grepl(gsub("\\+", " ", link), ., ignore.case=T)] %>%
    .[1]
  
  return(fullAddress)
}


stores$fullAddress <- NA
for (i in 1:nrow(stores)) {
  stores$fullAddress[i] <- zipScrape(stores$link[i])
  print(i)
}



  
