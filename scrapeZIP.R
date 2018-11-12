library(readxl)
library(rvest)
library(tidyverse)
library(doParallel)

stores <- read_excel("DOLLAR_GENERAL_STORE_LIST.xlsx")

stores <- stores %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "+", .), City %>% gsub(" ", "+", .), State %>% gsub(" ", "+", .), sep=",+"),
         link = gsub("\\+HWY\\+", "-", link),
         link = gsub("\\(", "", link))

# Check Google Maps
zipScrape <- function(link) {
  fullAddress <- read_html(paste0("https://www.google.com/maps/place/", link)) %>% 
    html_nodes("head meta") %>%
    html_attr("content") %>%
    .[12]
  
  return(fullAddress)
}

# Check MapQuest
zipScrapeMQ <- function(link) {
  link <- link %>% 
    substr(1,nchar(link)-4)
  link <- paste0(gsub("\\+", "%20", link), ",%20dollar%20general")
  
  fullAddress <- read_html(paste0("https://www.mapquest.com/search/result?query=", link)) %>% 
    html_nodes("span span") %>%
    html_text(trim=TRUE) %>%
    .[1] %>%
    gsub("\\s+", " ", .) %>%
    gsub(" ,", ",", .)
  
  return(fullAddress)
}

# Check Google Search
zipScrapeGS <- function(link) {
  index <- read_html(paste0("https://www.google.com/search?q=", link)) %>% 
    html_nodes("div span") %>% 
    html_text() %>% 
    grep("Address:", .) + 1
  
  if(length(index) == 0) return(NA)
  
  fullAddress <- read_html(paste0("https://www.google.com/search?q=", link)) %>% 
    html_nodes("div span") %>% 
    html_text() %>%
    .[index]
  
  return(fullAddress)
}

# Check Bing Search
zipScrapeBS <- function(link) {
  index <- read_html(paste0("https://www.bing.com/search?q=", link)) %>% 
    html_nodes("div span") %>% 
    html_text() %>% 
    grep("DirectionsWebsite", .) - 1
  
  if(length(index) == 0) return(NA)
  
  fullAddress <- read_html(paste0("https://www.bing.com/search?q=", link)) %>% 
    html_nodes("div span") %>% 
    html_text() %>%
    .[index]
  
  return(fullAddress)
}


# initialize fullAddress column
stores$fullAddress <- NA

# Scrape about 4600 stores from google maps
i=1
for (i in i:nrow(stores)) {
  stores$fullAddress[i] <- zipScrape(stores$link[i])
  print(i)
}

stores <- stores %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

storesNA <- stores %>%
  filter(is.na(zip))


# Scrape from bing search
storesNA <- storesNA %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "%20", .), City %>% gsub(" ", "%20", .), "dollar%20general", sep="%2C"))

i=1
for (i in i:nrow(storesNA)) {
  storesNA$fullAddress[i] <- zipScrapeBS(storesNA$link[i])
  print(i)
}

stores1 <- full_join(storesNA, stores %>% filter(!is.na(zip))) %>%
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

stores1NA <- stores1 %>%
  filter(is.na(zip))

stores1NA <- stores1NA %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "%20", .), City %>% gsub(" ", "%20", .), "dollar%20general", sep="%2C"))


i=1
for (i in i:nrow(stores1NA)) {
  stores1NA$fullAddress[i] <- zipScrapeGS(stores1NA$link[i])
  Sys.sleep(sample(10, 1) * 0.1)
  print(i)
}

for (i in i:nrow(stores1NA)) {
  stores1NA$fullAddress[i] <- zipScrapeBS(stores1NA$link[i])
  Sys.sleep(sample(10, 1) * 0.1)
  print(i)
}


stores1NA <- stores1NA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

stores2 <- full_join(stores1NA, stores1 %>% filter(!is.na(zip))) %>%
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = ifelse(zip < 0, substr(fullAddress, nchar(fullAddress)-10, nchar(fullAddress)-5), zip),
         zip = ifelse(zip == 1, NA, zip),
         zip = as.numeric(zip))

stores2NA <- stores2 %>%
  filter(is.na(zip))

for (j in 1:3) {
  stores2NA$link <- gsub("%2Cdollar%20general|\\#", "", stores2NA$link)
  stores2NA$link <- gsub("HWY", "Highway", stores2NA$link)
  i=1
  for (i in i:nrow(stores2NA)) {
    stores2NA$fullAddress[i] <- zipScrapeBS(stores2NA$link[i])
    Sys.sleep(sample(10, 1) * 0.1)
    print(i)
  }
  stores2 <- full_join(stores2NA, stores2 %>% filter(!is.na(zip))) %>%
    mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
           zip = ifelse(zip < 0, substr(fullAddress, nchar(fullAddress)-10, nchar(fullAddress)-5), zip),
           zip = ifelse(zip == 1, NA, zip),
           zip = as.numeric(zip))
  
  stores2NA <- stores2 %>%
    filter(is.na(zip))
}


write.csv(stores2, "stores.csv")




