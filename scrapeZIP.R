library(readxl)
library(rvest)
library(tidyverse)

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

##### Scrape about 4600 stores from google maps #####
i=1
for (i in i:nrow(stores)) {
  stores$fullAddress[i] <- zipScrape(stores$link[i])
  print(i)
}

# Get fixed zip codes
stores <- stores %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Create storesNA df
storesNA <- stores %>%
  filter(is.na(zip))

# Remove state from link
storesNA <- storesNA %>% 
  mutate(link = substr(link, 1, nchar(link)-4))


# Scrape google maps again but without state
i=1
for (i in i:nrow(stores)) {
  storesNA$fullAddress[i] <- zipScrape(storesNA$link[i])
  print(i)
}

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~5700 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~1100 zips
storesNA <- stores %>%
  filter(is.na(zip))


# Save progress so far
saveRDS(stores, "stores.RDS")


###### Scrape from bing search #####

# Create new link format
storesNA <- storesNA %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "%20", .), City %>% gsub(" ", "%20", .), "dollar%20general", sep="%2C"),
         link = gsub("HWY", "Highway", link),
         link = gsub("\\#%20|\\#", "", link))

# Begin scraping
i=1
for (i in i:nrow(storesNA)) {
  storesNA$fullAddress[i] <- zipScrapeBS(storesNA$link[i])
  print(i)
}

# Remove stores that defaulted to stores near me
storesNA <- storesNA %>%
  mutate(fullAddress = ifelse(fullAddress == "131 Bradford Ave, Crafton, PA 15205", NA, fullAddress),
         fullAddress = ifelse(fullAddress == "500 Pine Hollow Rd, Mc Kees Rocks, PA 15136", NA, fullAddress))

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6300 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~550 zips
storesNA <- stores %>%
  filter(is.na(zip))


# Remove dollar general at end of link
storesNA <- storesNA %>%
  mutate(link = paste(`Address 1` %>% gsub(" ", "%20", .), City %>% gsub(" ", "%20", .), sep="%2C"),
         link = gsub("HWY", "Highway", link),
         link = gsub("\\#%20|\\#", "", link))

# Begin Scraping
i=1
for (i in i:nrow(storesNA)) {
  storesNA$fullAddress[i] <- zipScrapeBS(storesNA$link[i])
  print(i)
}

# Remove stores that defaulted to stores near me
storesNA <- storesNA %>%
  mutate(fullAddress = ifelse(fullAddress == "131 Bradford Ave, Crafton, PA 15205", NA, fullAddress),
         fullAddress = ifelse(fullAddress == "500 Pine Hollow Rd, Mc Kees Rocks, PA 15136", NA, fullAddress))

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6400 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~450 zips
storesNA <- stores %>%
  filter(is.na(zip))

# Save progress so far
saveRDS(stores, "stores.RDS")

# Begin Scraping from google search
i=1
for (i in i:nrow(storesNA)) {
  storesNA$fullAddress[i] <- zipScrapeGS(storesNA$link[i])
  print(i)
}

# Remove stores that defaulted to stores near me
storesNA <- storesNA %>%
  mutate(fullAddress = ifelse(fullAddress == "131 Bradford Ave, Crafton, PA 15205", NA, fullAddress),
         fullAddress = ifelse(fullAddress == "500 Pine Hollow Rd, Mc Kees Rocks, PA 15136", NA, fullAddress))

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6400 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~398 zips
storesNA <- stores %>%
  filter(is.na(zip))

# Begin Scraping from google search
i=1
for (i in i:nrow(storesNA)) {
  linkMap <- read_html(paste0("https://www.google.com/search?q=", storesNA$link[i])) %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") %>% 
    .[1]
  
  if(substr(linkMap, 1, 5) == "/url?") next
  
  storesNA$fullAddress[i] <- read_html(linkMap) %>% 
    html_nodes("head meta") %>%
    html_attr("content") %>%
    .[12] 
  print(i)
}

# Remove stores that defaulted to stores near me
storesNA <- storesNA %>%
  mutate(fullAddress = ifelse(fullAddress == "131 Bradford Ave, Crafton, PA 15205", NA, fullAddress),
         fullAddress = ifelse(fullAddress == "500 Pine Hollow Rd, Mc Kees Rocks, PA 15136", NA, fullAddress))

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6400 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~96 zips
storesNA <- stores %>%
  filter(is.na(zip))

# Save progress so far
saveRDS(stores, "stores.RDS")

####### Use Yellowpages #######
storesNA <- storesNA %>%
  mutate(link = paste(`Address 1` %>% 
                        gsub("\\&","",.) %>%
                        gsub(" ", "%20", .), City %>% gsub(" ", "%20", .), "yellow%20pages", sep="%2C%20"),
         link = gsub("HWY", "Highway", link),
         link = gsub("\\#%20|\\#", "", link))

# Scrape google search and find yellowpages
i=1
for (i in i:nrow(storesNA)) {
  linkMap <- read_html(paste0("https://www.google.com/search?q=", storesNA$link[i])) %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") %>% 
    grep("dollar-general-", ., value=T) %>%
    substr(., 8, 10000) %>%
    sub("&.*", "", .)
  
  if(length(linkMap) != 1||!grepl("yellowpages", linkMap)) next
  
  storesNA$fullAddress[i] <- read_html(linkMap) %>% 
    html_nodes("#main-header span") %>% 
    html_text() %>% 
    paste0(collapse="") %>%
    gsub("YEARS\\s*IN\\s*BUSINESS|\\(2 Reviews\\)", "", .)
  print(i)
}

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6400 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~72 zips
storesNA <- stores %>%
  filter(is.na(zip))

# Save progress so far
saveRDS(stores, "stores.RDS")

############ Last Try ############
# Scrape google search and find yellowpages
i=1
for (i in i:nrow(storesNA)) {
  linkMap <- read_html(paste0("https://www.google.com/search?q=", storesNA$link[i], "%20dollar%20general")) %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") %>% 
    grep("dollar-general-", ., value=T) %>%
    substr(., 8, 10000) %>%
    sub("&.*", "", .)
  
  if(length(linkMap) != 1||!grepl("yellowpages", linkMap)) next
  
  storesNA$fullAddress[i] <- read_html(linkMap) %>% 
    html_nodes("#main-header span") %>% 
    html_text() %>% 
    paste0(collapse="") %>%
    gsub("YEARS\\s*IN\\s*BUSINESS|\\(2 Reviews\\)|\\(1 Review\\)", "", .)
  print(i)
}

# Get fixed zip codes in storesNA df
storesNA <- storesNA %>% 
  mutate(zip = substr(fullAddress, nchar(fullAddress)-4, nchar(fullAddress)),
         zip = as.numeric(zip))

# Join stores and storesNA df. Stores now has ~6400 zips
stores <- full_join(storesNA, stores %>% filter(!is.na(zip)))

# Reduce storesNA to ~65 zips
storesNA <- stores %>%
  filter(is.na(zip))


stores <- stores %>%
  mutate(zip = ifelse(zip == 1, NA, zip), 
         fullAddress = ifelse(is.na(zip), NA, fullAddress),
         fixedState = gsub("(.*, )(.*)", "\\2", fullAddress),
         fixedState = gsub("( .*)", "\\2", fixedState),
         fixedState = gsub("")) %>%
  arrange(`Store #`)

storesSave <- stores %>% 
  select(`Store #`, `Address 1`, City, fixedState, zip, fullAddress, StateInOrigFile = State)

# Save progress so far
saveRDS(stores, "stores.RDS")

write.csv(storesSave, "stores.csv")
