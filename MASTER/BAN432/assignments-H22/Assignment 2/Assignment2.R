# Assignment 2 - Scraping EDGAR for S-1's

  require(tidyverse)

# Set working directory
  
  #setwd("")
  
# Building a function to download master.idx for each quarter
  
  base.url <- "https://www.sec.gov/Archives/edgar/full-index/"
  year <- 2021
  
# Creating a loop that downloads the master indexes
  
  for(i in 1:4) {
    web.url <- paste0(
      base.url,
      year,
      "/QTR",
      i,
      "/master.idx"
    )
    
    download.file(web.url, 
                  destfile = paste0("data/","masterQTR", i, ".txt"), 
                  headers = c("User-Agent" = "sander.dahling@student.nhh.no"))
  }

# Import the data sets (could not find a way to loop this part)
  
  qtr1 <- read_delim(paste0("data/","masterQTR", 1, ".txt"), 
                     delim = "|", 
                     skip = 11,
                     col_names = F) %>% 
    filter(X3 == "S-1")
  
  qtr2 <- read_delim(paste0("data/","masterQTR", 2, ".txt"), 
                     delim = "|", 
                     skip = 11,
                     col_names = F) %>% 
    filter(X3 == "S-1")
  
  
  qtr3 <- read_delim(paste0("data/","masterQTR", 3, ".txt"), 
                     delim = "|", 
                     skip = 11,
                     col_names = F) %>% 
    filter(X3 == "S-1")
  
  qtr4 <- read_delim(paste0("data/","masterQTR", 4, ".txt"), 
                     delim = "|", 
                     skip = 11,
                     col_names = F) %>% 
    filter(X3 == "S-1")
  
  
# Store the amount of filed S-1's in a tibble 
  
  S_1 <- tibble(
    Q1 = nrow(qtr1),
    Q2 = nrow(qtr2),
    Q3 = nrow(qtr3),
    Q4 = nrow(qtr4)
  )
  
  
  