# The data used in this analysis is generated from the script pdf-scraper.r

# ---- Clear Environment ----
rm(list = ls())

# Load the various datasets
load("../r-data/pdf-data.Rdata")
load("../r-data/finance-data.Rdata")


# Loading the required packages
# Generalist packages
# Data Wrangling and utility
library(tidyverse)

# Formatting
library(patchwork)
library(gt)
library(stargazer)

# Sentiment Analysis
library(SentimentAnalysis)
library(tm)
library(quanteda)


# ---- Fix Language Formatting ----
#Sys.setlocale("LC_ALL", "nb_NO.UTF-8")


# ---- Formatting Functions ----

## Function for cleaning text column
#
# Parameters:
# df - dataframe that contains column to be cleaned
# colToClean - column to clean

cleanTextDF <- function(df, colToClean) {

  redundant_words <- c(
    "torstein øye birgirsson",
    "sander",
    "eide",
    "dahling"
  )
  pattern <- paste(redundant_words, collapse = "|")

  return(
    df %>%
      mutate({{ colToClean }} := tolower({{ colToClean }}),
             {{ colToClean }} := gsub('[[:punct:]]', '', {{ colToClean }}),
             {{ colToClean }} := removeWords({{ colToClean }}, stopwords()),
             {{ colToClean }} := removeNumbers({{ colToClean }}),
             {{ colToClean }} := gsub(pattern, "", {{ colToClean }}),
             {{ colToClean }} := gsub("\\s+", " ", {{ colToClean }})
      )
  )
}

# Option 1: Create sentiment analysis with a dictionary approach LM
# # Create Document Term Matrix
# dtm <- pdf_df %>% 
#   cleanTextDF(pdf_text) %>% 
#   .[["pdf_text"]] %>% 
#   tokens() %>% 
#   dfm()
# 
# # Saving dtm
# save(dtm, file = "./analyst-data/dtm.Rdata")
load("../r-data/dtm.Rdata")

# Define Dictionary
define_dict <- function(file_path) {
  dict <- read_csv(file_path)
  
  positive_words <- dict$Word[dict$Positive > 0]
  negative_words <- dict$Word[dict$Negative > 0]
  uncertain_words <- dict$Word[dict$Uncertainty > 0]
  
  return(
    dictionary(list(positive = positive_words, 
                    negative = negative_words, 
                    uncertain = uncertain_words))
  )
}

LM <- define_dict("../dict/Loughran-McDonald_MasterDictionary_1993-2021.csv")

main_df <- dtm %>% 
  dfm_lookup(LM) %>% 
  convert(to = "data.frame") %>% 
  as_tibble %>% 
  
  # Adding Word Count
  mutate(word_count = ntoken(dtm)) %>% 
  
  # Adding a sentiment score (!!!)
  mutate(sentiment = (positive - negative)/(positive + negative),
         positive_percent = positive/word_count,
         negative_percent = negative/word_count) %>%
  
  # Appending the sentiment output to the main pdf dataframe
  cbind(pdf_df, .) %>% 
  select(-doc_id, -pdf_text) %>%
  mutate(date = as.Date(date)) %>% 
  
  # Adding the financial data to the data frame
  left_join(finance_df, by = c("ticker", "date")) %>% 
  
  # Standardizing the recommendation column
  mutate(recommendation = case_when(
    tolower(recommendation) %in% c("buy") ~ "BUY",
    tolower(recommendation) %in% c("hold") ~ "HOLD",
    tolower(recommendation) %in% c("sell") ~ "SELL",
    TRUE ~ recommendation
  )) %>% 
  arrange(analyst, company, date) %>% 
  
  # Filter reports made after 04-09-23
  filter(date < as.Date("2023-09-04")) %>% 
  
  # Change the target price to be relative to stock splits
  mutate(target_price_adj = target_price/stock_ratio) %>% 
  
  select(-ipo.x) %>% 
  rename(ipo = "ipo.y") %>% 
  
  # Cleaning up any groupings
  ungroup()
  
# Saving the main_df for further analysis
save(main_df, file = "../r-data/main_df.Rdata")

# ---- Test Bed ----
# main_df %>% 
#   group_by(analyst) %>% 
#   mutate(positive_negative_ratio = positive/negative) %>% 
#   summarise(mean_positive_percent = mean(positive_percent),
#             mean_negative_percent = mean(negative_percent),
#             mean_positive = mean(positive),
#             mean_negative = mean(negative))

main_df %>% 
  filter(is.na(target_price)) -> t


