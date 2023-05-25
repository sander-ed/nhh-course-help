library(tidyverse)
library(sentimentr)
library(SentimentAnalysis)
library(tm)
library(slam)

# Load dataset
  
  setwd("G:/Min disk/NHH/BAN432/Assignments/Assignment 6")
  load("firm_dataset.Rdata")
  
# Adding index numbers
  
  raw.data$index <- 1:nrow(raw.data)
  
  insurance.data <- 
    raw.data %>% 
    filter(industry.fama.french.49 == "46 Insur")
  
  rest.data <- 
    raw.data %>% 
    filter(!industry.fama.french.49 == "46 Insur")

# Creating three document term matrixes

  dtm <- DocumentTermMatrix(Corpus(VectorSource(section.7.mda)))
  dtm.insur <- DocumentTermMatrix(Corpus(VectorSource(section.7.mda[insurance.data$index])))
  dtm.rest <- DocumentTermMatrix(Corpus(VectorSource(section.7.mda[rest.data$index])))

# Computing negative sentiment. Expecting greater negative sentiment
# for insurance company
  
  insurance.data$fin.neg <- row_sums(
    dtm.insur[,dtm.insur$dimnames$Terms %in% DictionaryLM$negative]) /
    row_sums(dtm.insur)
  
  rest.data$fin.neg <- row_sums(
    dtm.rest[,dtm.rest$dimnames$Terms %in% DictionaryLM$negative]) /
    row_sums(dtm.rest)

# Creating a tibble for our responses
  
  tibble(
    'Mean Negativity Insurance' = mean(insurance.data$fin.neg),
    'Mean Negativity Rest' = mean(rest.data$fin.neg)
  ) -> negativity
  
# The final answer
  negativity
  