# ---- Assignment 3 of BAN432 ----

# Load relevant libraries

  require(tidytext)
  require(readr)
  require(dplyr)
  require(udpipe)
  require(wordcloud)
  require(tm)
  require(tidyverse)

# Set working directory

  #setwd("")

# Load in the supplied data set

  load("firm_dataset.Rdata")
  
# Load the udpipe model for English
  
  tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  
# Use the tagger to annotate
  
  pos.test <- udpipe_annotate(tagger, x = section.1.business[1]) %>%
    as_tibble() %>%
    select(doc_id, token, lemma, xpos, upos)
  
# Then we add the business descriptions to the raw data

   data.desc <- raw.data %>% 
    mutate(doc_id = row_number()) %>% 
    filter(industry.fama.french.49 == "30 Oil" | industry.fama.french.49 == "31 Util") %>% 
     select(doc_id, industry.fama.french.49)

# We filter out the industries we want to analyse

   data.oil <- data.desc %>% 
     filter(industry.fama.french.49 == "30 Oil")
   
   data.util <- data.desc %>% 
     filter(industry.fama.french.49 == "31 Util")


# Then we can tag all the business descriptions for the industries we want  
 
# Oil sector 
  sec.oil <- udpipe_annotate(tagger, x=section.1.business[c(data.oil$doc_id)]) %>% 
    as_tibble()
  
  word.oil <- sec.oil %>% 
    filter(grepl("^N.+", xpos) | grepl("^RB.+", xpos)) %>% 
    select(token, lemma, upos, xpos) %>% 
    count(lemma, sort = T)
  
  png(filename = "oil.png",
      units = "cm",
      width = 20,
      height = 20,res = 300)
  wordcloud(word = word.oil$lemma[1:100],
            freq = word.oil$n[1:100])
  dev.off()
  

  
  
  # Util sector
  sec.util <- udpipe_annotate(tagger, x=section.1.business[c(data.util$doc_id)]) %>% 
    as_tibble()
  
  word.util <- sec.util %>% 
    filter(grepl("^N.+", xpos) | grepl("^RB.+", xpos)) %>% 
    select(token, lemma, upos, xpos) %>% 
    count(lemma, sort = T)

  png(filename = "util.png",
      units = "cm",
      width = 20,
      height = 20,res = 300)
  wordcloud(word = word.util$lemma[1:100],
            freq = word.util$n[1:100])
  dev.off()
     

  
       

   
  
  

  
  