library(rvest)
library(tidyverse)
library(textrank)
library(udpipe)

tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

aapl10k <- read_html("aapl_q1_2016.xml")
intro10k <- html_nodes(aapl10k, "intro")

tagged <- udpipe_annotate(tagger, x = html_text2(intro10k)) %>% 
  as_tibble() %>% 
  filter()

sentences <- tagged %>% 
  select("sentence_id", "sentence") %>% 
  unique()

terminology <- tagged %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  select("sentence_id", "lemma")

tr <- textrank_sentences(data = sentences,
                         terminology = terminology)

summary <- summary(tr,
                   n = 10)
