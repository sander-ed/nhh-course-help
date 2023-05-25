##########################################################################
### BAN432 TERM PAPER - 11/21/22 - Candidate numbers: 26, 65, 7, 59    ###
### constructing a sentiment dictionary targeted at newspaper articles ###
##########################################################################


# Data Wrangling and utility
library(tidyverse)
library(tidytext)
library(rvest)
library(xml2)
library(tibble)
library(stringr)
library(dplyr)
library(tibble)

# Formatting
library(patchwork)
library(gt)
library(stargazer)

# Regression Model
library(tm)
library(topicmodels)
library(textir)
library(SentimentAnalysis)
library(tokenizers)
library(slam)
library(Matrix)



### --------------- ###
### EXTRACTING DATA ###
### --------------- ###


# Creating basic structure for main dataframe
# Stock: Ticker to firm
# ArticleId: article id assigned to article from wall street journal in html code
# Date: date article published on WSJ
# WC: word count of article registred on WSJ in metadata
# Article text: article text from WSJ
# 
# Returns empty dataframe

createDfBasicStructure <- function() {
  articlesDf <- tibble(
    stock = NA,
    article_id = NA,
    date = NA,
    WC = NA,
    article_text = NA
  )
  
  return(articlesDf)
}

### CREATING EMPTY MAIN DF WITH BASIC STRUCTURE
main.df <- createDfBasicStructure()



### CREATING DATASET WITH ALL PATHS AND FIRMS

## Reading zip file with all articles

# Unzip WSJdata if not in present WD
if(!"WSJdata" %in% list.files()) {
  unzip("WSJdata.zip")
}


# Loads zip file with all paths as DF
WSJ.data <- unzip("WSJdata.zip", list = T)


# Marking each path with Firm and iltering out unecessary paths
WSJ.firms <- WSJ.data %>% 
  mutate(Firm = Name) %>% 
  mutate_at("Firm", str_replace, "WSJdata/", "") %>% 
  separate(Firm, "Firm", "/") %>% 
  filter(Firm != "") %>%
  filter(Length != 0)



### Creating helper functions to extract and process data from WSJdata.zip


# Returns all article ids from html file
#
# Parameters:
# raw.data - raw html nodes from html file
#
# Returns all article id as vector
getArticleIds <- function(raw.data) {
  result <- raw.data %>% 
    html_nodes(".article") %>% 
    html_attr("id") 
  
  return (result[!is.na(result)])
}


# Gets html nodes nested from id to article
#
# Parameters:
# raw.data - htmlnode to files
# article.id - article id to specific article
#
# returns hmtl node to id
getHtmlNodesById <- function(raw.data, article.id) {
    raw.data %>% 
      html_nodes(paste0("#", article.id))
  
}


# Gets article text to html node passed to function
# 
# Parameters: 
# html.nodes.by.id - htmlnodes extracted by specific id
#
# Returns article text as one character string
extract_htmltext <- function(html.nodes.by.id) {
  html.nodes.by.id %>% 
    html_elements(".articleParagraph") %>% 
    html_text2() %>% 
    head(-1) %>% 
    paste(collapse = " ")  
}


# Gets meta data from article as table 
#
# Parameters:
# raw.data: html structure from article
# articleid: article id to subtract date
#
# Returns metadata from article as dataframe
getTableInfoArticle <- function(html.nodes.by.id) {
  table.article <- html.nodes.by.id %>% 
    html_table()
  
  return(table.article[[1]]) 
}

# Gets specific value from meta data to article
#
# Parameters:
# table.article.df - metadata as df
# row.look - which row to look up passed as abbreviation
#
# Retrurns value corresponding to abbreviation look up
getMetaDataByRow <- function(table.article.df, row.look) {
  for (i in 1:nrow(table.article.df)) {
    if(table.article.df[i, 1] == row.look) {
      row.value = table.article.df[i, 2]$X2[1] 
      break
    } 
  }
  
  return(row.value)
}


# Gets date from when an article is published (Abbreviation: PD)
#
# Parameters:
# table.article.df - Dataframe containing metadata of article
#
# returns date article is published formatted as date 
getDate <- function(table.article.df) {
  
  return(as.Date(getMetaDataByRow(table.article.df, "PD"),"%d %B %Y"))
}

# Gets Word count of article (Abbreviation: WD)
# Parameters
# table.article.df: Dataframe containing metadata of 
# Returns word count as number
getWC <- function(table.article.df) {

  return(as.numeric(
    trimws(gsub("words", "", getMetaDataByRow(table.article.df, "WC"))))
         )
}



# Adding main dataframe with text from WSJ data
# Note: in order to work WSJ data has to be stored in WD as 
# WSJdata/firm/article.htm
#
# Parameters: 
# main.df - dataframe to populate
# dfpaths - dataframe that contains paths to text file
#
# Returns main dataframe populated

populateDfFirm <- function(main.df, dfpaths) {
  
  for(i in 1:nrow(dfpaths)) {
    
    firm <- dfpaths$Firm[i] # Firm to extract article info from
    sub.path <- dfpaths$Name[i] # Path to file
    rawHtml.article <- read_html(sub.path) # Reading html file. 
    
    
    extracted.article.ids <- getArticleIds(rawHtml.article) # Getting all article ids in file
    
    for(id in extracted.article.ids) { # Looping through each article in file
      if(!id %in% main.df$article_id) { # Some ids appear in multiple files - stops articles to be registered multiple times
        
        html.nodes.id <- getHtmlNodesById(rawHtml.article, id) 
        table.info <- getTableInfoArticle(html.nodes.id) # Getting meta data from article
        
        new.row <- c(firm, 
                     id, 
                     getDate(table.info),
                     getWC(table.info), 
                     extract_htmltext(html.nodes.id))
        
        main.df <- rbind(main.df, new.row) # Adding article to main dataframe
      }
    }
    
  }
  
  return(main.df) 
  
}



## Function for cleaning text column
#
# Parameters:
# df - dataframe that contains column to be cleaned
# colToClean - column to clean
cleanTextCOlumn <- function(df, colToClean) {
  
  return(
    df %>% 
      mutate({{ colToClean }} := tolower({{ colToClean }}),
             {{ colToClean }} := gsub('[[:punct:]]', '', {{ colToClean }}),
             {{ colToClean }} := removeWords({{ colToClean }}, stopwords()),
             {{ colToClean }} := removeNumbers({{ colToClean }}),
             {{ colToClean }} := gsub('\\s+', ' ', {{ colToClean }}),
             {{ colToClean }} := trimws({{ colToClean }}))
  )
}



## WARNING: takes a long runtime running main.df line below

# Populating main df with test data
# Cleaning text data with unecessary characteres
main.df <-  populateDfFirm(main.df, WSJ.firms) %>%
              mutate_at('date', as.numeric) %>% 
              mutate(date = as.Date(date, origin = '1970-01-01'),
                     date_inf1 = date + 1,
                     date_lag1 = date - 1) %>% 
              na.omit() %>% 
              cleanTextCOlumn(article_text)




## Merging data for vizualization and model
## Setting up main dataframe for analysis

returns.df <- read.csv("dailyReturn.csv") %>% 
                  mutate(date = as.Date(date)) 


## Adding stock price day before and day after to main.df

main.df <- main.df %>%   
  
  # First we add the returns from the same day the article is published
  left_join(returns.df, by = c("date" = "date", "stock" = "ticker")) %>% 
  rename(ret_t = "ret") %>% 
  select(stock, date, date_lag1, date_inf1, ret_t, article_id, article_text, WC) %>% 
  
  # Then we add the returns from the day before the article is published
  left_join(returns.df, by = c("date_lag1" = "date", "stock" = "ticker")) %>% 
  rename(ret_lag1 = "ret") %>% 
  select(stock, date, date_lag1, date_inf1, ret_t, ret_lag1, article_id, article_text, WC) %>% 
  
  # Lastly we add the returns from the day after the article is published
  left_join(returns.df, by = c("date_inf1" = "date", "stock" = "ticker")) %>% 
  rename(ret_inf1 = "ret") %>% 
  select(stock, permno, date, ret_t, prc, vol, date_lag1, ret_lag1, date_inf1, ret_inf1, article_id, article_text, WC)
   


##### -------------------- #####
#####       TASK 1         #####
##### -------------------- #####


# Improve readability
options(scipen = 50, digits = 4)


# Part I: Plotting number of articles against volume and returns ----

# Summarizing the data by each company in order to present the
# mean values of the data set per company. We do this to 
# find systematic patterns and correct for varying amount of
# time each company has been listed on the stock exchange
main.df.returns <- main.df %>% 
  select(stock, date, prc, vol, ret_t) %>% 
  group_by(stock) %>% 
  summarise(mean_vol = mean(vol, na.rm = T),
            mean_ret = mean(ret_t, na.rm = T),
            n_articles = n(),
            time_frame_days = as.integer(max(date) - min(date))) %>% 
  mutate(mean_articles = n_articles/time_frame_days) %>% 
  filter(!is.na(mean_ret)) 

# After summarizing the data it is possible to plot each company on
# two graphs with volume and price as explanatory variables

# Volume ----
# First we plot mean articles published per day against mean
# volume per trading day. 
p1 <- main.df.returns %>% 
  ggplot(aes(x = mean_vol, y = mean_articles)) +
  
  # Jitter in order to plot the data and smooth to find trends
  geom_jitter(size = 1.5, aes(col = time_frame_days)) +
  geom_smooth(method = "lm", linetype = "dashed") +
  
  # Formatting
  scale_color_gradient(low="red", high="green") +
  scale_x_continuous(limits = c(0, 30000000)) +
  labs(x = "Mean Volume",
       y = "Mean Articles per day",
       title = "Articles against volume",
       subtitle = "Average number of articles per day in the WSJ measured against average volume traded",
       col = "Time frame on SE")+
  theme_classic() 


# Returns ----
# First we plot mean articles published per day against mean
# return per trading day. 
p2 <- main.df.returns %>% 
  ggplot(aes(x = mean_ret, y = mean_articles)) +
  
  # Jitter in order to plot the data and hline to visualize zero
  geom_jitter(size = 1.5, aes(col = time_frame_days)) +
  geom_smooth(method = "lm", linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  
  # Formatting
  scale_color_gradient(low="red", high="green") +
  labs(x = "Mean Return",
       y = "Mean Articles per day",
       title = "Return against volume",
       subtitle = "Average number of articles per day in the WSJ measured against average return on stock",
       col = "Time frame on SE")+
  theme_classic() 

# We patch the two plots together. Note that some outliers are missing from
# both tables, although they are very large outliers compared to the rest
# of the dataset
p1/p2

# Part II: Volatility around published article

# In order to determine the adequate time frame from when an article
# is published to when the market reacts we need to wrangle our data
# in a way which presents the returns relative to a published article

# We use absolute values in order to find the volatility of a stock, as
# we assume that it is the change that drives newsworthiness, not the initial 
# value or if the change is positive/negative. 

# Following this, we do this with a series of left_joins from the return.df into the main.df
return.articles <- main.df %>% 
  select(stock, date, date_lag1, date_inf1, ret_t, ret_lag1, ret_inf1) %>% 
  # In order to test for volatility, we turn the returns into absolute values
  mutate(ret_t = abs(ret_t),
         ret_lag1 = abs(ret_lag1),
         ret_inf1 = abs(ret_inf1))


# Then we can summarize the data to measure volatility for each of
# the days relative to when an article is published

# First we calculate the mean for each day, and add the total mean
return.articles %>% 
  summarise(ret_lag1 = mean(ret_lag1, na.rm = T),
            ret_t = mean(ret_t, na.rm = T),
            ret_inf1 = mean(ret_inf1, na.rm = T)) %>% 
  mutate(mean_ret = mean(abs(returns.df$ret))) -> mean.returns

# We can also add a row for the number of observations
# for each type of return
tibble(ret_lag1 = length(return.articles$ret_lag1[!is.na(return.articles$ret_lag1)]),
       ret_t = length(return.articles$ret_t[!is.na(return.articles$ret_t)]),
       ret_inf1 = length(return.articles$ret_inf1[!is.na(return.articles$ret_inf1)]),
       mean_ret = length(returns.df$ret[!is.na(returns.df$ret)])) -> return.obs

# Then we calculate the sd and combine the values
return.articles %>% 
  summarise(ret_lag1 = sd(ret_lag1, na.rm = T),
            ret_t = sd(ret_t, na.rm = T),
            ret_inf1 = sd(ret_inf1, na.rm = T)) %>% 
  mutate(mean_ret = sd(abs(returns.df$ret))) %>% 
  add_row(mean.returns, .) %>%
  add_row(return.obs, ., .before = 1) %>% 
  add_column(Return = c("Mean", "Std", "N"), .before = 1) %>% 
  rename('Day t' = ret_t,
         'Day t-1' = ret_lag1,
         'Day t+1' = ret_inf1,
         'For all days' = mean_ret) -> summary.table



# Plotting results to table
summary.table %>% 
  gt::gt() %>% 
  tab_header(
    title = md("Volatility relative to article publication date"),
    subtitle = md("Where `day t` is article publication date")
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  tab_source_note(
    source_note = md("Data gathered from transformed `main.df` and `returns.df` datasets")
  ) %>% 
  tab_footnote(
    footnote = md("*Volatility* refers to the absolute value of return"),
    locations = cells_title(groups = "title")
  ) %>% 
  fmt_number(
    columns = c("Day t-1", "Day t","Day t+1","For all days"),
    rows = 3,
    use_seps = F,
    decimals = 0
  ) %>% 
  cols_align(
    align = "center"
  )



# In order to measure whether these means are significantly different than 
# the mean for all days, we can create a two-way t-test which measures the
# various t-values and shows us whether we can throw away our null-hypothesis
# of the two means being the same.

# T-test using summary statistics

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  
  return(dat) 
}


# Day t-1
t.test2(m1 = summary.table$`Day t-1`[1],
        m2 = summary.table$`For all days`[1],
        s1 = summary.table$`Day t-1`[2],
        s2 = summary.table$`For all days`[2],
        n1 = summary.table$`Day t-1`[3],
        n2 = summary.table$`For all days`[3]) -> Day_tminus1_ttest

# Day t
t.test2(m1 = summary.table$`Day t`[1],
        m2 = summary.table$`For all days`[1],
        s1 = summary.table$`Day t`[2],
        s2 = summary.table$`For all days`[2],
        n1 = summary.table$`Day t`[3],
        n2 = summary.table$`For all days`[3]) -> Day_t_ttest

# Day t+1
t.test2(m1 = summary.table$`Day t+1`[1],
        m2 = summary.table$`For all days`[1],
        s1 = summary.table$`Day t+1`[2],
        s2 = summary.table$`For all days`[2],
        n1 = summary.table$`Day t+1`[3],
        n2 = summary.table$`For all days`[3]) -> Day_tplus1_ttest

# These results show that the p-value is within our designated
# threshold, and we can reject the null hypothesis of the 
# mean volatility around a published article is the same
# as the mean for all returns


##### -------------------- #####
#####       TASK 2         #####
##### -------------------- #####

## CREATE TRAINING SET OF FIRMS

# Creating set of firms consisting only firms that exists in both WSJ data and returns data

# Finding unique firms in rettuns df
return.firms.unique <- returns.df %>% 
  select(ticker) %>% 
  distinct() %>% 
  rename(Firm = 1)

## Finding unique firms from WSJ data
wsj.firms.unique <- WSJ.firms %>% 
  distinct(Firm)

# Gets firms that are both in text data and return data
# We will not analyze firms that are not present in both datasets
firms.unique.data <- return.firms.unique %>% 
  inner_join(wsj.firms.unique)

# Set seed for analysis
set.seed(150)

# Vector for firms in training set - 50% of firms in both data sets
firms.training <- firms.unique.data[sample(nrow(firms.unique.data),  
                                           length(firms.unique.data$Firm)/2), ] 
# Vector for firms in testset
firms.testset <- setdiff(firms.unique.data$Firm, firms.training)


# Creating column for marking training (A group) and test (B group) firms
main.df$type <- NA
main.df$type[main.df$stock %in% firms.training] <- "training"
main.df$type[main.df$stock %in% firms.testset] <- "test"


### ---------------------------------------------- ###
##        DATA WRANGLING SPECIFIC FOR MODEL        ###
### ---------------------------------------------- ###
# Method:
# Concanating each article given on same day
# Filtering out articles out of given bounds based on word count
### ---------------------------------------------- ###


# Creating dataframe to calculate concanated WC in articles  
main.df.WC <- main.df %>% ## Code for word count
  mutate(WC = as.numeric(WC)) %>%     
  group_by(stock, date) %>% 
  summarise(WC_count = sum(WC)) %>% 
  select(stock, date, WC_count)


# Find number of articles below and over 2500 words
main.df.WC %>% 
  mutate(under2500 = ifelse(WC_count <= 2500, "Under 2500 words", "Over 2500 words")) %>% 
  group_by(under2500) %>% 
  summarise(n = n())


# Plotting distribuitons of word count
ggplot(main.df.WC, aes(x = WC_count)) +
  geom_density(fill="grey",cex = 0.8, alpha = 0.5) + 
  geom_vline(xintercept = 2500, linetype = "dashed") +
  geom_text(aes(x=2505, label="98,9% of data", y=.0006), angle=90, vjust = 1.2, text=element_text(size=)) +
  ylab("Density") +
  xlab("Word count") +
  ggtitle("Distribuition over WSJ article`s word count", subtitle = "98,9% of articles consists of less than 2 500 words") +
  theme_classic()


# Concanating articles and updating concanated WC
main.df <- main.df %>% 
  group_by(stock, date) %>%
  mutate(conc_article_text = paste0(article_text, collapse = " ")) %>% 
  distinct_at(vars(-article_text, -WC, -article_id)) %>% 
  left_join(main.df.WC, by = c("stock" = "stock", "date" = "date")) %>% 
  rename("WC" = "WC_count",
         "article_text" =  "conc_article_text") %>% 
  filter(!is.na(ret_lag1))  


# Store copy of main.df before filtering word count for later analysis
main.df.wc <- main.df 


# Filtering out articles on upper and lower bound
main.df <- main.df %>%
  filter(WC <= 2500, WC >= 240) 


### RUNNING MNIR REGRESSION TO CONSTRUCT SENTIMENT DICTIONARY ###
# NOTE: code is constructed based on lecture code 15 as inspiration
# Downloaded from BAN432 11/14/22


#Creating corpus based on article texts
corpus <- Corpus(VectorSource(main.df$article_text))                           


## Function for creating Document term matrix - 
# created as we fit multiple times in document
#
# Parameters:
# Corpus - to create DTM based on
# wordlenghts.vector - bounds to filter out word lengths
# bounds - bound to filter numbers of times words appears in documents
# Retunrs created DTNM
createDTM <- function(corpus, wordlenghts.vector, bounds.vector) {
  return(
    DocumentTermMatrix(corpus,                                              
                       control = list(                                   
                         removePunctuation = T,                         
                         stopwords = T,                                 
                         stemming = T,                                  
                         removeNumbers = T,
                         wordLengths = wordlenghts.vector,
                         bounds = list(global = bounds.vector)))
  ) 
}



# Creates DTM based on corpus
# Word length bounds: lower (4) and upper (20)
# Word bounds: lower (2500) and upper (5500)
dtm_full <- createDTM(corpus, c(4, 20), c(2500, 5500))


# Filtering the DTM based on training set
dtm_full <- dtm_full[ , col_sums(dtm_full[main.df$type == "training",]) != 0 ]


## Function for running fit to dtm based on dataframe - 
# created as we fit multiple times in document
#
# Parameters:
# df - that has column to create fit to
# dtm - that holds terms to fit to colu,mn
# 
# Returns ran fit
runFitParallell <- function(df, dtm) {
  cl <- makeCluster(5)
  
  # fit the model
  fits <- dmr(cl, 
              covars = df[df$type == "training", "ret_lag1"], 
              counts = dtm[df$type == "training",], 
              bins=NULL, 
              gamma=40,  # sparsity
              nlambda=10, 
              verb= 2)
  
  stopCluster(cl)
  
  return(fits)
}



# fits the model based on main df and DTM
fits <- runFitParallell(main.df, dtm_full)

# predict using the model
proj <- srproj(fits, as.matrix(dtm_full))


# Extracting SR score and storing in main.df så each row has sentiment score
main.df$z <- scale(proj[,1]) 

# Storing a vector with each term and their MNIR coefficient
# This is used later in task 4 to predict returns based on transcripts
mnir.coef <- sort(coef(fits)[2,])


# Vectors for positive and negative terms
constructed.Dict.pos<- sort(mnir.coef[which(mnir.coef > 0)], decreasing = TRUE)
constructed.Dict.neg <- sort(mnir.coef[which(mnir.coef < 0)], decreasing = FALSE)


# Plotting tokens in table
table.length <- 10
tibble('Pos Tokens' = names(constructed.Dict.pos)[1:table.length],
       'MNIR Coef Pos' = constructed.Dict.pos[1:table.length],
       'Neg Tokens' = names(constructed.Dict.neg)[1:table.length],
       'MNIR Coef Neg' = constructed.Dict.neg[1:table.length]) %>% 
  gt::gt() %>% 
  tab_header(
    title = md("Results from sentiment analysis"),
    subtitle = paste("The", table.length, "highest and lowest scoring words")
  ) %>% 
  fmt_number(
    columns = c("MNIR Coef Pos","MNIR Coef Neg"),
    use_seps = F,
    decimals = 3
  ) %>% 
  cols_align(
    align = "center"
  )


##### -------------------- #####
#####       TASK 3         #####
##### -------------------- #####


# Part 1 ------

# Function for running regression model on filtered set
# Runs regression with sentiment as explanatory variable
# Parameters:
# FittedData - df with fitted data and column with sentiment and returns
# colFilter - column to be filtered
# filterDataBy - what value column is filtered by
# Returns regression model
reg.z <- function(fittedData, colFilter, filterDataBy) {
  
  col <- enquo(colFilter) ## Must enquo columns for dplyr functions
  
  reg <- lm(
    ret_lag1 ~  z, 
    data = fittedData %>% filter(!!col == filterDataBy))
  
  return(reg)
}


# Running regression on training set to predict returns
m_training <- reg.z(main.df, type, "training")

# Running regression on test set to predict returns
m_test <- reg.z(main.df, type, "test")


# plotting results from two regression in terminal
#
# Parameters:
# m1 - first regression
# m2 - second regression
plotRegression <- function(m1, m2) {
  stargazer(list(m1, m2), 
            type = "text", 
            keep.stat=c("n", "adj.rsq"), 
            report=('vc*t'))
}


# Plotting results
plotRegression(m_training, m_test)



# Saving regression in table
stargazer(list(m_training, m_test), 
          type = "text", 
          keep.stat=c("n", "rsq"),
          style = "default", 
          report=('vc*t'),
          
          covariate.labels = c("Sentiment"),
          column.separate = c(2),
          
          dep.var.caption = '<font size="+1">Main model</font>',
          dep.var.labels = '<font size="+1">Ret t-1</font>',
          
          add.lines = list(c("Test or Train?","Test", "Train")),
          
          out = "plots/mainModel.html")



# Plotting regression with training set in ggplot
# sentiment by x-axis and return t - 1 on y-axis
ggplot(main.df %>% filter(type == "training"), aes(x = z, y = ret_lag1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Ret t-1") +
  xlab("Sentiment score") +
  ggtitle("MNIR plot for main model on group A", subtitle = "Sentiment score in relation with returns day before article published") +
  theme_classic() -> reg1



# Plotting regression with test set in ggplot
# sentiment by x-axis and return t - 1 on y-axis
ggplot(main.df %>% filter(type == "test"), aes(x = z, y = ret_lag1)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(-.5,1) +
  xlim(-6,6) + ## Manually fixing axes to fit regression on model A
  ylab("Ret t-1") +
  xlab("Sentiment score") +
  ggtitle("MNIR plot for main model on group B", subtitle = "Sentiment score in relation with returns day before article published") +
  theme_classic() -> reg2



# Pathing plots together
reg1/reg2


# Part 2 ------

#### Splitting Group B firms with lots of articles (n articles)

# Creating column with distinction in many and few articles


## Creating column for n articles

main.df <- main.df %>% 
  group_by(stock) %>%
  summarise(n_articles = n()) %>% 
  inner_join(main.df) 


# Found quantile that splitted set in two, may need to be changed if dataset changes

quantile.half.articles <- c(0, .25)

  

## Printing how many firms in each group to check approxamentally 50% in each group

main.df %>% 
  filter(type == "test", 
         n_articles <= quantile(main.df$n_articles, probs = quantile.half.articles)[2]) %>% 
  distinct(stock) %>% 
  nrow()

main.df %>% 
  filter(type == "test", 
         n_articles > quantile(main.df$n_articles, probs = quantile.half.articles)[2]) %>% 
  distinct(stock) %>% 
  nrow()



# Creating new column based on result 50/50 in each group
main.df <- main.df %>% 
  mutate(group_test = 
           ifelse(n_articles <= quantile(main.df$n_articles, probs = quantile.half.articles)[2], 
                  "Few articles", 
                  "Many articles"))



# Running new regression on smaller groups
m.few.articles <- reg.z(main.df %>% filter(type == "test"), 
                        group_test, 
                        "Few articles")

m.many.articles <- reg.z(main.df %>% filter(type == "test"), 
                         group_test, 
                         "Many articles")


# Plotting results
plotRegression(m.few.articles, m.many.articles)


# Saving regression as table
stargazer(list(m.few.articles, m.many.articles), 
          type = "html", 
          keep.stat=c("n", "rsq"),
          style = "default", 
          report=('vc*t'),
          
          covariate.labels = c("Sentiment"),
          column.separate = c(1, 1),
          
          dep.var.caption = '<font size="+1">Regression splitted on n articles</font>',
          column.labels = c("Few articles", "Many articles"),
          dep.var.labels = '<font size="+1">Ret t-1</font>',
          
          add.lines = list(c("Test or train?","Test", "Test")),
          
          out = "plots/narticlesnModel.html")



# Part 3 ------

#### TUNING PARAMETERS TO SHOW DIFFERENCE IN RESULTS
# WARNING: TAKES LOTS OF CPU AND MEMORY 

## Fit 1: change in bounds

main.df.bounds <- main.df

dtm.bounds <- createDTM(corpus, c(3, 20), c(50, 8000))

fits.bounds <- runFitParallell(main.df.bounds, dtm.bounds)

proj.bounds <- srproj(fits.bounds, as.matrix(dtm.bounds))

main.df.bounds$z <- scale(proj.bounds[, 1]) 


## Fit 2: change in word length

main.df.wl <- main.df

dtm.wl <- createDTM(corpus, c(5, 10), c(2500, 5500))

fits.wl <- runFitParallell(main.df.wl, dtm.wl)

proj.wl <- srproj(fits.wl, as.matrix(dtm.wl))

main.df.wl$z <- scale(proj.wl[, 1]) 


## Fit 3: change in word count 

main.df.wc <- main.df.wc # Copy from start of model data wrangling

corpus.wc <- Corpus(VectorSource(main.df.wc$article_text)) # Needs new corpus as there are more articles

dtm.wc <- createDTM(corpus.wc, c(3, 20), c(2500, 5500))

fits.wc <- runFitParallell(main.df.wc, dtm.wc)

proj.wc <- srproj(fits.wc, as.matrix(dtm.wc))

main.df.wc$z <- scale(proj.wc[, 1]) 


# Running regressions
m1 <- reg.z(main.df.bounds, type, "training")
m2 <- reg.z(main.df.bounds, type, "test")

m3 <- reg.z(main.df.wl, type, "training")
m4 <- reg.z(main.df.wl, type, "test")

m5 <- reg.z(main.df.wc, type, "training")
m6 <- reg.z(main.df.wc, type, "test")



## Plot bounds - we expect significanse in training set and high R squared - and opposite in training set
plotRegression(m1, m2)

## Plot word length - we expect significanse in training set and high R squared - and opposite in training set
plotRegression(m3, m4)

## Plot word count - we expect lower R squared but still significance in both sets NB: trenger e plott pe filtreingen med og uten
plotRegression(m5, m6)



# Saving regression table
stargazer(m_training, m_test, m1, m2, m3, m4, m5, m6,
          
          type = "html",
          keep.stat=c("n", "rsq"), 
          report=('vc*t'),
          style = "default",
          
          covariate.labels = c("Sentiment"),
          column.labels = c("Main Model", "Bounds", "Word Length", "Word Count"),
          column.separate = c(2,2,2,2),
          
          dep.var.caption = '<font size="+1">Regressions with parameter tuning</font>',
          dep.var.labels = '<font size="-1">Results from tuned parameters juxtaposed against the main model',
          
          add.lines = list(c("Test or Train?","Test", "Train", "Train", "Test", "Train", "Test", "Train", "Test")),
          out = "plots/regressionCompare.html")


##### -------------------- #####
#####       TASK 4         #####
##### -------------------- #####


# Loading earning call data
load("earningsCallsTranscripts.Rdata")



# Cleaning intro and qa text columns
transcripts <- cleanTextCOlumn(transcripts, intro)
transcripts <- cleanTextCOlumn(transcripts, qa)


# Joining transcripts with return data - joining by same date
transcripts.main <- transcripts %>% 
  left_join(returns.df, by = c("publishedOn" = "date", "permno" = "permno")) %>% 
  na.omit() 
  


# Extracting positive and negative vectors from dictionary created from task 2
constructed.Dict.terms <- names(mnir.coef)
constructed.Dict.pos <- mnir.coef[which(mnir.coef > 0)]
constructed.Dict.neg <- mnir.coef[which(mnir.coef < 0)]


# Creating one corpus for each column
corpus.intro <- Corpus(VectorSource(transcripts.main$intro))  
corpus.qa <- Corpus(VectorSource(transcripts.main$qa))



# Setting up word length bounds and bounds for document term matrix
transcript.word.length <- c(4,20)
transcript.bounds <- c(20, 40)

## Creating one corpus for eacn column - shpuld they be combined?
dtm.transcripts.intro <- createDTM(corpus.intro, transcript.word.length, transcript.bounds) 
dtm.transcripts.qa <- createDTM(corpus.qa, transcript.word.length, transcript.bounds)


## Calculating sentiment score - method is equal to calculating based on LM dict in lecture
# Need to pass words as character vector
#
# Parameters:
# dict.sentiment - sentiment vector containing terms and weights - needs to be passed as character vector
# dtm.text - document term matrix from corpus
#
# Returns vector with scores
calc.sentiment.score <- function(dict.sentiment, dtm.text) {
  return( 
    row_sums(
      dtm.text[,dtm.text$dimnames$Terms %in% dict.sentiment])/
      row_sums(dtm.text)
  )
}



# Calculating sentiment score for intro
transcripts.main$dict.pos.intro <- calc.sentiment.score(names(constructed.Dict.pos), 
                                                        dtm.transcripts.intro)

transcripts.main$dict.neg.intro <- calc.sentiment.score(names(constructed.Dict.neg), 
                                                        dtm.transcripts.intro)

# Calculating sentiment score for qa
transcripts.main$dict.pos.qa <- calc.sentiment.score(names(constructed.Dict.pos), 
                                                     dtm.transcripts.qa)

transcripts.main$dict.neg.qa <- calc.sentiment.score(names(constructed.Dict.neg), 
                                                     dtm.transcripts.qa)

  

## Running regression 
m1 <- lm(
  ret ~ dict.neg.qa + dict.pos.qa, 
  data = transcripts.main)


#Plotting results
stargazer(m1, type = "text")


# Saving regression table
stargazer(list(m_training, m_test), 
          type = "text", 
          keep.stat=c("n", "rsq"),
          style = "default", 
          report=('vc*t'),
          
          covariate.labels = c("Sentiment"),
          column.separate = c(2),
          
          dep.var.caption = '<font size="+1">Main model</font>',
          dep.var.labels = '<font size="+1">Ret t-1</font>',
          
          add.lines = list(c("Test or Train?","Test", "Train")),
          
          out = "plots/mainModel.html")



# Splitting into group A (training) and B (test)
transcripts.main$type = NA
transcripts.main$type[transcripts.main$ticker %in% firms.training] <- "training"
transcripts.main$type[transcripts.main$ticker %in% firms.testset] <- "test"


# Running regression
m2 <- lm(
  ret ~   dict.neg.qa + dict.pos.qa, 
  data = transcripts.main %>% filter(type == "training"))

m3 <- lm(
  ret ~  dict.neg.qa + dict.pos.qa, 
  data = transcripts.main %>% filter(type == "test"))


# Plotting regression in terminale
plotRegression(m2, m3)


# Saving regression table
stargazer(list(m1, m2, m3), 
          type = "html", 
          keep.stat=c("n", "rsq", "adj.rsq"),
          style = "default", 
          report=('vc*t'),
          
          covariate.labels = c("Negative sentiment", "Postive sentiment"),
          column.separate = c(1, 2),
          
          dep.var.caption = '<font size="+1">Transcript of earning calls and stock return</font>',
          column.labels = c("Full sample", "Splitted sample"),
          dep.var.labels = '<font size="+1">Ret t</font>',
          
          add.lines = list(c("Test or train?","Full set", "Training", "Test")),
          
          out = "plots/transcriptsModel.html")








