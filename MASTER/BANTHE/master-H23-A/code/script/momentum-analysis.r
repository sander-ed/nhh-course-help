# Load libraries
library(quantmod)
library(tidyverse)
library(readr)
library(tidyquant)
library(quantmod)
library(stringr)
library(TTR)
library(mark)
library(stringr)
library(furrr)
library(zoo)


# ---- Load PDF text data ----
load("../r-data/pdf-data.Rdata")

# ---- Download the stock data ----

# Load the tickers
load_tickers <- function(){
  tickers_dict <- read_delim("../dict/ticker-companies.csv", delim = ";", show_col_types = FALSE) %>% 
    select(-ipo)
  return(c(tickers_dict[,"ticker"])[[1]])
}

tickers <- load_tickers()


# ---- Financial Functions ----

# Initialize MACD function
myMACD <- function (price,S,L,K){
  MACD <- EMA(price,S) - EMA(price,L)
  signal <- EMA(MACD,K)
  indication <- MACD - signal
  output <- cbind(MACD,signal, indication)
  colnames(output) <- c("MACD","signal", "indication")
  return(output)
}

# Initialize RSI function
myRSI <- function (price,n){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- 1
    } else {
      D[i] <- 1
    }
    if (i>n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}

# Compute ADX
adx_compute <- function(finance_data) {
  
  # Define a helper function for ADX calculation
  compute_adx <- function(data) {
    date <- data$Time
    xts_obj <- xts(data[, c("open", "high", "low", "close")], order.by = data$Time)
    adx_val <- ADX(xts_obj, n = 14)
    return(
      as_tibble(adx_val) %>% 
        cbind(date)
    )
  }
  
  result <- finance_data %>%
    select(ticker, date, open, high, low, close) %>%
    mutate(Time = as.POSIXct(paste(date), format="%Y-%m-%d")) %>%
    group_by(ticker) %>%
    do(compute_adx(.)) %>%
    left_join(finance_data, by = c("ticker", "date"))
  
  return(result)
}

get_dividends_safe <- function(ticker, from, to) {
  tryCatch({
    div_data <- tq_get(ticker, get = "dividends", from = from, to = to) %>%
      mutate(dividend = 1)
    return(div_data)
  }, error = function(e) {
    return(NULL)
  })
}

get_splits_safe <- function(ticker, from, to) {
  tryCatch({
    div_data <- tq_get(ticker, get = "splits", from = from, to = to)
    return(div_data)
  }, error = function(e) {
    return(NULL)
  })
}


# ---- Application ----
# Using the tq_get from tidyquant, we download the appropriate data from yahoo finance

all_dividends_list <- lapply(tickers, get_dividends_safe, from = "2016-01-01", to = "2023-10-01")
all_dividends <- bind_rows(Filter(Negate(is.null), all_dividends_list))
rm(all_dividends_list)

all_splits_list <- lapply(tickers, get_splits_safe, from = "2018-01-01", to = "2023-10-01")
all_splits <- bind_rows(Filter(Negate(anyNA), all_splits_list))
rm(all_splits_list)

{
  finance_data <- tq_get(tickers, 
                         from = as.POSIXct("2016-01-01"),       # Start Date Download
                         to = as.POSIXct("2023-10-01") ) %>%    # End Date Download
    arrange(symbol, date) %>% 
    filter(!is.na(close)) %>% 
    rename(ticker = "symbol") %>%
    left_join(all_dividends, by = c("ticker" = "symbol", "date")) %>%
    tidyr::replace_na(list(dividend = 0, value = 0)) %>%   # Replace NA with 0 for dividends
    rename(dividend_value = "value") %>% 
    left_join(all_splits, by = c("ticker" = "symbol", "date")) %>% 
    tidyr::replace_na(list(splits = 0, value = 0)) %>%   # Replace NA with 0 for splits
    rename(split = "value") 
  
  finance_data <- adx_compute(finance_data)
  
  finance_data <- finance_data %>%
    group_by(ticker) %>%
    mutate(
      #MACD = myMACD(close, 12, 26, 9)[, "MACD"],
      #Signal = myMACD(close, 12, 26, 9)[, "signal"],
      #MACD_Indication = myMACD(close, 12, 26, 9)[, "indication"],
      RSI = RSI(close, 14),
      SMA_20 = SMA(close, n = 20),
      SMA_50 = SMA(close, n = 50),
      SMA_100 = SMA(close, n = 100),
      SMA_200 = SMA(close, n = 200),
      RETURN_1Y = (close - lag(open, 252)) / lag(open, 252),
      RETURN_6M = (close - lag(open, 126)) / lag(open, 126),
      RETURN_3M = (close - lag(open, 63)) / lag(open, 63),
      RETURN_1M = (close - lag(open, 21)) / lag(open, 21),
      RETURN_1W = (close - lag(open, 5)) / lag(open, 5),
      SMA20_MIN_SMA50 = SMA(close, n = 20) - SMA(close, n = 50),
      SMA100_MIN_SMA200 = SMA(close, n = 100) - SMA(close, n = 200),
      date = format(date, "%Y-%m-%d")
    )
  
  # Splits
  finance_data <- finance_data %>%
    group_by(ticker) %>%
    arrange(desc(date)) %>%
    mutate(stock_ratio = ifelse(split == 0, 1, 1 / split)) %>%
    mutate(stock_ratio = cumprod(stock_ratio)) %>%
    arrange(ticker, date)
  
  # Adding returns and volatility
  # Set the rolling window size
  window_size <- 21
  
  finance_data <- finance_data %>%
    group_by(ticker) %>%
    arrange(date) %>% # Ensure that the data is sorted by date within each group
    mutate(
      # Calculate returns based on adjusted prices
      log_daily_return = log(close / lag(close)),
      # Calculate rolling volatility using the 'rollapply' function from the 'zoo' package
      rolling_volatility = rollapply(log_daily_return, width = window_size, FUN = sd, by.column = TRUE, fill = NA, align = 'right', na.rm = TRUE),
      # Annualize the rolling volatility
      annualized_rolling_volatility = rolling_volatility * sqrt(252)
    ) %>% 
    arrange(ticker, date) %>% 
    ungroup()
  
  # Use tq_get() to download the data
  vix_data <- tq_get("^VIX",
                     from = "2018-01-01",
                     to = "2023-09-04",
                     get = "stock.prices") %>% 
    select(date, close) %>% 
    rename(VIX = "close")
  
  finance_data <- finance_data %>% 
    mutate(date = as.Date(date)) %>% 
    left_join(vix_data, by = "date") %>% 
    filter(date > as.POSIXct('2018-01-01'))
  
}


df <- finance_data
# As the data is missing days where the market is not open, we choose to fill the missing
# dates with the previously known market data, which most of the time is the last weekday following
# the weekend or holiday

fill_na_columns <- function(df) {
  # First we create a dataframe which contains all dates for each company 
  all_dates <- data.frame(date = seq(min(ymd(df$date)), max(ymd(df$date)), by = "day")) %>% 
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d"))
  all_tickers <- unique(df$ticker)
  
  expanded_df <- expand_grid(date = all_dates$date, ticker = all_tickers) %>% 
    arrange(ticker, date) %>% 
    mutate(date = format(date, "%Y-%m-%d"),
           date = as.Date(date))
  
  # Join the expanded frame with the original dataframe
  df_merged <- left_join(expanded_df, df, by = c("ticker", "date"))
  
  # List of columns to fill
  columns_to_fill <- setdiff(names(df_merged), c("date", "ticker"))
  
  return(
    df_merged %>%
      arrange(ticker, date) %>%
      mutate(date = ymd(date)) %>% 
      # Fill missing dates per ticker
      group_by(ticker) %>%
      complete(date = seq(min(ymd(date)), max(ymd(date)), by = "1 day")) %>%
      # Fill NA values per ticker and column
      group_modify(~ .x %>%
                     tidyr::fill(all_of(columns_to_fill), .direction = "down")) %>%
      ungroup() %>% 
      filter(!is.na(open))
  )
}

finance_data <- fill_na_columns(finance_data)

# ---- Time on OSE ----
ipo <- read_delim("../dict/ticker-companies.csv", delim = ";", show_col_types = FALSE) %>%
  mutate(ipo = as.Date(ipo, format="%d.%m.%Y")) %>% 
  select(-company)


# Left_joining the ipo time to the data frame
finance_data <- finance_data %>%
  left_join(ipo, by = "ticker") %>% 
  mutate(time_on_OSE = time_length(difftime(date, ipo), "years"),
         ln_time_on_OSE = log(time_on_OSE))

# ---- Bloomberg Data ----
# Define the locale with comma as the decimal mark and space as the group mark
my_locale <- locale(decimal_mark = ",")

# Get the list of Bloomberg files
bloomberg_files <- list.files("../data/bloomberg", full.names = TRUE)
bloomberg_files <- bloomberg_files[!bloomberg_files %in% c("../data/bloomberg/desktop.ini")]

# Function to process each file
process_file <- function(file_path) {
  read.csv(file_path, sep = ";") %>% 
    as_tibble() %>% 
    rename(market_cap = 'market_cap_nok_thousand',
           shares_outstanding = "shares_outstanding_thousand") %>% 
    select(-date2, -date3) %>% 
    mutate(across(-c(financial_leverage), ~na_if(.x, "#N/A N/A"))) %>% 
    mutate(
      date = as.Date(date, format = "%d.%m.%Y"),
      market_cap = gsub(" ", "", gsub(",", ".", market_cap)),
      market_cap = ifelse(grepl("^[0-9.]+$", market_cap), as.numeric(market_cap)*1000000, NA_real_),
      PX_LAST = as.numeric(gsub(" ", "", gsub(",", ".", PX_LAST))),
      shares_outstanding = as.numeric(gsub(" ", "", gsub(",", ".", shares_outstanding)))*1000,
      pe_ratio = as.numeric(gsub(" ", "", gsub(",", ".", pe_ratio))),
      financial_leverage = as.numeric(gsub(" ", "", gsub(",", ".", financial_leverage)))
    ) %>% 
    filter(!is.na(PX_LAST)) %>% 
    filter(!is.na(market_cap)) %>% 
    mutate(ticker = paste0(ticker, ".OL"))
}

# Combine all data frames into one
bloomberg_data <- bind_rows(lapply(bloomberg_files, process_file)) %>% 
  mutate(ticker = gsub("AKERBP.OL", "AKRBP.OL", ticker))


# Append Bloomberg data to finance_df
finance_df <- finance_data %>% 
  left_join(bloomberg_data, by = c("ticker", "date"))


# ---- Fixing Dividends ----
# Dividends
finance_df <- finance_df %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(dividend_30d = rollapply(dividend, width = 30, FUN = sum, partial = TRUE, align = 'right', fill = 0)) %>%
  # Replace dividend_30d values greater than 1 with 1, since we only care if at least one dividend was paid
  mutate(dividend_paid_last_30d = ifelse(dividend_30d > 0, 1, 0)) %>%
  # Remove the temporary column
  select(-dividend_30d) %>% 
  ungroup()


# ---- Save the finance_df to an .Rdata file ----
save(finance_df, file = "../r-data/finance-data.Rdata")

# ---- Clear Environment ----
#rm(list = ls())
