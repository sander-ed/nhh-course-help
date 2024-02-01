library(pdftools)
library(tidyverse)
library(mark)
library(stringr)
library(tictoc)
library(furrr)
library(lubridate)


# ---- Fix Language Formatting ----
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

# ---- Set stock dictionary ----
tickers <- read_delim("../dict/ticker-companies.csv", delim = ";", show_col_types = FALSE)


# ---- Functions ----

# Extracting the path to each pdf document
extract_paths <- function(i) {
  
  # Defining the main data frame
  df_pdf <- tibble(
    analyst = character(0),
    company = character(0),
    path = character(0),
  )
  
  # Storing which company is covered by this analyst in a vector
  covered_companies <- list.files(path = paste0("../data/analyst-data/", i))
  covered_companies <- covered_companies[!covered_companies %in% c("desktop.ini")]
  
  # A function which extracts all the pdf data from a given company in this list
  for(comp in covered_companies){
    docs <- list.files(path = paste0("../data/analyst-data/", i, "/", comp))
    docs <- docs[!docs %in% c("desktop.ini")]
    
    # Extracting data from a given path
    for(report in docs){
      path_raw <- paste0("../data/analyst-data/", i, "/", comp, "/", report)
      
      # Creating a temporary tibble for the current iteration
      temp_df <- tibble(
        analyst = i,
        company = comp,
        path = path_raw,
      )
      
      # Appending the temporary tibble to the main df_pdf tibble
      df_pdf <- bind_rows(df_pdf, temp_df)
    }
  }
  return(df_pdf)
}

# Basic formatting functions
nok_to_float <- function(currency_string) {
  # Remove all non-numeric characters (except for the decimal point)
  numeric_string <- gsub("[^0-9.]", "", currency_string)
  
  # Convert the string to a float
  as.numeric(numeric_string)
}

# ---- PDF EXTRACTION ----
# Extracting PDF content from the files
extract_pdf_carnegie <- function(file_path){
  # Extracting raw pdf content
  raw_pdf <- pdf_text(file_path) %>% 
    readr::read_lines() %>% 
    gsub("\\s+", " ", .)
  # Removing all empty strings
  pdf <- raw_pdf[!grepl("^\\s*$", raw_pdf)]
  
  # Extract report date CARNEGIE
  {
    dates <- str_extract_all(pdf[1:10], "\\d{1,2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \\d{4}")
    dates_vector <- unlist(sapply(dates, function(x) if (length(x) > 0) x else NULL))
  }
  
  # Extract report date CARNEGIE
  date_freqs <- table(dates_vector)
  
  # If the highest frequency is 1 (i.e., all dates are unique), then get the latest date
  # Otherwise, get the date with the highest frequency
  if (max(date_freqs) == 1) {
    report_date <- max(as.Date(format(dmy(dates_vector), "%Y-%m-%d"))) %>% 
      format("%Y-%m-%d")
  } else {
    report_date <- names(date_freqs)[which.max(date_freqs)] %>% 
      dmy() %>% 
      format("%Y-%m-%d")
  }
  
  # Finding line numbers for specific phrases, case-insensitive and exact match
  legal_statement_lines <- grep("^Legal statement$", trimws(pdf), ignore.case = TRUE)
  disclosures_lines <- grep("^Disclosures and disclaimers$", trimws(pdf), ignore.case = TRUE)
  
  # Handle cases where either phrase might not be present
  if (length(legal_statement_lines) == 0 && length(disclosures_lines) == 0) {
    # Neither phrase was found; do nothing or handle the case as needed
    warning("Neither 'Legal statement' nor 'Disclosures and disclaimers' were found in the document.")
  } else {
    # At least one phrase was found; proceed to remove lines after the first occurrence
    lines_to_remove <- c(legal_statement_lines, disclosures_lines)
    lines_to_remove <- lines_to_remove[lines_to_remove > 0]  # Filter out any non-positive numbers
    first_line_to_remove <- ifelse(length(lines_to_remove) > 0, min(lines_to_remove), length(pdf))
    
    # Removing the lines after the first occurrence of either statement
    pdf <- pdf[1:(first_line_to_remove - 1)]
  }
  
  # Extract the new target price
  target_price <- str_extract(pdf, "(?i)(?<=TP: |New target price: |Target price: |Nav:|NAV: |Nav: )(NOK)?[0-9.,]+") %>% 
    na.omit()
  target_price <- target_price[1] %>% 
    nok_to_float()
  
  # Extract the share price CARNEGIE
  share_price <- str_extract(pdf, "(?i)(?<=Share Price: )NOK[0-9.]+") %>% 
    na.omit()
  share_price <- share_price[1] %>% 
    nok_to_float()
  
  
  # Extracting recommendation
  recommendation <- str_extract(pdf, "BUY|SELL|HOLD")
  recommendation_most <- names(table(recommendation))[which.max(table(recommendation))]
  
  # Collapsing the text into one string
  pdf_str <- paste(pdf, collapse = " ")
  
  # Combining all extracted values
  output_df <- tibble(
    path = file_path,
    date = report_date,
    share_price = share_price,
    target_price = target_price,
    recommendation = recommendation_most,
    pdf_text = pdf_str
  )
  return(output_df)
}


# Extract PDF DNB-Markets
extract_pdf_dnb <- function(file_path){
  # Extracting raw pdf content
  raw_pdf <- pdf_text(file_path) %>% 
    readr::read_lines() %>% 
    gsub("\\s+", " ", .)
  # Removing all empty strings
  pdf <- raw_pdf[!grepl("^\\s*$", raw_pdf)]
  
  # Extract report date CARNEGIE
  {
    dates <- str_extract_all(pdf[1:20], "\\d{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}")
    dates_vector <- unlist(sapply(dates, function(x) if (length(x) > 0) x else NULL))
  }
  
  # Finding the date which is metnioned the most
  date_freqs <- table(dates_vector)
  
  # If the highest frequency is 1 (i.e., all dates are unique), then get the latest date
  # Otherwise, get the date with the highest frequency
  if (max(date_freqs) == 1) {
    report_date <- max(as.Date(format(dmy(dates_vector), "%Y-%m-%d"))) %>% 
      format("%Y-%m-%d")
  } else {
    report_date <- names(date_freqs)[which.max(date_freqs)] %>% 
      dmy() %>% 
      format("%Y-%m-%d")
  }
  
  # Finding line numbers for specific phrases, case-insensitive
  legal_statement_lines <- grep("Legal statement", trimws(pdf), ignore.case = TRUE)
  disclosures_lines <- grep("Disclosures and disclaimers", trimws(pdf), ignore.case = TRUE)
  
  # Handle cases where either phrase might not be present
  if (length(legal_statement_lines) == 0 && length(disclosures_lines) == 0) {
    warning("Neither 'Legal statement' nor 'Disclosures and disclaimers' were found in the document.")
  } else {
    lines_to_remove <- union(legal_statement_lines, disclosures_lines)
    first_line_to_remove <- ifelse(length(lines_to_remove) > 0, min(lines_to_remove), length(pdf))
    
    # Removing the lines after the first occurrence of either statement
    pdf <- pdf[1:(first_line_to_remove - 1)]
  }
  
  # Extract the new target price
  target_price <- str_extract(pdf, "(?i)(?<=TP: |New target price: |Target price: )NOK[0-9.,]+") %>% 
    na.omit()
  target_price <- target_price[1] %>% 
    nok_to_float()
  
  # Extract Share price
  share_price_text <- str_extract(pdf, "(?i)Share price \\(NOK\\) [0-9.,]+") %>% 
    na.omit()
  share_price <- nok_to_float(share_price_text)
  share_price <- share_price[1]
  
  # If the share price is NA
  if(is.na(share_price)){
    pdf_shareprice_filter <- str_detect(pdf, "Share price|[0-9.,]")
    pdf_filtered <- pdf[pdf_shareprice_filter]
    
    # Identify the index/position of the string containing "Share price"
    index_price <- which(str_detect(pdf_filtered, "Share price")) + 1
    index_price <- index_price[1]
    
    # Extracting the price value
    if (any(index_price <= length(pdf_filtered))) {
      # Extract numeric values and convert to numeric type
      share_price <- as.numeric(str_extract(pdf_filtered[index_price], "\\b[0-9.,]+\\b"))
    } else {
      # If the "Share price" line is the last line and there's no line below it to extract price
      share_price <- NA
    }
  }
  
  # Extracting recommendation
  recommendation <- str_extract(pdf, "BUY|SELL|HOLD")
  recommendation_most <- names(table(recommendation))[which.max(table(recommendation))]
  
  # Collapsing the text into one string
  pdf_str <- paste(pdf, collapse = " ")
  
  # Combining all extracted values
  output_df <- tibble(
    path = file_path,
    date = report_date,
    share_price = share_price,
    target_price = target_price,
    recommendation = recommendation_most,
    pdf_text = pdf_str
  )
  return(output_df)
}

# Extract PDF Pareto
extract_pdf_pareto <- function(file_path){
  # Extracting raw pdf content
  raw_pdf <- pdf_text(file_path) %>% 
    readr::read_lines() %>% 
    gsub("\\s+", " ", .)
  # Removing all empty strings
  pdf <- raw_pdf[!grepl("^\\s*$", raw_pdf)]
  
  # Removing disclosures and disclaimers
  line_numbers <- grep("Disclaimer and legal disclosures", pdf)
  if(identical(line_numbers, integer(0))){line_numbers <- length(pdf)}
  pdf <- pdf[1:line_numbers-1]
  
  # Extract report date CARNEGIE
  {
    date_string <- "\\d{1,2} (?i)(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \\d{4}"
    dates <- str_extract_all(pdf[1:20], date_string)
    dates_vector <- unlist(sapply(dates, function(x) if (length(x) > 0) x else NULL))
  }
  
  # Finding the date which is metnioned the most
  date_freqs <- table(dates_vector)
  
  # If the highest frequency is 1 (i.e., all dates are unique), then get the latest date
  # Otherwise, get the date with the highest frequency
  if (max(date_freqs) == 1) {
    report_date <- max(as.Date(format(dmy(dates_vector), "%Y-%m-%d"))) %>% 
      format("%Y-%m-%d")
  } else {
    report_date <- names(date_freqs)[which.max(date_freqs)] %>% 
      dmy() %>% 
      format("%Y-%m-%d")
  }
  
  # Extract the target price
  target_price_text <- str_extract(pdf, "(?i)Target price \\(NOK\\) [0-9.,]+") %>% 
    na.omit()
  # Extract the numeric part of the matched string
  target_price <- str_extract(target_price_text, "[0-9.,]+") %>% 
    as.numeric()
  
  # Extract the target price
  share_price_text <- str_extract(pdf, "(?i)Share price \\(NOK\\) [0-9.,]+") %>% 
    na.omit()
  # Extract the numeric part of the matched string
  share_price <- str_extract(share_price_text, "[0-9.,]+") %>% 
    as.numeric()
  
  # Extracting recommendation
  recommendation <- str_extract(pdf, "(?i)(BUY|SELL|HOLD)")
  recommendation_most <- names(table(recommendation))[which.max(table(recommendation))]
  
  # Collapsing the text into one string
  pdf_str <- paste(pdf, collapse = " ")
  
  # Combining all extracted values
  output_df <- tibble(
    path = file_path,
    date = report_date,
    share_price = share_price,
    target_price = target_price,
    recommendation = recommendation_most,
    pdf_text = pdf_str
  )
  return(output_df)
}

# Extract PDF Fearnleys
# extract_pdf_fearnleys <- function(file_path){
#   # Extracting raw pdf content
#   raw_pdf <- pdf_text(file_path) %>% 
#     readr::read_lines() %>% 
#     gsub("\\s+", " ", .)
#   # Removing all empty strings
#   pdf <- raw_pdf[!grepl("^\\s*$", raw_pdf)]
#   
#   # Removing disclosures and disclaimers
#   line_numbers <- grep("DISCLOSURES AND DISCLAIMERS FOR RECOMMENDATIONS", pdf)
#   if(identical(line_numbers, integer(0))){line_numbers <- length(pdf)}
#   pdf <- pdf[1:line_numbers-1]
#   
#   # Extract report date CARNEGIE
#   {
#     date_string <- "\\d{1,2} (?i)(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \\d{4}"
#     dates <- str_extract_all(pdf, date_string)
#     dates_vector <- unlist(sapply(dates, function(x) if (length(x) > 0) x else NULL))
#   }
#   
#   # Finding the date which is metnioned the most
#   report_date <- dates_vector[1]
#   
#   # Extract the target price text
#   target_price_text <- str_extract(pdf, "(?i)TP NOK [0-9.]+") %>% 
#     na.omit()
#   
#   # Extract the numeric part of the matched string
#   target_price <- str_extract(target_price_text, "[0-9.]+") %>% 
#     as.numeric()
#   
#   return(pdf)
# }
# 
# extract_pdf_fearnleys(df_FEARNLEYS[[1, "path"]])


# Running the function
df_CARNEGIE <- extract_paths("CARNEGIE")
df_DNB <- extract_paths("DNB")
df_PARETO <- extract_paths("PARETO")
# df_FEARNLEYS <- extract_paths("FEARNLEYS")

# ---- Carnegie ----
{
  tic("map function")
  print("Running code...")
  future::plan(multisession)
  
  # Create a new dataframe by applying the function on the 'path' column
  df <- df_CARNEGIE$path %>%
    future_map(~extract_pdf_carnegie(.)) %>%
    purrr::map_df(~.)
  
  # Join the new dataframe with the original based on the 'path' column
  df_final_CARNEGIE <- df %>%
    left_join(df_CARNEGIE, by = "path")
  rm(df)
  
  df_final_CARNEGIE <- df_final_CARNEGIE %>% 
    select(analyst, company, path, date, recommendation, share_price, target_price, pdf_text)
  
  print("Done")
  toc()
}

# ---- DNB ----

{
  tic("map function")
  print("Running code...")
  future::plan(multisession)
  
  # Create a new dataframe by applying the function on the 'path' column
  df <- df_DNB$path %>%
    future_map(~extract_pdf_dnb(.)) %>%
    purrr::map_df(~.)
  
  # Join the new dataframe with the original based on the 'path' column
  df_final_DNB <- df %>%
    left_join(df_DNB, by = "path")
  rm(df)
  
  df_final_DNB <- df_final_DNB %>% 
    select(analyst, company, path, date, recommendation, share_price, target_price, pdf_text)
  
  print("Done")
  toc()
}

# ---- PARETO ----

{
  tic("map function")
  print("Running code...")
  future::plan(multisession)
  
  # Create a new dataframe by applying the function on the 'path' column
  df <- df_PARETO$path %>%
    future_map(~extract_pdf_pareto(.)) %>%
    purrr::map_df(~.)
  
  # Join the new dataframe with the original based on the 'path' column
  df_final_PARETO <- df %>%
    left_join(df_PARETO, by = "path")
  rm(df)
  
  df_final_PARETO <- df_final_PARETO %>% 
    select(analyst, company, path, date, recommendation, share_price, target_price, pdf_text)
  
  print("Done")
  toc()
}

rm(list = c("df_CARNEGIE", "df_DNB", "df_PARETO"))

# Binding the dataframes
pdf_df <- rbind(df_final_CARNEGIE, df_final_DNB, df_final_PARETO)

# Appending the tickers to the data frame
pdf_df <- pdf_df %>% 
  left_join(tickers, by = c("company"))


# Saving the combined dataframe for later use
save(pdf_df, file = "../r-data/pdf-data.Rdata")

# ---- Clear Environment ----
rm(list = ls())

load("../r-data/pdf-data.Rdata")


t <- pdf_df %>% 
  filter(analyst == "DNB", company == "DNB-BANK")

