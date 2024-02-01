# Loading Libraries
library(tidyverse)
library(stargazer)
library(lubridate)
library(latex2exp)
library(patchwork)
library(ggridges)

# ---- Load Data ----
load("../r-data/main_df.Rdata")
load("../r-data/model_df.Rdata")

# ---- Color Scale ----
banfiethe <- c("#ADD8E6", "#4682B4", "#0010CD", "#00008B", "#6A5ACD", "#191970")

# ---- Fix Language Formatting ----
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

# ---- Oslo Børs ----

# This is to summarise and compare the companies we have chosen for our analysis. 
# The data is gathered from Yahoo! Finance the 26.10.2023

# get_market_cap <- function(symbol){
#   # We get the Yahoo! stats
#   print(symbol)
#   url <- paste0('https://finance.yahoo.com/quote/', symbol, "?p=", symbol)
#   
#   # Use tryCatch to handle errors
#   result <- tryCatch({
#     yo <- url %>% read_html() %>% html_table() 
#     value <- yo[[2]][1, 2]
#     return(value[[1]])
#   }, error = function(e) {
#     return(NA)  # Return NA if there's an error
#   })
#   
#   return(result)
# }

# Reading in the Oslo Børs companies
# oslo <- readr::read_delim("../data/euronext/Euronext_Equities_2023-10-25.csv", delim = ";", show_col_types = FALSE) %>% 
#   as_tibble() %>% 
#   filter(!is.na(Symbol)) %>% 
#   mutate(across(c(`Open Price`, `High Price`, `low Price`, `last Price`), 
#                 ~ as.numeric(gsub(",", ".", .)))) %>% 
#   mutate(Symbol = paste0(Symbol, ".OL"))
# 
# save(oslo, file = "../r-data/oslo-bors.Rdata")
load("../r-data/oslo-bors.Rdata")

# Summary of Oslo Børs Market Cap
in_index <- tibble(
  Symbol = unique(main_df$ticker),
  in_index = 1
)

market_cap_total <- oslo %>% 
  left_join(in_index, by = c("Symbol")) %>% 
  mutate(in_index = ifelse(is.na(in_index), 0, 1)) %>% 
  group_by(in_index) %>% 
  summarise(sum_market_cap = sum(market_cap_nok))

# Index Percentage of total market cap
paste0("Index contains ", 
       round(market_cap_total[[2, "sum_market_cap"]]/(market_cap_total[[2, "sum_market_cap"]] + market_cap_total[[1, "sum_market_cap"]]), 4), 
       "% of market cap on OSE")

# ---- Selected Companies ----
library(xtable)
selected_companies <- model_df %>%
  group_by(company, ticker) %>% 
  summarise(
    data_points = n(),
    start_date = min(as.Date(date, format="%Y-%m-%d")),
    end_date = max(as.Date(date, format="%Y-%m-%d"))
  ) %>%
  ungroup()

# Assuming your tibble is named 'selected_companies'
formatted_table <- selected_companies %>%
  select(ticker, company, start_date, end_date, data_points) %>%
  mutate(
    start_date = format(start_date, "%d.%m.%Y"),
    end_date = format(end_date, "%d.%m.%Y"),
    data_points = format(data_points, big.mark = ",")
  )

# Convert to LaTeX
latex_code <- xtable(formatted_table, caption = "Selected companies", label = "tab:my_label", align = c("l", "l", "l", "l", "l", "r"))

# Print the LaTeX code
cat("\\begin{ruledtable}\n")
cat("\\centering\n")
print(latex_code, hline.after = c(-1, 0, nrow(formatted_table)), 
      include.rownames = FALSE, 
      sanitize.text.function = function(x) x)
cat("\\end{ruledtable}\n")

sum(selected_companies$data_points)

# --- Data Balance ----
table(model_df$analyst)

# ---- Data Set Summary ----
summary(model_df)
model_df %>% 
  arrange(desc(RSI)) -> test

# Negativ+positive words against total words
main_df %>% 
  select(positive, negative, word_count) %>% 
  mutate(loaded_words = positive+negative) %>% 
  ggplot(aes(x = word_count, y = loaded_words)) + 
  geom_point() + 
  geom_smooth(method = "lm")


# ---- Descriptive Analytics ----

# In order to be sure that there are aren't any inherent biases within the sentiment score, it could be
# useful to plot a distribution of all observed sentiment scores for the reports.

# Summary Statistics
model_df %>% 
  mutate(market_cap = market_cap/100000) %>% 
  select(company, ticker, date, # 
         
         # Dependant Variable
         sentiment, target_ratio,
         
         # Control Variables
         sentiment_1lag, target_ratio_1lag, analyst, market_cap, time_on_OSE, analyst, dividend_paid_last_30d, pe_ratio,
         financial_leverage, annualized_rolling_volatility, VIX,
         
         # Explanatory Variables
         RETURN_3M, MACD_Indication, RSI
  ) -> desc_df

desc_df %>% 
  select(-company, -ticker, -date) %>% 
  stargazer(as.data.frame(.))

# Sentiment Distribution

ggplot(model_df, aes(x = sentiment, fill = analyst)) +
  geom_histogram(binwidth = 0.075, color = "black") + 
  labs(x = "Sentiment", y = "Number of Observations") +
  scale_fill_manual(values = banfiethe) +
  geom_vline(xintercept = mean(model_df$sentiment), color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(model_df$sentiment), max(model_df$sentiment), by = 0.1)) +
  theme_classic()

ggplot(data = model_df, 
       aes(x = sentiment, y = analyst, fill = analyst)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  scale_fill_manual(values = banfiethe) +
  theme_classic()

# Sentiment Distribution + Density Ridges

library(ggplot2)
library(ggridges)
library(patchwork)

mean_sentiments <- model_df %>%
  group_by(analyst) %>%
  summarize(mean_sentiment = mean(sentiment))


histogram_plot <- ggplot(model_df, aes(x = sentiment, fill = analyst)) +
  geom_histogram(binwidth = 0.075, color = "black") +
  scale_fill_manual(values = banfiethe) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_vline(xintercept = mean(model_df$sentiment), color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(model_df$sentiment), max(model_df$sentiment), by = 0.1)) +
  labs(x = "Sentiment", y = "Number of Observations", fill = "Investment Bank") +
  theme_classic() +
  theme(legend.position = c(0.9, 0.7))

density_ridges_plot <- ggplot(data = model_df, aes(x = sentiment, y = analyst, fill = analyst)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  geom_vline(data = mean_sentiments, aes(xintercept = mean_sentiment, color = analyst), linetype = "dashed", size = 1) +
  scale_fill_manual(values = banfiethe) +
  scale_color_manual(values = banfiethe) + # Ensure the line colors match the fill colors
  scale_x_continuous(limits = c(-1, 1)) +
  scale_x_continuous(breaks = seq(min(model_df$sentiment), max(model_df$sentiment), by = 0.1)) +
  labs(x = "Sentiment", y = "Investment Bank") +
  theme_classic() +
  theme(legend.position = "none") # Optional: hide the legend if it's redundant


combined_plot <- (histogram_plot + density_ridges_plot) + 
  plot_layout(heights = c(2, 1))

# To display the plot
print(combined_plot)

ggsave(file = "../output/sentiment_dist.png", plot = combined_plot, units = "px", width = 3000, height = 2050)



# Average Sentiment per analyst
# Convert date to year and get average sentiment per year and analyst
yearly_sentiment <- model_df %>%
  mutate(year = year(date)) %>%
  group_by(year, analyst) %>%
  summarise(average_sentiment = mean(sentiment))

# Create the plot
ggplot(yearly_sentiment, aes(x = factor(year), y = average_sentiment, fill = analyst)) +
  geom_col(position = "dodge") +
  theme_minimal() +  # Choosing a minimal theme
  labs(x = "Year", y = "Average Sentiment", fill = "Analyst") +
  theme_classic()

# Recommendations
yearly_recommendations_by_analyst <- model_df %>%
  mutate(year = year(date)) %>%
  filter(recommendation %in% c("BUY", "HOLD", "SELL")) %>%
  count(year, analyst, recommendation)

# Create the plot with stacked bars
ggplot(yearly_recommendations_by_analyst, aes(x = as.factor(year), y = n, fill = recommendation)) +
  geom_col(position = position_dodge(preserve = "single")) +
  theme_minimal() +  # Choosing a minimal theme
  labs(x = "Year", y = "Number of Recommendations", fill = "Recommendation Type") +
  facet_wrap(~ analyst, scales = "free_x")  # Create a separate plot facet for each analyst

# Average Target Ratio
average_target_ratio <- model_df %>% 
  group_by(analyst, recommendation) %>% 
  summarise(mean_target_ratio = mean(target_ratio))

# Create the plot
ggplot(average_target_ratio, aes(x = recommendation, y = mean_target_ratio, fill = analyst)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Analyst Recommendations vs Mean Target Ratio",
       x = "Recommendation",
       y = "Mean Target Ratio") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

# Average Sentiment
average_sentiment <- model_df %>% 
  group_by(analyst, recommendation) %>% 
  summarise(mean_target_ratio = mean(sentiment))

# Create the plot
ggplot(average_sentiment, aes(x = recommendation, y = mean_target_ratio, fill = analyst)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Analyst Recommendations vs Mean Target Ratio",
       x = "Recommendation",
       y = "Mean Target Ratio") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
  

# Target Ratio over time
recc_color = c("darkgreen", "grey", "red")

model_df %>% 
  ggplot(aes(x = date, y = target_ratio, color = recommendation)) +
  geom_point() +
  facet_wrap(~analyst, scales = "free") +
  scale_color_manual(values = recc_color) +
  scale_fill_manual(values = recc_color) +
  geom_smooth(method = "lm", aes(fill = recommendation), alpha = 0.05) +
  # Labs
  labs(x = "Date", 
       y = TeX(r'(Target Ratio \rho)'),
       color= "Recommendation",
       fill = "Recommendation"
  ) +
  theme_classic() +theme(legend.position = "none") -> rec_point

model_df %>% 
  ggplot(aes(x = recommendation, fill = recommendation)) +
  geom_bar() +
  facet_wrap(~analyst) +
  scale_fill_manual(values = recc_color) +
  # Labs
  labs(x = "Recommendation", 
       y = "Number of Observations",
       fill = "Recommendation"
  ) +
  theme_classic() +
  theme(legend.position = "none") -> rec_bar

rec_point/rec_bar +
  plot_layout(heights = c(2, 1))

# Distribution of Reports
format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart], 
        year)
}

model_df %>%
  mutate(year_q = quarter(date, with_year = TRUE),
         year_q = gsub("\\.", "Q", year_q)) %>% 
  ggplot(aes(x = year_q, fill = analyst)) +
  geom_bar(stat = "count", color="black") +
  scale_fill_manual(values = banfiethe) +
  labs(
    y = "Year and Quarter",
    x = "Number of Reports",
    fill = "Analyst"
  )+
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, vjust = 0.5))

  
