# Analysis of the Financial and Textual Data

# Loading Libraries
library(tidyverse)
library(purrr)
library(caret)
library(modelr)
library(MLmetrics)
library(xgboost)
library(xtable)


# ---- Updating Files ----
# source("pdf-scraper.r")
# source("momentum-analysis.r")
# source("sentiment-analysis.r")

# ---- Color Scale ----
banfiethe <- c("#4682B4","#00008B", "#6A5ACD", "#191970")


# ---- Load Data ----
load("../r-data/main_df.Rdata")


# ---- Defining Model Data Frame ----

# As the model will not use all of the data from the main_df data frame,
# it is useful to define the data frame used for the models as model_df

{
  model_df <- main_df %>% 
    group_by(analyst, company) %>% 
    
    select(
      analyst,
      company,
      ticker,
      date,
      recommendation,
      target_price_adj,
      sentiment,
      ADX,
      adjusted,
      MACD_Indication,
      Signal,
      RSI,
      #RETURN_1Y, RETURN_6M, RETURN_1M
      RETURN_3M, RETURN_1W,
      #SMA20_MIN_SMA50, SMA100_MIN_SMA200,
      time_on_OSE,
      pe_ratio,
      financial_leverage,
      market_cap,
      dividend_paid_last_30d,
      rolling_volatility,
      annualized_rolling_volatility,
      VIX
    ) %>% 
    
    mutate(
      target_ratio = (target_price_adj/adjusted)-1,
      
      # Calculating adjusted SMA
      # SMA20_50_ADJ = SMA20_MIN_SMA50/adjusted,
      # SMA100_200_ADJ = SMA100_MIN_SMA200/adjusted,
      
      # Lagged Target Price
      target_price_1lag = lag(target_price_adj),
      target_ratio_1lag = lag(target_ratio),
      
      # From these lagged indicators we can compute relative change from t-1 to t
      target_price_change_percent = (target_price_adj - target_price_1lag)/target_price_1lag,
      
      # Sentiment Lag
      sentiment_1lag = lag(sentiment),
      # From these lagged indicators we can compute relative change
      sentiment_1lag_change = (sentiment - sentiment_1lag),
      
      # Factorizing charachter values
      analyst = factor(analyst),
      company = factor(company),
      recommendation = factor(recommendation),
      
      # Log-transforming
      log_pe_ratio = log(pe_ratio),
      log_market_cap = log(market_cap),
      log_time_on_OSE = log(time_on_OSE)
    ) %>% 
    
    # We filter out NA values for target_ratio
    filter(!is.na(target_ratio)) %>% 
    ungroup()
  
  # ---- Creating Test/Train Dataset
  colSums(is.na(model_df))
  
  model_df <- model_df %>% 
    #select(analyst, sentiment, sentiment_1lag, market_cap_nok_thousand, time_on_OSE, dividend_paid_last_30d, pe_ratio, financial_leverage,
    #       annualized_rolling_volatility, RETURN_1W, RETURN_3M, RETURN_6M, MACD_Indication, ADX, RSI) %>% 
    na.omit() # Around 70 observations is removed from the dataset
  
  save(model_df, file = "../r-data/model_df.Rdata")
}

# Printing the number of observations
paste0("There are ", nrow(model_df), " observations in the model_df dataset")

# Creating the final dataset
corr_df <- model_df %>% 
  select(
    sentiment, target_ratio, target_ratio_1lag, log_market_cap, log_time_on_OSE, dividend_paid_last_30d, log_pe_ratio, 
    financial_leverage, annualized_rolling_volatility, sentiment_1lag, RSI, RETURN_1W, RETURN_3M, MACD_Indication,
    target_price_change_percent, VIX
  ) %>% 
  as_tibble()

# Correlation Table
mcor<-round(cor(corr_df),2)

library(metan)

corr_df %>% 
  corr_coef() %>% 
  plot()



# Linearity Assumption
long_model_df <- model_df %>%
  pivot_longer(cols = c(RETURN_1W, RETURN_3M, MACD_Indication, RSI),
               names_to = "variable", values_to = "value")

p_linear <- ggplot(long_model_df, aes(x = value, y = sentiment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free") +
  theme_classic() +
  labs(title = "Sentiment Analysis Across Various Metrics",
       x = "Metric",
       y = "Sentiment")



# ---- OLS Regression ----

# Simple OLS regression for sentiment to change in target pricing
model_df %>% 
  lm(sentiment ~ target_ratio + target_ratio_1lag + target_price_change_percent, data = .) %>% 
  summary()


# Several OLS regressions

# Define a list of formulas for regression
formulas <- list(
  full = sentiment ~ target_ratio + target_ratio_1lag + target_price_change_percent,
  tr = sentiment ~ target_ratio,
  tr1 = sentiment ~ target_ratio_1lag,
  tpc = sentiment ~ target_price_change_percent
)

# Use purrr::map() to apply lm() function to each formula
models <- map(formulas, ~lm(.x, data = model_df))

stargazer::stargazer(models$full, 
                     models$tr,
                     models$tr1,
                     models$tpc,
                     # Formatting
                     #type = "text",
                     keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic"), 
                     title = "Sentiment Against Target Price Metrics",
                     column.labels = c("Full Model", "Target Ratio", "Target Ratio Lag", "% Change in Target Price"))

# Old OLS ----
# Results show that the sentiment within the text of a given report is most
# useful when trying to explain variance in relative target price change


# Simple OLS for showing relationship between market trends and sentiment
# Model Forms ----
full_form <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag +

  # Trend Variables
  RETURN_1W + RETURN_3M + MACD_Indication + RSI

form1 <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag +
  
  # Trend Variables
  RETURN_1W

form2 <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag +
  
  # Trend Variables
  RETURN_3M

form3 <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag +
  
  # Trend Variables
  MACD_Indication

form4 <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag +
  
  # Trend Variables
  RSI

control_form <- sentiment ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + sentiment_1lag


target_ratio_form <- target_ratio ~
  # Control Variables
  log(market_cap) + log(time_on_OSE) + analyst +
  dividend_paid_last_30d + log(pe_ratio) + financial_leverage + annualized_rolling_volatility + VIX + target_ratio_1lag +
  
  # Trend Variables
  RETURN_1W + RETURN_3M + MACD_Indication + RSI

# Trend Models
sentiment_formFull <- sentiment ~ RETURN_1W + RETURN_3M + MACD_Indication + RSI
sentiment_formR1W <- sentiment ~ RETURN_1W
sentiment_formR3M <- sentiment ~ RETURN_3M
sentiment_formMACD <- sentiment ~ MACD_Indication
sentiment_formRSI <- sentiment ~ RSI



# Mulitple OLS models ----

# Define a list of formulas for regression
forms_control <- list(
  full = full_form, 
  r1w = form1, 
  r3m = form2, 
  MACD = form3, 
  RSI = form4,
  trf = target_ratio_form
)

forms_sentiment <- list(
  sentFull <- sentiment_formFull,
  sentR1W <- sentiment_formR1W,
  sentR3M <- sentiment_formR3M,
  sentMACD <- sentiment_formMACD,
  sentRSI <- sentiment_formRSI
)

# Sentiment with control variables
# Use purrr::map() to apply lm() function to each formula
full.models <- map(forms_control, ~lm(.x, data = model_df))

stargazer::stargazer(full.models$full,
                     full.models$r1w,
                     full.models$r3m,
                     full.models$MACD,
                     full.models$RSI,
                     full.models$trf,
                     # Formatting
                     type = "text",
                     keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic"), 
                     title = "Regression of Analyst Indicators",
                     column.labels = c("Full Model", "Return 1W", "Return 3M", "MACD", "RSI", "Full Model (TR)"))

# Sentiment without control variables
# Use purrr::map() to apply lm() function to each formula
sent.models <- map(forms_sentiment, ~lm(.x, data = model_df))

stargazer::stargazer(sent.models[[1]],
                     sent.models[[2]],
                     sent.models[[3]],
                     sent.models[[4]],
                     sent.models[[5]],
                     # Formatting
                     type = "text",
                     keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic"), 
                     column.labels = c("Full Model", "Return 1W", "Return 3M", "MACD", "RSI"),
                     title = "Regression of Sentiment against Market Trend Indicators")

# Plotting residuals vs fitted
full.models.plot <- list(
  "Full Model (1)" = full.models$full,
  "Return 1W (2)" = full.models$r1w,
  "Return 3M (3)" = full.models$r3m,
  "MACD (4)" = full.models$MACD,
  "RSI (5)" = full.models$RSI,
  trf = full.models$trf
)

plot_data <- map_df(names(full.models.plot), function(model_name) {
  model <- full.models.plot[[model_name]]
  data.frame(
    Model = model_name,
    Fitted = fitted.values(model),
    Residuals = resid(model)
  )
}) %>% 
  filter(Model != "trf")

# Create the ggplot
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  facet_wrap(~Model, scales = "free") +
  theme_classic() +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Values for Multiple Models")

# Hetroskediasitet
library(lmtest)
# Apply Breusch-Pagan test
bp_full = bptest(full.models$full)
bp_r1w = bptest(full.models$r1w)
bp_r3m = bptest(full.models$r3m)
bp_MACD = bptest(full.models$MACD)
bp_RSI = bptest(full.models$RSI)
bp_trf = bptest(full.models$trf)

# You can view the results for each test
bp_full
bp_r1w
bp_r3m
bp_MACD
bp_RSI
bp_trf



# LOOCV Caret OLS ----
# Setting up Leave-One-Out Cross-Validation
train_control_ols <- trainControl(method = "LOOCV")

# Train the model using LOOCV
set.seed(123) # for reproducibility
loocv.ols.model <- train(full_form, data = model_df, method = "lm", trControl = train_control_ols)

# Print the results
print(loocv.ols.model)

loocv.ols.model$results
summary(loocv.ols.model)


loocv.models <- map(forms_control, ~train(.x, data = model_df, method = "lm", trControl = train_control_ols))

loocv.models$full$results$RMSE

model_names <- c("full", "r1w", "r3m", "MACD", "RSI")
rmse_df <- lapply(model_names, function(model) {
  # Replace loocv.models with your actual list variable
  loocv.models[[model]]$results$RMSE
}) %>% 
  data.frame()

# Rename the columns to reflect the model names
names(rmse_df) <- model_names

RMSE.df.ols <- rmse_df %>% 
  mutate(metric = "RMSE",
         model = "OLS")





# Adjusting for linearity, independence, homoscedasticity, normality of residuals, and multicollinearity  ----
# 1. Linearity
# Observed vs Predicted
plot(test_predictions, test_df$sentiment,
     main = "Observed vs. Predicted",
     xlab = "Predicted values",
     ylab = "Observed values")
abline(0, 1)

# Residuals vs Predicted
residuals <- test_df$sentiment - test_predictions
plot(test_predictions, residuals,
     main = "Residuals vs. Predicted",
     xlab = "Predicted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
# Here there needs to be no trends

# 2. Independence
library(lmtest)
dwtest(ols_model)

# 3. Homoscedasticity
# Residuals vs Fitted
plot(ols_model$fitted.values, residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Breusch-Pagan test
bptest(ols_model)

# 4. Normality of Residuals
# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")

# Shapiro-Wilk test
shapiro.test(residuals)

# 5. No Multicollinearity
library(car)
vif(ols_model)  # VIF values greater than 5 or 10 indicate high multicollinearity


# XGBoost K-fold ----
xbg.df <- model_df %>% 
  mutate(analyst = as.numeric(as.factor(analyst)),
         log_market_cap = log(market_cap),
         log_time_on_OSE = log(time_on_OSE),
         log_pe_ratio = log(pe_ratio))


# Expanded grid for hyperparameter tuning
extended_grid_xgb <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 5, 7, 9),
  eta = c(0.01, 0.05, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.5, 0.7, 0.9),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5, 0.7, 0.9)
)

# Set up cross-validation
cv <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = defaultSummary,
  seeds = set.seed(123),
  search = "grid"  # Use grid search for tuning
)

# Train the model
# set.seed(123)
# xgb_tuned <- train(
#   full_form,
#   data = xbg.df,
#   method = "xgbTree",
#   tuneGrid = extended_grid_xgb,
#   trControl = cv,
#   metric = "RMSE",
#   verbosity = 0
# )

# saveRDS(xgb_tuned, file = "../r-data/xgb_model.rds")
xbg.model <- readRDS(file = "../r-data/xgb_model.rds")

# Display best tuning parameters
print(xbg.model$bestTune)

# Variable Importance
importance.full <- varImp(xbg.model)
importance.full

# Display the RMSE of the best model
best_rmse <- min(xbg.model$results$RMSE)
print(best_rmse)

# -------------------------------
# Tuned Model Parameters
best_params <- list(
  nrounds = 200,
  max_depth = 7,
  eta = 0.05,
  gamma = 0.2,
  colsample_bytree = 0.5,
  min_child_weight = 3,
  subsample = 0.7
)
# -------------------------------

{
  xbg.models <- map(forms_control, ~train(.x, 
                                          data = xbg.df, 
                                          method = "xgbTree", 
                                          tuneGrid = expand.grid(best_params),
                                          trControl = cv,
                                          metric = "RMSE",
                                          verbosity = 0)
  )
  
  model_names <- c("full", "r1w", "r3m", "MACD", "RSI")
  rmse_df <- lapply(model_names, function(model) {
    # Replace loocv.models with your actual list variable
    xbg.models[[model]]$results$RMSE
  }) %>% 
    data.frame()
  
  # Rename the columns to reflect the model names
  names(rmse_df) <- model_names
  
  RMSE.df.xgb <- rmse_df %>% 
    mutate(metric = "RMSE",
           model = "XGBoost")
}


RMSE.df <- rbind(RMSE.df.ols, RMSE.df.xgb) %>% 
  rename("Full Model" = "full",
         "Return 1W" = "r1w",
         "Return 3M" = "r3m",
         "Model" = "model",
         "Metric" = "metric") %>% 
  select(Metric, Model, 'Full Model', 'Return 1W', 'Return 3M', 'MACD', 'RSI')

xtable(RMSE.df, digits=c(0,0,0,4,4,4,4,4))


# Variable Importance Plot
base.xgb.model <- train(control_form, 
                        data = xbg.df, 
                        method = "xgbTree", 
                        tuneGrid = expand.grid(best_params),
                        trControl = cv,
                        metric = "RMSE",
                        verbosity = 0)

importance.control <- varImp(base.xgb.model)
importance.full
importance.control

# Convert the lists to data frames
importance.full_df <- data.frame(Variable = rownames(importance.full$importance), Importance = importance.full$importance)
rownames(importance.full_df) <- 1:nrow(importance.full_df)


importance.control_df <- data.frame(Variable = rownames(importance.control$importance), Importance = importance.control$importance)
rownames(importance.control_df) <- 1:nrow(importance.control_df)


# Add a column to indicate the source of the importance measure
importance.full_df$Source <- "Full Model"
importance.control_df$Source <- "Control Model"

# Combine the data frames
combined_df <- rbind(importance.full_df, importance.control_df) %>% 
  rename(Importance = "Overall")

missing_variables <- setdiff(combined_df[combined_df$Source == "Full Model", "Variable"], combined_df[combined_df$Source == "Control Model", "Variable"])
bind_rows(
  combined_df,
  data.frame(
    Variable = missing_variables,
    Importance = rep(0, length(missing_variables)),
    Source = rep("Control Model", length(missing_variables))
  )
) -> combined_df_plot

p <- ggplot(combined_df_plot, aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Variable Importance Comparison",
       subtitle = "",
       x = "Importance",
       y = "Variable") +
  scale_fill_manual(values = banfiethe) +
  theme_minimal()
p

ggsave(file = "../output/variable_imp.png", plot = p, units = "px", width = 3000, height = 1500)





# ---- GAM Model ----
library(gam)
gam=gam(sentiment~analyst+log(market_cap)+log(time_on_OSE)+s(pe_ratio)+financial_leverage+annualized_rolling_volatility+
          sentiment_1lag + RSI + RETURN_1W + RETURN_3M + MACD_Indication + VIX,data=train_df)
par(mfrow =c(4,3))
plot(gam,se=TRUE,col="red")

# ---- LASSO ----
# Load the glmnet package
library(glmnet)

# Since glmnet requires a matrix for x, we create it from the training data
# We also need to scale the data since LASSO is sensitive to the scale of the input variables
x_train <- model.matrix(base_form, data = train_df)[,-1] # remove intercept
y_train <- train_df$sentiment

# Fit the LASSO model
# alpha = 1 indicates LASSO regression (L1 penalty)
lasso_model <- glmnet(x_train, y_train, alpha = 1)

# To select a lambda value, cross-validation is typically used
set.seed(122) # for reproducibility
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

# Plot the cross-validation curve to see which lambda is best
plot(cv_lasso)

# Best lambda value according to cross-validation
best_lambda <- cv_lasso$lambda.min
cat("Best lambda for LASSO:", best_lambda, "\n")

# Refit the LASSO model using the best lambda
lasso_model_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Prepare the test data
x_test <- model.matrix(base_form, data = test_df)[,-1] # remove intercept
y_test <- test_df$sentiment

# Make predictions on the test set
lasso_predictions <- predict(lasso_model_best, s = best_lambda, newx = x_test)

# Calculate the MSE for the test set
testMSElasso <- mean((y_test - lasso_predictions)^2)
print(paste("Test MSE for LASSO:", testMSElasso))


# Testbed
model_df %>% 
  ggplot(aes(x = sentiment, y = financial_leverage)) +
  geom_point()

# End ----
# Removing the main_df in order to reduce confusion
rm(main_df)
save(model_df, file = "../r-data/model_df.Rdata")
