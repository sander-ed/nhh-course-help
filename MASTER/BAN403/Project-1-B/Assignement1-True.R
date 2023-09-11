###################################
#                                 #
#           Simulation of         #
#         Business Processes      #
#               BAN403            #
#                                 #
###################################

## --------------------------------------------------------------------- ##
                      #Data simulation for task 1

library(tidyverse)
library(moments)

# Reading in the data from table 3 in the paper "Can we trust teaching evaluations
# when response rates are not high? Implications from a Monte Carlo simulation"
table3 <- read_csv("T0003-10.1080_03075079.2019.1711046.csv")

colnames(table3) = c("Subset", "Evaluation_level", "Class_level",
                     "Response_level", "Class_count", "Mean_evaluation_score",
                     "Standard_deviation", "Skewness", "Kurtosis")

# Formatting the input data to a usable format
table3$Mean_evaluation_score <- as.numeric(gsub("???", "-",table3$Mean_evaluation_score))
table3$Standard_deviation <- as.numeric(gsub("???", "-",table3$Standard_deviation))
table3$Skewness <- as.numeric(gsub("???", "-", table3$Skewness))
table3$Kurtosis <- as.numeric(gsub("???", "-",table3$Kurtosis))
#Note: if the content of the first gsub condition is "???" due to encoding, it needs to be replaced with the first character in table3$Kurtosis[1][1]

# Importing data from Figure 1 in the paper
stat <- data.frame(name = c("Class_size", "Evaluation_score", "Response_rate" ),
                   mean = c(23.5, 4.23, 0.496),
                   SD = c(15.6, 0.68, 0.227))

# Define the breaks and labels for Class_level
class_breaks <- c(4, 19, 39, 59, Inf)
class_labels <- c("Small", "Medium", "Large", "Very Large")

# Define the breaks and labels for Response_level
response_breaks <- c(0, 0.4, 0.6, 1)
response_labels <- c("Low", "Medium", "High")

# Define the breaks and labels for Evaluation_level
evaluation_breaks <- c(1, 3.49, 3.99, 4.49, 5)
evaluation_labels <- c("Low", "Medium", "High", "Very High")

Sim_size = sum(table3$Class_count)  # This variable assigns the amount of classes to be simulated
# sum(table3$Class_count) * 24 was used for the simulation

# Simulating class size and response rate
classes <- data.frame(class = pmax(pmin(round(rnorm(n = Sim_size,
                                              mean = stat[1,]$mean,
                                              sd = stat[1,]$SD)),
                                        159),5),
                      Response_rate = pmax(pmin(rnorm(n = Sim_size,
                                                            mean = stat[3,]$mean,
                                                            sd = stat[3,]$SD),0.9),0.1),
                      Responses = NA,
                      Mean_evaluation_score = NA,
                      Standard_deviation = NA,
                      Skewness = NA,
                      Kurtosis = NA)

# Defining Class_level and Response_rate within the breaks
classes$Class_level <- cut(classes$class, breaks = class_breaks, labels = class_labels)
classes$Response_level <- cut(classes$Response_rate, breaks = response_breaks, labels = response_labels)


# compiling subsets with missing data
for (subset in 1:length(table3$Subset)) {
  if (filter(table3, Subset == subset)$Class_count < 2) {
    # find neighboring subsets and combine them
    neighboring_subsets <- subset + c(-1, 0, 1)
    compile <- table3 %>% filter(Subset %in% neighboring_subsets)
    compile <- na.omit(compile)
    table3$Class_count[subset] <- sum(compile$Class_count)
    table3$Mean_evaluation_score[subset] <- mean(compile$Mean_evaluation_score)
    table3$Standard_deviation[subset] <- mean(compile$Standard_deviation)
    table3$Skewness[subset] <- mean(compile$Skewness)
    table3$Kurtosis[subset] <- mean(compile$Kurtosis)
    }

}

#################

# Simulating evaluation scores
df = data.frame()
for (n in 1:12) {
  hist_subset <- table3 %>%
    filter(Class_level == table3$Class_level[n],
           Response_level == table3$Response_level[n])
  
  sim_subset <- classes %>%
    filter(Class_level == table3$Class_level[n],
           Response_level == table3$Response_level[n])
  
  sim_subset$Subset <- sample(hist_subset$Subset, length(sim_subset$class), replace = TRUE, prob = hist_subset$Class_count/sum(hist_subset$Class_count))
  df <- rbind(df,sim_subset)
}
classes <- df

for (n in 1:length(classes$class)) {
  df <- data.frame(class = rep(n,classes$class[n]),
                   student = 1:classes$class[n])
  df$Evaluation_score <- pmax(pmin(round(rnorm(n = classes$class[n],
                                               mean = table3$Mean_evaluation_score[classes$Subset[n]],
                                               sd = table3$Standard_deviation[classes$Subset[n]])),
                                   5),1)
  df <- sample_frac(df, classes$Response_rate[n])
  classes$Responses[n] = length(df$student)
  
  classes$Mean_evaluation_score[n] = mean(df$Evaluation_score)
  classes$Standard_deviation[n] = sd(df$Evaluation_score)
  classes$Skewness[n] = skewness(df$Evaluation_score)
  classes$Kurtosis[n] = kurtosis(df$Evaluation_score)
  
}

# Categorizing the classes
classes$Evaluation_level <- cut(classes$Mean_evaluation_score, breaks = evaluation_breaks, labels = evaluation_labels)
classes <- classes[classes$Responses >= 4,]
classes <- na.omit(classes)

#checking confidence intervals
conf_intervals <- data.frame(subset=table3$Subset)
conf_intervals$max_eval <- table3$Mean_evaluation_score + qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$min_eval <- table3$Mean_evaluation_score - qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$max_sd <- table3$Standard_deviation + qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$min_sd <- table3$Standard_deviation - qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$max_skewness <- table3$Skewness + qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$min_skewness <- table3$Skewness - qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$max_kurtosis <- table3$Kurtosis + qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)
conf_intervals$min_kurtosis <- table3$Kurtosis - qnorm(0.9) * table3$Standard_deviation / sqrt(table3$Class_count)

# Testing if the classes are within the conf intervals

for (n in 1:length(classes$class)) {
  subset_class <- table3 %>%
    filter(Evaluation_level == classes$Evaluation_level[n],
           Class_level == classes$Class_level[n],
           Response_level == classes$Response_level[n])
  conf_class <- conf_intervals[conf_intervals$subset == subset_class$Subset,]
  
  classes$Eval_test[n] <- classes$Mean_evaluation_score[n] >= conf_class$min_eval & 
    classes$Mean_evaluation_score[n] <= conf_class$max_eval
  classes$SD_test[n] <- classes$Standard_deviation[n] >= conf_class$min_sd &  
    classes$Standard_deviation[n] <= conf_class$max_sd
  classes$Skew_test[n] <- classes$Skewness[n] >= conf_class$min_skewness & 
    classes$Skewness[n] <= conf_class$max_skewness
  classes$Kurt_test[n] <- classes$Kurtosis[n] >= conf_class$min_kurtosis & 
    classes$Kurtosis[n] <= conf_class$max_kurtosis
}

classes <- read.csv("Classes.csv") # Loading in the data which have ran the code above using sum(table3$Class_count) * 24 as simulation size

# Removing FALSE test values
classes <- classes %>%
  filter(Eval_test == TRUE,
         SD_test == TRUE,
         Skew_test==TRUE)


# Generating the outoput
sim_dat <- table3[,1:4]
class_summary <- classes %>%
  group_by(Evaluation_level, Class_level, Response_level) %>%
  summarize(
    Class_count = n(),
    Mean_evaluation_score = mean(Mean_evaluation_score),
    Standard_deviation = mean(Mean_evaluation_score),
    Skewness = mean(Skewness),
    Kurtosis = mean(Kurtosis)
  ) %>%
  ungroup()

sim_dat <- sim_dat %>%
  left_join(class_summary, by = c("Evaluation_level", "Class_level", "Response_level"))


# Making regression
reg <- lm(Mean_evaluation_score~Response_rate + class, classes)
summary(reg)

