library(tidyverse)
library(reshape2)
library(lubridate)
library(ggthemes)


df <- read.csv("charge_chart.csv")

colnames(df) <- 0:23

df <- df %>%  
  rowid_to_column("day") %>% 
  gather("hour", "value", -day) %>% 
  arrange(day) %>% 
  mutate(date = as_date("2022-07-01 00:00:00")+(day-1)+hours(hour),
         sunday = ifelse(wday(date)==1,T,F)) %>% 
  mutate(week = isoweek(date))



df %>% 
  ggplot(aes(date, value, colour = sunday)) +
  geom_jitter() +
  theme_few() +
  ylab("Charged [kWh]") +
  xlab("Date") +
  ggtitle("Optimal solution (with jitter)") +
  theme(plot.title = element_text(hjust = 0.5))

df %>% 
  group_by(day) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(day, value)) +
  geom_col(fill = "#619CFF") +
  theme_few() +
  ylab("Charged [kWh]") +
  xlab("Date") +
  ggtitle("Optimal solution") +
  theme(plot.title = element_text(hjust = 0.5))



  
