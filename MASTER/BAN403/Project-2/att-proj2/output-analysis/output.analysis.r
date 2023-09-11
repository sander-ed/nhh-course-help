### Packages needed to run the file ----
library(tidyverse) # Tidyverse syntax

### Column name vectors ----
# Column names for base 2 burrito trucks
cols_base <- 
  c('Scenario', 'Replication', 'BurritoSold3', 'ServiceTime3', 'ServiceQueueServed3', 'StdService3',
    'PeopleBeenInLine3', 'ServiceQueue3', 'StdQueue3', 'BurritoSold5', 'ServiceTime5',
    'ServiceQueueServed5', 'StdService5', 'PeopleBeenInLine5', 'ServiceQueue5', 'StdQueue5',
    'Reject_b1','Reject_b2', 'Reject_b3', 'Reject_b4', 'Reject_b5', 'WorkersPerTruck')

# Column names for 3 burrito trucks
cols_three <- 
  c('Scenario', 'Replication', 'BurritoSold3', 'ServiceTime3', 'ServiceQueueServed3', 'StdService3',
    'PeopleBeenInLine3', 'ServiceQueue3', 'StdQueue3', 'BurritoSold5', 'ServiceTime5',
    'ServiceQueueServed5', 'StdService5', 'PeopleBeenInLine5', 'ServiceQueue5', 'StdQueue5', 
    'BurritoSold6', 'ServiceTime6', 'ServiceQueueServed6', 'StdService6',
    'PeopleBeenInLine6', 'ServiceQueue6', 'StdQueue6', 'Reject_b1',
    'Reject_b2', 'Reject_b3', 'Reject_b4', 'Reject_b5', 'WorkersPerTruck')


### Data importation ----
# Only two trucks
df_base1 <- read.delim('data/ban403-base-model-1.dat') %>% 
  mutate(WorkersPerTruck = 1) 
df_base2 <- read.delim('data/ban403-base-model-2.dat') %>% 
  mutate(WorkersPerTruck = 2)
df_base3 <- read.delim('data/ban403-base-model-3.dat') %>% 
  mutate(WorkersPerTruck = 3)

# Combined - two trucks
df_base <- rbind(df_base1, df_base2, df_base3)
rm(df_base1, df_base2, df_base3) # Removing unnecessary data frames

# Three truck
df_three1 <- read.delim('data/ban403-expanded-model-1.dat') %>% 
  mutate(WorkersPerTruck = 1)
df_three2 <- read.delim('data/ban403-expanded-model-2.dat') %>% 
  mutate(WorkersPerTruck = 2)

# Combined - three trucks
df_three <- rbind(df_three1, df_three2)
rm(df_three1, df_three2) # Removing unnecessary data frames

### Data Wrangling ----
# Wrangling the df_base 
df_base <- df_base %>% 
  drop_na(Replication) %>% 
  Filter(function(x)!all(is.na(x)), .) %>% 
  filter(!is.na(Replication)) %>% 
  replace(is.na(.), 0) %>% 
  set_names(cols_base)%>% 
  select(-Scenario)

# Wrangling the df_three
df_three <- df_three %>% 
  drop_na(Replication) %>% 
  Filter(function(x)!all(is.na(x)), .) %>% 
  filter(!is.na(Replication)) %>% 
  replace(is.na(.), 0) %>% 
  set_names(cols_three) %>% 
  select(-Scenario)

### Constants ----

# The number of worked minutes during each individual
# buildings brake over the simulated period, which is a period of
# 30 days. Each day they work for two hours. Lastly we convert the
# worked hours into the minutes worked. The minutes worked
# is the same amount of brake each of the given buildings have in
# brake time in this period. 
Ctime <- (30*2*60)


# The fixed cost of placing one Burrito truck
Cmove <- 1000


### Functions used for the script ----
# Function for throughput
throughput <- 
  function(item_processed){
  tp <- (item_processed/Ctime)
  return(tp)
  }

# Service rate (mu) 

mu <- 
  function(service_time){
    m <- 1/service_time
    return(m)
  }

# Function for cycle time
cycle_time <- 
  function(queue_time, service_time){
    ct <- queue_time + service_time
    return(ct)
  }

# Function for total contribution
sum_contribution <- 
  function(served){
    tr <-  served * 60
    return(tr)
  }

# Function for total variable cost
sum_variable_cost <- 
  function(served){
    svc <- served * 40
    return(svc)
  }

# Steady State function
steady_state <- function(lambda, mu){
  rho <-  lambda/mu
  return(rho)
}


# Function taking data frame from wide to long
jaam_long <- function(df){
  df %>% pivot_longer(cols = -WorkersPerTruck) %>%
    separate(name, into = c("variable", "truck"), sep = "(?<=[^0-9])(?=[0-9])") %>%
    pivot_wider(names_from = variable, values_from = value)
}

### Mutating on wanted variables to data frames ----
# df_base mutated on variables
df_base <- df_base %>% 
  mutate(throughput_truck3 = throughput(PeopleBeenInLine3),                               # Throughput
         throughput_truck5 = throughput(PeopleBeenInLine5),
         processing_truck3 = mu(ServiceTime3),                                            # Processing rate for the truck
         processing_truck5 = mu(ServiceTime5),
         steadystate_truck3 = steady_state(throughput_truck3, processing_truck3),         # Steady state
         steadystate_truck5 = steady_state(throughput_truck5, processing_truck5),
         cycletime_truck3 = cycle_time(ServiceQueueServed3, ServiceTime3),                # Cycle time
         cycletime_truck5 = cycle_time(ServiceQueueServed5, ServiceQueue5),
         revenue_truck3 = sum_contribution(BurritoSold3),                                 # Net revenue
         revenue_truck5 = sum_contribution(BurritoSold5),
         variable_cost_truck3 = sum_variable_cost(BurritoSold3),                          # Variable cost
         variable_cost_truck5 = sum_variable_cost(BurritoSold5),
         total_profit_truck3 = (revenue_truck3-Cmove),                                   # Net total revenue 
         total_profit_truck5 = (revenue_truck5-Cmove),
         rejected_truck3 = (df_base$Reject_b2 + df_base$Reject_b3 + df_base$Reject_b5),   # Rejections for each truck
         rejected_truck5 = (df_base$Reject_b1 + df_base$Reject_b4)
         )

# df_three mutated on variables
df_three <- df_three %>% 
  mutate(throughput_truck3 = throughput(PeopleBeenInLine3),                               # Throughput
         throughput_truck5 = throughput(PeopleBeenInLine5),
         throughput_truck6 = throughput(PeopleBeenInLine6),
         processing_truck3 = mu(ServiceTime3),                                            # Processing rate for the truck
         processing_truck5 = mu(ServiceTime5),
         processing_truck6 = mu(ServiceTime6),
         steadystate_truck3 = steady_state(throughput_truck3, processing_truck3),         # Steady state
         steadystate_truck5 = steady_state(throughput_truck5, processing_truck5),
         steadystate_truck6 = steady_state(throughput_truck6, processing_truck6),
         cycletime_truck3 = cycle_time(ServiceQueueServed3, ServiceTime3),                # Cycle time
         cycletime_truck5 = cycle_time(ServiceQueueServed5, ServiceQueue5),
         cycletime_truck6 = cycle_time(ServiceQueueServed5, ServiceQueue6),
         revenue_truck3 = sum_contribution(BurritoSold3),                                 # Net revenue
         revenue_truck5 = sum_contribution(BurritoSold5),
         revenue_truck6 = sum_contribution(BurritoSold6),
         variable_cost_truck3 = sum_variable_cost(BurritoSold3),                          # Variable cost
         variable_cost_truck5 = sum_variable_cost(BurritoSold5),
         variable_cost_truck6 = sum_variable_cost(BurritoSold6),
         total_profit_truck3 = (revenue_truck3-Cmove),                                   # Net total revenue
         total_profit_truck5 = (revenue_truck5-Cmove),
         total_profit_truck6 = (revenue_truck6-Cmove),
         rejected_truck3 = (df_three$Reject_b3 + df_three$Reject_b5),                     # Rejections for each truck
         rejected_truck5 = (df_three$Reject_b1 + df_three$Reject_b4),
         rejected_truck6 = (df_three$Reject_b2)
  )


### Final Output analasys numbers ----
# Finding the mean from the scenario to find simulated number from JaamSim
# Selecting only viable numbers used in the output analysis

# Mean for df_base
df_mean_base <- df_base %>% 
  group_by(WorkersPerTruck) %>% 
  summarise_all(mean) %>% 
  select(WorkersPerTruck, 
         ServiceTime3,
         ServiceTime5,
         ServiceQueue3,
         ServiceQueue5,
         throughput_truck3,
         throughput_truck5,
         steadystate_truck3,
         steadystate_truck5,
         rejected_truck3,
         rejected_truck5,
         cycletime_truck3,
         cycletime_truck5,
         total_profit_truck3,
         total_profit_truck5
         ) %>% 
  jaam_long() %>% 
  mutate(S = 1)

# Modifying the Scenario column to correct variables
df_mean_base$S <- c(1,1,2,2,3,3)

# For df_three
df_mean_three <- df_three %>% 
  group_by(WorkersPerTruck) %>% 
  summarise_all(mean) %>% 
  select(WorkersPerTruck,
         ServiceTime3,
         ServiceTime5,
         ServiceTime6,
         ServiceQueue3,
         ServiceQueue5,
         ServiceQueue6,
         throughput_truck3,
         throughput_truck5,
         throughput_truck6,
         steadystate_truck3,
         steadystate_truck5,
         steadystate_truck6,
         rejected_truck3,
         rejected_truck5,
         rejected_truck6,
         cycletime_truck3,
         cycletime_truck5,
         cycletime_truck6,
         total_profit_truck3,
         total_profit_truck5,
         total_profit_truck6
         ) %>% 
  jaam_long() %>% 
  mutate(S = 4)

# Modifying the Scenario column to correct variables
df_mean_three$S <- c(4,4,4,5,5,5)

# Combining the two data frames int main data frame
# This is the data frame where our data in the output
# analysis is from 
df_main <- rbind(df_mean_base, df_mean_three) 


# Monetary values for each scenario
df_sagg <- df_main %>% 
  group_by(S) %>% 
  summarise(SumTotalProfit = sum(total_profit_truck),
            SumTotalProfitPerDay = sum(total_profit_truck)/30,
            MeanServiceTimes = mean(ServiceTime),
            MeanQueueTimes = mean(ServiceQueue),
            SumRejected = sum(rejected_truck)) %>% 
  # Importing the revenue from the optimization model reformulated to a 30 day average
  # with the fixed costs spread out
  left_join(tibble(
    S = c(1,2,3,4,5),
    OptimizationProfit30days = c((3340+(Cmove*2)-(Cmove*2/30)),
                                 (3340+(Cmove*2)-(Cmove*2/30)),
                                 (3340+(Cmove*2)-(Cmove*2/30)),
                                 (2880+(Cmove*3)-(Cmove*3/30)),
                                 (2880+(Cmove*3)-(Cmove*3/30)))
  ))


df_sagg_long <- gather(df_sagg, key = "variable", value = "value", -S, -OptimizationProfit30days)
p <- ggplot(df_sagg_long, aes(x = S, y = value)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Observations for each scenario", x = "Scenario", y = "Value")
ggsave("output/scenario_aggregated_plot.png", p, dpi = 300)

