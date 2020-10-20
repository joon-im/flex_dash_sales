# 1.0 LIBRARIES -----

# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
library(timetk)


# 2.0 PROCESSED DATA ----
# Load data
sales_data_raw <- read_csv('sales_data_sample.csv') 
#country_codes <- read_csv("https://datahub.io/core/country-list/r/data.csv")
#country_codes <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# Select relevant data
processed_data_tbl <- sales_data_raw %>% 
  select(ORDERDATE, ORDERNUMBER, ORDERLINENUMBER, COUNTRY, SALES, PRODUCTLINE, DEALSIZE, STATUS)

# Preprocessing
processed_data_tbl <- processed_data_tbl %>%
  mutate(ORDERDATE = mdy_hm(ORDERDATE),
         ORDERDATE = as_date(ORDERDATE))

# Manual edits
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="UK"] <- "United Kingdom"
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="USA"] <- "United States"

processed_data_tbl %>%
  arrange(ORDERDATE) %>%
  head()

# 3.0 TIME SERIES AGGREGATION ----

# 3.1 DATA MANIPULATION ----
time_unit <- "day"

time_plot_tbl <- processed_data_tbl %>%
  
  mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>%
  
  group_by(date) %>%
  summarize(total_sales = sum(SALES)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Date: {date}
                               Revenue: {scales::dollar(total_sales)}"))

time_plot_tbl

# 3.2 FUNCTION ----

aggregate_time_series <- function(data, time_unit = "month") {
  
  output_tbl <- data %>%
    
    mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>%
    
    group_by(date) %>%
    summarize(total_sales = sum(SALES)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
  
  return(output_tbl)
  
}

processed_data_tbl %>%
  aggregate_time_series(time_unit = "quarter")

# 3.3 TIME SERIES PLOT ----

g <- data %>%
  
  ggplot(aes(date, total_sales)) +
  
  geom_line(color = "#2c3e50") +
  geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
  geom_smooth(method = "loess", span = 0.2) +
  
  theme_tq() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "", y = "")


ggplotly(g, tooltip = "text")


# 3.4 FUNCTION ----

plot_time_series <- function(data) {
  
  g <- data %>%
    
    ggplot(aes(date, total_sales)) +
    
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")
  
  ggplotly(g, tooltip="text")
}

plot_time_series(processed_data_tbl %>% aggregate_time_series(time_unit = "day"))


# 4.0 FORECAST -----

# Why use ML instead of Auto-Regressive such as ARIMA, Holt Winters, State Space, etc?
## 1. Speed
## 2. Better On-the-Fly
##  -ML good at pattern detection i.e. seasonality w/o much tuning (flexible)
##  -AR requires lots of tuning and tuning is different depending on time series - bad because user input is variable (inflexible)

# 4.1 SETUP TRAINING DATA AND FUTURE DATA ----

# TODO - timetk

data <- processed_data_tbl %>%
  aggregate_time_series("year")

train_tbl <- data %>% tk_augment_timeseries_signature()

future_data_tbl <- data %>%
  mutate(date = as_date(date)) %>%
  tk_index() %>%
  tk_make_future_timeseries(n_future = 12, inspect_weekdays=TRUE, inspect_months = TRUE) %>% # Analyze w/o weekends
  tk_get_timeseries_signature() 



# 4.2 MACHINE LEARNING ----

# TODO - XGBoost

## XGBoost uses random trees: make reproducible 
## Good when there are patterns in data (day, month, quarter) but not good when there are less patterns (year)
seed <- 123
set.seed(seed)

# Parsnip model: parameters normally chosen thru training/testing but can use basic parameters 
model_xgboost <- boost_tree(
  mode = "regression",
  mtry = 20,  # Num columns to use: using all will overfit, so reduce to 2/3 of columns
  trees = 500,  # Num trees to use: speed is essential for real-time training
  min_n = 3,  # Min Nodes: each node must have 3 observations/values minimum
  tree_depth = 8, # Max tree depth is 8 levels to prevent overfitting
  learn_rate = 0.01,  # Ensure we find a high accuracy solution (small)
  loss_reduction = 0.01)  %>% # Each split must improve model by 1% to make a split/node
  set_engine(engine="xgboost") %>% # Set model engine: provide 'object=' for model_spec
  fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))

# 4.3 MAKE PREDICTION & FORMAT OUTPUT ----

# TODO - predict
prediction_tbl <- predict(model_xgboost, new_data = future_data_tbl) %>%
  bind_cols(future_data_tbl) %>%
  select(.pred, index) %>%
  rename(total_sales = .pred, 
         date        = index) %>%
  mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
  add_column(key = "Prediction")

output_tbl <- data %>%
  add_column(key = "Actual") %>%
  bind_rows(prediction_tbl) 

output_tbl

# 4.4 FUNCTION ----

# TODO - generate_forecast()

n_future <- 2
seed <- 123

generate_forecast <- function(data, n_future = 12, seed = NULL) {
  
  train_tbl <- data %>% 
    tk_augment_timeseries_signature()
  
  future_data_tbl <- data %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
    tk_get_timeseries_signature() 
  
  # Isolate and pull scale 
  time_scale <-  data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  # Linear Regression for "year", XGBoost for other time units
  if (time_scale == "year") {
    
    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(total_sales ~ ., data = train_tbl %>% select(total_sales, index.num))
    
    
  } else {
    seed <- seed
    set.seed(seed)
    model <- boost_tree(
      mode = "regression", 
      mtry = 20, 
      trees = 500, 
      min_n = 3, 
      tree_depth = 8, 
      learn_rate = 0.01, 
      loss_reduction = 0.01) %>%
      set_engine(engine = "xgboost") %>%
      fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))
    
  }
  
  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(total_sales = .pred, 
           date        = index) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    add_column(key = "Prediction")
  
  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl) 
  
  output_tbl
  
  return(output_tbl)
}


processed_data_tbl %>%
  aggregate_time_series(time_unit = "quarter") %>%
  generate_forecast(n_future = 4, seed = 123) 


# 5.0 PLOT FORECAST ----

# 5.1 PLOT ----

# TODO - plot
data <- processed_data_tbl %>%
  aggregate_time_series(time_unit = "month") %>%
  generate_forecast(n_future = 12, seed = 123) 

g <- data %>%
  ggplot(aes(date, total_sales, color = key)) +
  
  geom_line() +
  geom_point(aes(text = label_text), size = 0.01) +
  geom_smooth(method = "loess", span = 0.2) +
  
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "", y = "")

ggplotly(g, tooltip = "text")

# 5.2 FUNCTION ----

# TODO - plot_forecast()

data <- processed_data_tbl %>%
  aggregate_time_series(time_unit = "year") %>%
  generate_forecast(n_future = 2, seed = 123)

plot_forecast <- function(data) {
  
  # Yearly - LM Smoother (Loess)
  
  time_scale <- data %>% 
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  # If only one yearly prediction -> points
  
  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()
  
  g <- data %>%
    ggplot(aes(date, total_sales, color = key)) +
    
    geom_line() +
    # geom_point(aes(text = label_text), size = 0.01) +
    # geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y=0) + 
    labs(x = "", y = "")
  
  # Yearly - LM Smoother
  if (time_scale == "year") {
    g <- g + geom_smooth(method = "lm")
  } else {
    g <- g + geom_smooth(method = "loess", span = 0.2)
  }
  
  # Only 1 Prediction
  if (n_predictions == 1) {
    g <- g + geom_point(aes(text = label_text), size = 1)
  } else {
    g <- g + geom_point(aes(text = label_text), size = 0.01)
  }
  
  ggplotly(g, tooltip = "text")
  
}

processed_data_tbl %>%
  aggregate_time_series(time_unit = "quarter") %>%
  generate_forecast(n_future = 1, seed = 123) %>%
  plot_forecast()

# 6.0 SAVE FUNCTIONS ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "/cloud/project/00_scripts/04_demand_forecast.R")
