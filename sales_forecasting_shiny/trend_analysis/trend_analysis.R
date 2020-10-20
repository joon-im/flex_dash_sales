# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# TREND ANALYSIS ----

# 1.0 LIBRARIES -----

# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
library(timetk)      

source("00_scripts/04_demand_forecast.R")


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

# 3.0 TREND EVALUATION ----

processed_data_tbl %>%
    aggregate_time_series("month") %>%
    generate_forecast(n_future = 24) %>%
    plot_forecast()


# 3.1 XGBOOST ----

source("trend_analysis/model_forecast_xgb.R")

processed_data_tbl %>%
    aggregate_time_series("quarter") %>%
    generate_forecast_xgb(n_future = 10, seed = 123, 
                          mtry = 30, 
                          trees = 500, 
                          min_n = 5, 
                          tree_depth = 6, 
                          learn_rate = 0.01, 
                          loss_reduction = 0.01) %>%
    plot_forecast()


# 3.2 GLMNET ----

source("trend_analysis/model_forecast_glmnet.R")

processed_data_tbl %>%
    aggregate_time_series("quarter") %>%
    generate_forecast_glmnet(n_future = 8, seed = 123, 
                             penalty = 1, mixture = 0.5) %>%
    plot_forecast()
