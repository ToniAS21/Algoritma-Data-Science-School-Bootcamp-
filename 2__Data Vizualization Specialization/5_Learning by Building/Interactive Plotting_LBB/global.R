library(shiny)
library(shinydashboard)

# Library
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(scales)
library(glue)
library(quantmod)
library(tidyverse)
library(xts)
library(highcharter)

# Read data
cl_dow_stock18 <- readRDS("data_input_clean/cl_dow_stock18.rds")
dow_companies <- readRDS("data_input_clean/dow_companies.rds")

# Data Preparation for Expert Page

bbands <- BBands(cl_dow_stock18[,c("High","Low","Close")])

dt_exp <- xts(cl_dow_stock18[,-2], order.by=cl_dow_stock18[,2])

dt_exp <- data.frame(Date=index(dt_exp),coredata(dt_exp))

# Adjusment Type data after xts format

dt_exp <- 
  dt_exp %>% mutate(
    Symbol = as.factor(Symbol),
    Adj.Close = as.numeric(Adj.Close),
    Close = as.numeric(Close),
    High = as.numeric(High),
    Low = as.numeric(Low),
    Open = as.numeric(Open),
    Volume = as.numeric(Volume),
    year = as.numeric(year)
  )
