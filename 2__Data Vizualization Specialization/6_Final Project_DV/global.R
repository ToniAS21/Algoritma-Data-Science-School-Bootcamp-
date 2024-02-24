library(shiny)
library(shinydashboard)

library(tidyverse)
library(ggplot2)
library(plotly)
library(highcharter)
library(lubridate)
library(mapdata)
library(tidyr)
library(zoo)
library(tsibble)
library(echarts4r)
library(scales)
library(glue)

df <- readRDS("data_clean/superstore.rds")

useBytes = TRUE

# Untuk Page 2a `Ship Mode`
df_rn <- 
  df %>% 
  rename(`Ship Mode` = Ship.Mode)