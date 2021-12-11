#install.packages("covidcast")
library(covidcast)
library(tidyverse)
library(patchwork)

data <- covidcast_signal("doctor-visits", "smoothed_cli", start_day = "2020-05-01", end_day = "2020-06-07")
