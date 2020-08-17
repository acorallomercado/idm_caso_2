# Universidad Austral - Maestria en Ciencia de Dato (MCD)
# Materia: Introducci??n al Data Mining 
# Resolusion caso 2 - Telco Company Chrurn
# Datos del Equipo: 

library(dbplyr)
library(tidyverse)
library(corrplot)

# Set working directory
source("local_config.R")

# Get data from file and create dataset
data_raw <- read.csv("Churn.csv", sep=";", T)

# Fix numeric values.
dataset <- data_raw %>% mutate(Day_Mins = as.numeric(sub(',', '.', Day_Mins, fixed = TRUE))) %>%
  mutate(Day_Mins = as.numeric(sub(',', '.', Day_Mins, fixed = TRUE))) %>%
  mutate(Day_Calls = as.numeric(sub(',', '.', Day_Calls, fixed = TRUE))) %>%
  mutate(Day_Charge = as.numeric(sub(',', '.', Day_Charge, fixed = TRUE))) %>%
  mutate(Eve_Mins = as.numeric(sub(',', '.', Eve_Mins, fixed = TRUE))) %>%
  mutate(Eve_Calls = as.numeric(sub(',', '.', Eve_Calls, fixed = TRUE))) %>%
  mutate(Eve_Charge = as.numeric(sub(',', '.', Eve_Charge, fixed = TRUE))) %>%
  mutate(Night_Mins = as.numeric(sub(',', '.', Night_Mins, fixed = TRUE))) %>%
  mutate(Night_Calls = as.numeric(sub(',', '.', Night_Calls, fixed = TRUE))) %>%
  mutate(Night_Charge = as.numeric(sub(',', '.', Night_Charge, fixed = TRUE))) %>%
  mutate(Intl_Mins = as.numeric(sub(',', '.', Intl_Mins, fixed = TRUE))) %>%
  mutate(Intl_Calls = as.numeric(sub(',', '.', Intl_Calls, fixed = TRUE))) %>%
  mutate(Intl_Charge = as.numeric(sub(',', '.', Intl_Charge, fixed = TRUE))) %>%
  mutate(CustServ_Calls = as.numeric(sub(',', '.', CustServ_Calls, fixed = TRUE)))

# Check categorical values. 
table(dataset$Churn)
dataset <- dataset %>% mutate(Churn = as.factor(ifelse(Churn=="False.", "FALSE","TRUE")))

table(dataset$Intl_Plan)
table(dataset$Vmail_Plan)

# Set categorical values as factor.
dataset <- dataset %>% mutate(State = as.factor(State)) %>% 
  mutate(Area_Code = as.factor(Area_Code)) %>%
  mutate(Phone = as.factor(Area_Code)) %>%
  mutate(Intl_Plan = as.factor(Intl_Plan)) %>%
  mutate(Vmail_Plan = as.factor(Vmail_Plan)) 

data_set_n <- select(dataset, c("Day_Mins", "Day_Calls", "Day_Charge", "Eve_Mins", "Eve_Calls", "Eve_Charge", "Night_Mins", "Night_Calls", "Night_Charge", "Intl_Mins", "Intl_Calls",  "Intl_Charge", "CustServ_Calls"))
cor_churn <- cor(data_set_n)

#corrplot(cor_feliz, method = "ellipse")
#corrplot.mixed(cor_churn, lower="number", upper="shade", addshade = "all")
corrplot(cor_churn)