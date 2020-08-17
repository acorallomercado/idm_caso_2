# Universidad Austral - Maestria en Ciencia de Dato (MCD)
# Materia: Introducción al Data Mining 
# Resolusion caso 2 - Telco Company Chrurn
# Datos del Equipo: 

library(dbplyr)
library(tidyverse)

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
  
  