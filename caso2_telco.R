# Universidad Austral - Maestria en Ciencia de Dato (MCD)
# Materia: Introducción al Data Mining 
# Resolusion caso 2 - Telco Company Chrurn
# Datos del Equipo: 

library(dbplyr)
library(tidyverse)
library(ggplot2)

# Set working directory
source("local_config.R")

# Get data from file and create dataset
data_raw <- read.csv("Churn.csv", sep=";", T)

# Looking at the dataset
str(data_raw)

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
  mutate(Intl_Plan = as.factor(Intl_Plan)) %>%
  mutate(Vmail_Plan = as.factor(Vmail_Plan)) 

str(dataset)  
summary(dataset)

#Variables validation

#Churn proportions
sum.churn <- summary(dataset$Churn)
sum.churn

prop.churn <- sum(dataset$Churn == "TRUE") / length(dataset$Churn)
prop.churn

#Analasis de variables categoricas

#Plan internacional
counts1 <- table(dataset$Churn, dataset$Intl_Plan,
                dnn=c("Churn", "International Plan"))
sumtable1 <- addmargins(counts, FUN = sum)
sumtable1

#Proporcion por filas
row.margin1 <- round(prop.table(counts1, margin = 1), 4)*100 
row.margin1

#Porporcion por columna
col.margin1 <- round(prop.table(counts1, margin = 2), 4)*100
col.margin1

# Grafico de Barras Superpuesto Proporciones de churners por International Plan
ggplot() +
  geom_bar(data = dataset,
           aes(x = factor(Intl_Plan),
               fill = factor(Churn)),
           position = "stack") +
  scale_x_discrete("Customer Service Calls") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))

#Plan buzón de voz 
counts2 <- table(dataset$Churn, dataset$Vmail_Plan,
                 dnn=c("Churn", "Voice Plan"))
sumtable2 <- addmargins(counts2, FUN = sum)
sumtable2

#Proporcion por filas
row.margin2 <- round(prop.table(counts2, margin = 1), 4)*100 
row.margin2

#Porporcion por columna
col.margin2 <- round(prop.table(counts2, margin = 2), 4)*100
col.margin2

# Grafico de Barras Superpuesto Proporciones de churners por Plan de Voz 
ggplot() +
  geom_bar(data = dataset,
           aes(x = factor(Vmail_Plan),
               fill = factor(Churn)),
           position = "stack") +
  scale_x_discrete("Customer Service Calls") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))


#Variables Nùmericas

#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=CustServ_Calls)) + 
  geom_boxplot()

#Verficamos la cantidad de llamadas por 
ggplot() +
  geom_bar(data=dataset,
           aes(x = factor(CustServ_Calls),
               fill = factor(Churn)),
           position = "fill") +
  scale_x_discrete("Customer Service Calls") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))


  