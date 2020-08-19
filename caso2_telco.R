# Universidad Austral - Maestria en Ciencia de Dato (MCD)
# Materia: Introduccion al Data Mining 
# Resolusion caso 2 - Telco Company Chrurn
# Datos del Equipo: 

library(dbplyr)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tidyverse)
library(grDevices)
library(randomForest)
library(caret)
library(C50)
library(questionr)
library(e1071)


rm( list=ls() )
gc()

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

data_set_n <- select(dataset, c("Day_Mins", "Day_Calls", "Day_Charge", "Eve_Mins", "Eve_Calls", "Eve_Charge", "Night_Mins", "Night_Calls", "Night_Charge", "Intl_Mins", "Intl_Calls",  "Intl_Charge", "CustServ_Calls"))
cor_churn <- cor(data_set_n)

corrplot.mixed(cor_churn, lower="number", upper="shade", addshade = "all")

str(dataset)  
summary(dataset)

#Variable Validation

#Churn proportions
sum.churn <- summary(dataset$Churn)
sum.churn

prop.churn <- sum(dataset$Churn == "TRUE") / length(dataset$Churn)
prop.churn

###Categorical Variables

##International Plan
counts1 <- table(dataset$Churn, dataset$Intl_Plan,
                 dnn=c("Churn", "International Plan"))
sumtable1 <- addmargins(counts1, FUN = sum)
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
  scale_x_discrete("Plan Internacional") +
  scale_y_continuous("Porcentaje") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))

##Plan buzon de voz 
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
  scale_x_discrete("Plan de Buzon de Voz") +
  scale_y_continuous("Porcentaje") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))


###Variables Numericas

##Llamadas al Servicio de Clientes
#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=CustServ_Calls)) + 
  geom_boxplot()

#Verficamos la cantidad de Churners en relacion a los llamados a SAC 
ggplot() +
  geom_bar(data=dataset,
           aes(x = factor(CustServ_Calls),
               fill = factor(Churn)),
           position = "fill") +
  scale_x_discrete("Llamadas SAC") +
  scale_y_continuous("Porcentaje") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("green", "violet"))

#Cant de minutos diurnos
#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=Day_Mins)) + 
  geom_boxplot()

#Transformamos la variable Minutos Diarios
dataset$z.daymin <- (dataset$Day_Mins - mean(dataset$Day_Mins))/sd(dataset$Day_Mins)

#Calculamos el sesgo
sesgo.daymin <- (3*(mean(dataset$Day_Mins) - median(dataset$Day_Mins)))/sd(dataset$Day_Mins)
sesgo.daymin

#Histogramas de Day_min y Day_min Zscores
par(mfrow = c(1,2))
hist(dataset$Day_Mins, breaks = 30,
     xlim = c(0, 380),
     main = "Histograma de Day_min",
     xlab = "Day_min",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")
hist(dataset$z.daymin,
     breaks = 30,
     xlim = c(-3.5, 3.5),
     main = "Histograma de Day_min Zscores",
     xlab = "Day_min Z-score",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")

#Raiz cuadrada inversa 
invsqrt.daymin <- 1 / sqrt(dataset$Day_Mins)

#Normal probability plot
par(mfrow = c(1,1))
qqnorm(invsqrt.daymin,
       datax = TRUE,
       col = "red",
       ylim = c(0.01, 0.15),
       main = "Normal Probability Plot")
qqline(invsqrt.daymin,
       col = "blue",
       datax = TRUE)


ggplot(dataset, aes(Day_Mins, fill = Churn)) +
  geom_bar(position = "fill")+
  scale_x_binned(n.breaks = 20)+
  geom_vline(xintercept = quantile(dataset$Day_Mins), color="red")


##Cant de minutos internacionales
#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=Intl_Mins)) + 
  geom_boxplot()

#Transformamos la variable Minutos internacionales
dataset$z.intmin <- (dataset$Intl_Mins - mean(dataset$Intl_Mins))/sd(dataset$Intl_Mins)

#Calculamos el sesgo
sesgo.intmin <- (3*(mean(dataset$Intl_Mins) - median(dataset$Intl_Mins)))/sd(dataset$Intl_Mins)
sesgo.intmin

#Histogramas de Intl_mins y Intl_min Zscores 
par(mfrow = c(1,2))
hist(dataset$Intl_Mins, breaks = 30,
     xlim = c(0, 20),
     main = "Histograma de Intl_mins",
     xlab = "Intl_mins",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")
hist(dataset$z.intmin,
     breaks = 30,
     xlim = c(-3.5, 3.5),
     main = "Histogram de Zscores Intl_mins",
     xlab = "Intl_mins Z-score",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")

#Raiz cuadrada inversa 
invsqrt.intmin <- 1 / sqrt(dataset$Intl_Mins)

#Normal Probability score
par(mfrow = c(1,1))
qqnorm(invsqrt.intmin,
       datax = TRUE,
       col = "red",
       ylim = c(0.20, 0.40),
       main = "Normal Probability Plot")
qqline(invsqrt.intmin,
       col = "blue",
       datax = TRUE)


ggplot(dataset, aes(Intl_Mins, fill = Churn)) +
  geom_bar(position = "fill")+
  scale_x_binned(n.breaks = 10)+
  geom_vline(xintercept = quantile(dataset$Intl_Mins), color="red")

##Cant de minutos nocturnos
#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=Night_Mins)) + 
  geom_boxplot()

#Transformamos la variable Minutos nocturnos
dataset$z.nigmin <- (dataset$Night_Mins - mean(dataset$Night_Mins))/sd(dataset$Night_Mins)

#Calculamos el sesgo
sesgo.nigmin <- (3*(mean(dataset$Night_Mins) - median(dataset$Night_Mins)))/sd(dataset$Night_Mins)
sesgo.nigmin

#Histogramas de Night_Mins y Night_Mins Zscores 
par(mfrow = c(1,2))
hist(dataset$Night_Mins, breaks = 30,
     xlim = c(0, 400),
     main = "Histograma de Night_Mins",
     xlab = "Night_Mins",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")
hist(dataset$z.nigmin,
     breaks = 30,
     xlim = c(-3.3, 4),
     main = "Histogram de Zscores Night_Mins",
     xlab = "Night_Mins Z-score",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col="black")

#Raiz cuadrada inversa 
invsqrt.nigmin <- 1 / sqrt(dataset$Night_Mins)

#Normal Probability score
par(mfrow = c(1,1))
qqnorm(invsqrt.nigmin,
       datax = TRUE,
       col = "red",
       ylim = c(0.045, 0.21),
       main = "Normal Probability Plot")
qqline(invsqrt.nigmin,
       col = "blue",
       datax = TRUE)

ggplot(dataset, aes(Night_Mins, fill = Churn)) +
  geom_bar(position = "fill")+
  scale_x_binned(n.breaks = 20)+
  geom_vline(xintercept = quantile(dataset$Night_Mins), color="red")


##Cant meses con la cuenta
#Verificamos posible outliers
ggplot(dataset, aes(x=Churn, y=Account_Length)) + 
  geom_boxplot()

#Raiz cuadrada inversa 
invsqrt.acct <- 1 / sqrt(dataset$Account_Length)

#Normal Probability score
par(mfrow = c(1,1))
qqnorm(invsqrt.acct,
       datax = TRUE,
       col = "red",
       ylim = c(0.045, 0.21),
       main = "Normal Probability Plot")
qqline(invsqrt.acct,
       col = "blue",
       datax = TRUE)

ggplot(dataset, aes(Account_Length, fill = Churn)) +
  geom_bar(position = "fill")+
  scale_x_binned(n.breaks = 20)+
  geom_vline(xintercept = quantile(dataset$Account_Length), color="red")

# Graficos Dispersionn para analisis bivariado de variables cuantitativas
qplot(Day_Calls, CustServ_Calls, data = dataset, colour=Churn, main = "Dispersion de las variables Day Calls y Customer Service Calls, por Churn")
qplot(Day_Mins, CustServ_Calls, data = dataset, colour=Churn, main = "Dispersion de las variables Day Minutes y Customer Service Calls, por Churn")
qplot(Day_Mins, Eve_Mins, data = dataset, colour=Churn, main = "Dispersion de las variables Day Minutes y Evening Minutes, por Churn")

#Discretizamos las variables

#Llamadas al CustServ
dataset <- mutate(dataset, CustServ_Calls_dis = as.character(ifelse (CustServ_Calls > 3 , "High",
                                                                     ifelse(CustServ_Calls >= 0, "Low", CustServ_Calls))))


counts3 <- table(dataset$Churn, dataset$CustServ_Calls_dis,
                 dnn=c("Churn", "CustServ_Calls_dis"))
sumtable3 <- addmargins(counts3, FUN = sum)
sumtable3

col.margin3 <- round(prop.table(counts3, margin = 2), 4)*100
col.margin3


#Minutos diarios
dataset <- mutate(dataset, daymin_dis = as.character(ifelse(Day_Mins > 220 , "Day Mins > 220" ,
                                                            ifelse(Day_Mins > 140, "140 < Day Mins <= 220",
                                                                   ifelse(Day_Mins >= 0, "Day Mins <= 140")))))

counts4 <- table(dataset$Churn, dataset$daymin_dis,
                 dnn=c("Churn", "daymin_dis"))
sumtable4 <- addmargins(counts4, FUN = sum)
sumtable4

col.margin4 <- round(prop.table(counts4, margin = 2), 4)*100
col.margin4


#Minutos Internacionales 
dataset <- mutate(dataset, intlmin_dis = as.character(ifelse(Intl_Mins > 12 , "Intl Mins > 12" ,
                                                             ifelse(Intl_Mins > 6, "6 < Intl Mins <= 4",
                                                                    ifelse(Intl_Mins >= 0, "Intl Mins <= 4")))))

counts5 <- table(dataset$Churn, dataset$intlmin_dis,
                 dnn=c("Churn", "intlmin_dis"))
sumtable5 <- addmargins(counts5, FUN = sum)
sumtable5

col.margin5 <- round(prop.table(counts5, margin = 2), 4)*100
col.margin5

#Minutos Nocturnos
dataset <- mutate(dataset, nigmin_dis = as.character(ifelse(Night_Mins > 300 , "Night_Mins > 300" ,
                                                            ifelse(Night_Mins > 120, "120 < Night Mins <= 300",
                                                                   ifelse(Night_Mins >= 0, "Night Mins <= 120")))))


counts6 <- table(dataset$Churn, dataset$nigmin_dis,
                 dnn=c("Churn", "nigmin_dis"))
sumtable6 <- addmargins(counts6, FUN = sum)
sumtable6

col.margin6 <- round(prop.table(counts6, margin = 2), 4)*100
col.margin6

#Permanencia del cliente
dataset <- mutate(dataset, acct_dis = as.character(ifelse(Account_Length > 210 , "Account_Length > 210" ,
                                                            ifelse(Account_Length > 90, "90 < Account Length <= 210",
                                                                   ifelse(Account_Length >= 0, "Account Length <= 90")))))


counts7 <- table(dataset$Churn, dataset$acct_dis,
                 dnn=c("Churn", "Account_Length"))
sumtable7 <- addmargins(counts7, FUN = sum)
sumtable7

col.margin7 <- round(prop.table(counts7, margin = 2), 4)*100
col.margin7

#Se generar el dataset para el modelo

churn <- dataset %>% select(Intl_Plan, Vmail_Plan, CustServ_Calls_dis, daymin_dis, nigmin_dis, Churn)

#Particionamos 
set.seed(22)
churn$part <- runif(length(churn$Churn),
                  min = 0,
                  max = 1)

training <- churn[churn$part <= 0.75,]
testing <- churn[churn$part > 0.75,]

summary(training$Churn)
summary(testing$Churn)


proporcionsplit <- as.table(rbind(c(2005, 341),
                           c(647, 106)))
dimnames(proporcionsplit) <- list(Data.Set =
                             c("Training Set", "Test Set"),
                           Status = c("False", "True"))
#Se le corre el test a la tabla
Xsq_data <- chisq.test(proporcionsplit)
# Show the test statistic,
# p-value, expected frequencies
Xsq_data$statistic #estadìstico del chi cuadrado X2
Xsq_data$p.value #valor de p
Xsq_data$expected #valores esperados

#pvalue es 0.80, suficientemente grande para indicar que no hay evidencia de que las frecuencias
#observadas en los datasets de traing y testing sean significativamente distintas, por lo que la particion es valida.

#Eliminamos part y churn de test
testing <- testing[,-7]
training <- training[,-7]

#Se generan los arboles

##Arbol 1
tree_one <- rpart(Churn ~., data = training, method = 'class')
summary(tree_one)

prediccion_1 <- predict(tree_one, newdata = testing, type = "class")

confusionMatrix(prediccion_1, testing[["Churn"]])

#Utilizamos el C5.0

tree_c50 <- C5.0(Churn ~., training)
summary(tree_c50)

prediccion_2 <- predict(tree_c50, newdata = testing)

confusionMatrix(prediccion_2, testing[["Churn"]])

#Creamos forest

forest500 <- randomForest(Churn ~., data = training, ntree = 500, importance = T)
summary(forest500)

prediction_3 <- predict(forest500, testing)
confusionMatrix(prediction_3, testing[["Churn"]])

