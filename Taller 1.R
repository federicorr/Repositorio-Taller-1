install.packages("pacman")
install.packages("rio")
install.packages("tidyverse")
install.packages("skimr")
install.packages("caret")



library(pacman)
library(rio)
library(tidyverse)
library(skimr)
library(caret)

import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

urlbase <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/", 1:10, ".html")

#Otra opción (Juan Camilo)

library(pacman)
p_load(readr, rvest, tidyverse, dplyr, matrixStats, boot, caret, stargazer, tibble, ggplot2)

urls <- c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")

chunks <- list()

for (url in urls) {
  chunk <- read_html(url) %>%
    html_table() %>%
    as.data.frame()
  chunks <- append(chunks, list(chunk))
}

df <- dplyr::bind_rows(chunks)

#mayores de 18
df18<- df[df$age >=18, ]

# Analicemos la estructura de la base
glimpse(df18)
sum(is.na(df18))

# Chequeamos los NAs de la base
sapply(df18, function(x) sum(is.na(x)))
#LIMPIANDO LA BASE
#Creamos variable log de ingresos laborales por horas
df18 = df18 %>% 
  mutate(log_inglab_h = log(y_ingLab_m_ha))

#eliminamos las observaciones que tengan NA en ingresos laborales
df18 <- df18 %>% filter(log_inglab_h != "NA")
#edad al cuadrado
df18 = df18 %>% 
  mutate(age2 = (age)^2)
##BASE FINAL SELECCIONANDO VARIABLES
df18 <- select(df18,college, maxEducLevel, age, age2, estrato1, sex, regSalud, cotPension, 
                log_inglab_h, y_ingLab_m_ha,sizeFirm, microEmpresa, oficio, 
                hoursWorkUsual, informal, relab )
       
#PUNTO 3
       
# correr regresión 
regresion1 <- lm("log_inglab_h ~ age + age2", data = df18)
summary(regresion1)

# ver coeficiente de la regresión
lm_summary = as.data.frame(summary(regresion1)$coefficients)

#Intervalos de confianza de la predicción
y_predicho<- predict(regresion1)
df18 <- cbind(df18, y_predicho)
confint(regresion1)

#Errores bootstrap
set.seed(12345)
n<- length(df18$log_inglab_h)
R<-1000
eta_mod1<- rep(0,R)
eta_mod2<-rep(0,R)
eta_mod3<-rep(0,R)
y_predichos<- matrix(NA,n,R)
for (i in 1:R){
  df_sample<- sample_frac(df18,size=1, replace=TRUE)
  f<- lm(log_inglab_h~age+age2, df_sample)
  coefs<- f$coefficients
  
  eta_mod1[i]<- coefs[1]
  eta_mod2[i]<- coefs[2]
  eta_mod3[i]<- coefs[3]
  
} 
for (i in 1:R){
  columnas<- eta_mod1[i]+eta_mod2[i]*df18$age+eta_mod3[i]*df18$age2
  y_predichos[,i]<-columnas
}
ee<-rowSds(y_predichos)
df2<-cbind(df18,ee)
IC_bajo=df18$y_predicho-1.96*ee
IC_alto=df18$y_predicho+1.96*ee

df18<- cbind(df18,IC_alto, IC_bajo)

#Gráfico y predicho modelo 1 con IC por bootstrap
ggplot(df18, aes(age, y_predicho)) + geom_point() +                                
  geom_line(color = "blue", size = 2) +
  geom_ribbon(aes(ymin=IC_bajo, ymax=IC_alto), alpha=0.1, fill = "red", 
              color = "black", linetype = "dotted")
       
#5A
#Training(70%) y testing (30%)

id_train <- sample (1:nrow(df18), size = 0.7*nrow(df18), replace =F) 
train <- df18["id_train"]
set.seed(10101)

id_test <- sample (1:nrow(df18), size = 0.3*nrow(df18), replace =F) 
test <- df18["id_test"]
set.seed(10101)

#Hacemos Lasso para saber la combinacion de variables que seria ideal en el modelo 
# Para obtener un ajuste con regularizaci?n Lasso se indica argumento alpha = 1.

modelo_lasso <- glmnet(
  x = x_ingLab_m,
  y = y_ingLab_m,
  alpha = 1,
  nlambda = 300,
  standardize = FALSE

