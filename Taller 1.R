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
       
#Hallar peak age 
peak_y_predicho=(max(df18$y_predicho))
peak_y_predicho #8.892299
#este valor está en la posición 23
peak_age=(df18$age[23])
peak_age #45
       
#5A
#Training(70%) y testing (30%)

sample <- sample(c(TRUE, FALSE), nrow(df18), replace=TRUE, prob=c(0.7,0.3))
train  <- df18[sample, ]
test   <- df18[!sample, ]
set.seed(10101)
#5b
#Hacemos Lasso para saber la combinacion de variables que seria ideal en el modelo 
# Para obtener un ajuste con regularizaci?n Lasso se indica argumento alpha = 1.

modelo_lasso <- glmnet(
  x = x_ingLab_m,
  y = y_ingLab_m,
  alpha = 1,
  nlambda = 300,
  standardize = FALSE
  
  #make this example reproducible
  set.seed(1)
  
   #PUNTO CUATRO
          
   #Estimación incondicional modelo de género
    #Hallar logaritmo del ingreso
    ingtot2 = base2$ingtot
    ingtot2<-ifelse((ingtot2)==0,1,ingtot2) #reemplazar los ingreso= 0 por 1
    log_ingtot<- log(ingtot2)
    base2<- cbind(base2,log_ingtot)
    #Estimar modelo
    regresion2<- lm(log_ingtot~sex, data=base2)
    lm_summary2=as.data.frame(summary(regresion2)$coefficients)

    #Estimación incondicional modelo age-género
    regresion3<- lm(log_ingtot~sex+age+age2, data=base2)
    lm_summary3=as.data.frame(summary(regresion3)$coefficients)
    y_predicho3<- predict(regresion3)
    base2<- cbind(base2, y_predicho3)
   
    
      #peak age hombres con modelo de age 
      base_hombres= subset(base2, base$sex==1)
      
      #peak age mujeres con modelo age 
      base_mujeres= subset(base2, base$sex==0)
     
      
     
      #Errores bootstrap modelo y~age+age2+sex
      eta_mod11<-rep(0,R)
      eta_mod22<-rep(0,R)
      eta_mod33<-rep(0,R)
      eta_mod44<-rep(0,R)
      
      set.seed(12345)
      n<- length(base2$log_ingtot)
      R<-1000
      y_predichos2<- matrix(NA,n,R)
      for (i in 1:R){
        db_sample<- sample_frac(base2,size=1, replace=TRUE)
        f2<- lm(log_ingtot~sex+age+age2, db_sample)
        coefs2<- f2$coefficients
        
        eta_mod11[i]<- coefs2[1]
        eta_mod22[i]<- coefs2[2]
        eta_mod33[i]<- coefs2[3]
        eta_mod44[i]<- coefs2[4]
        
      } 
      for (i in 1:R){
        columnas2<- eta_mod11[i]+eta_mod22[i]*base2$sex+eta_mod33*base2$age+eta_mod44*base2$age2
        y_predichos2[,i]<-columnas2
      }
      ee2<-rowSds(y_predichos2)
      df22<-cbind(base2,ee2)
      IC_bajo2=base2$y_predicho3-1.96*ee2
      IC_alto2=base2$y_predicho3+1.96*ee2
      
      base2<- cbind(base2,IC_alto2, IC_bajo2)
      
      #Gráfico con modelo y=age+age2+sex divido por género por bootstrap
      ggplot(base2, aes(age, y_predicho3)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo2, ymax=IC_alto2), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")+facet_grid(sex~.)

      #peak age hombres modelo y=age+age2+sex por bootstrap
      base_hombres2= subset(base2, base$sex==1)
      peak_hombre2<-(which.max(base_hombres$y_predicho3))#16
      peak_age_hombre2=(base_hombres$age[16])#43
      as.integer(max(base_hombres$ingtot))#85.833.333
      
      #peak age mujeres modelo y=age+age2+sex por bootstrap
      base_mujeres2= subset(base2, base$sex==0)
      peak_mujeres2<-(which.max(base_mujeres$y_predicho3))#23
      peak_age_mujer2=(base_mujeres$age[23])#43
      as.integer(max(base_mujeres$ingtot))#40.000.000

      #Modelo con interaccion age-sex para analisis de pendiente
      regresion4<- lm(log_ingtot~sex+age+age2+sex:age, data=base2) 
      summary(regresionaux)
      
      stargazer(regresion4,regresion3, regresion2, regresion1,type="text")

      #regresion condicional
      #denominar dummies como factores
      base2$maxEducLevel<-as.factor(base2$maxEducLevel)
      base2$estrato1<-as.factor(base2$estrato1)
      base2$regSalud <-as.factor(base2$regSalud)
      base2$cotPension<-as.factor(base2$cotPension)
      base2$sizeFirm<-as.factor(base2$sizeFirm)
      base2$oficio<-as.factor(base2$ofici)
      base2$informal<-as.factor(base2$informal)
      base2$relab<-as.factor(base2$relab)
      
      
      
      #estimación modelo condicional
      regresion5<- lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension
                      +sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal
                      +relab, data=base2)
      lm_summary5=as.data.frame(summary(regresion5$coefficients))
      
      
      #FWL
      base2<-base2 %>% mutate(res_y_a=lm(log_ingtot~maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con ingreso
                              res_s_a=lm(sex~sex+maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con sexo
      )
      
      regresion6<-lm(res_y_a~res_s_a-1,base2)
      
      #boostrap FWL
      
      set.seed(12345)
      eta.fn<-function(base2,i){
        coef(lm(res_y_a~res_s_a-1,base2, subset = i))
      }
      replicas<- boot(data = base2, statistic = eta.fn, R = 1000)
      
      
      stargazer(rregresion3, regresion5, regresion6,type="text")

