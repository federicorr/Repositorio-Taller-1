#PROBLEM SET 1: PREDICTING INCOME

#PUNTO 2

#Creacion de base de datos

library(pacman)
p_load(readr, rvest, tidyverse, dplyr, matrixStats, boot, caret, stargazer, tibble, ggplot2)

#Scrapping de datos

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

#Limpieza de base de datos

#Definir poblacion mayor de 18 años
df18<- df[df$age >=18, ]

# Analizar la estructura de la base
glimpse(df18)
sum(is.na(df18))

# Chequear los NAs de la base
sapply(df18, function(x) sum(is.na(x)))
       
#Crear variable log de ingresos laborales por horas
df18 = df18 %>% 
  mutate(log_inglab_h = log(y_ingLab_m_ha))

#Eliminar las observaciones que tengan NA en ingresos laborales
df18 <- df18 %>% filter(log_inglab_h != "NA")
       
#Crear variable de edad al cuadrado
df18 = df18 %>% 
  mutate(age2 = (age)^2)
       
#Generar base de datos final
df18 <- select(df18,college, maxEducLevel, age, age2, estrato1, sex, regSalud, cotPension, 
                log_inglab_h, y_ingLab_m_ha,sizeFirm, microEmpresa, oficio, 
                hoursWorkUsual, informal, relab )
       
#Estadísticas descriptivas
       
  #Variables discretas
    
    aggregate(df18$age,by=list(df18$sex),mean)
    
    table(df18$regSalud, df18$maxEducLevel)
    table(df18$age, df18$regSalud)
    table(df18$regSalud, df18$sex)
    table(df18$regSalud, df18$cotPension)
    table(df18$regSalud, df18$maxEducLevel)
    
    #Variables continuas
       
    ingreso <- (as.data.frame(summary(df18))) ; wage
    output <- capture.output(ingreso, file=NULL, append =FALSE)
    output_ad <-as.data.frame(output) #convertir summary en tabla
    write.table(x = output_ad, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)
    
    wage <- df18 %>%  summarize(mean(df18$y_ingLab_m_ha,na.rm = T));wage
       
        # Promedio de salario por grupos
       
    # por sexo
    a <- df18 %>% group_by(sex) %>% summarize(mean(df18$y_ingLab_m_ha,na.rm = T));a
    # por edad
    b <- df18 %>% group_by(age) %>% summarize(mean(y_ingLab_m_ha,na.rm = T));b 
    # por estrato
    c <- df18 %>% group_by(estrato1) %>% summarize(mean(y_ingLab_m_ha,na.rm = T));c
    
         #Varianzas de variables continuas
    df18 %>% var()
    var(df18$y_ingLab_m_ha) 
    var(df18$age) 
    var(df18$hoursWorkUsual)
    var(df18$sex)

          #Diferencia de medias de wage entre edades <57> y sexo
    d_medias_sex<- t.test (df18$y_ingLab_m_ha ~ df18$sex ) ;d_medias_sex
    
    
          #Gráficos
       
    #Diferencia de medias
    g_medias <- boxplot(df18$y_ingLab_m_ha ~ df18$sex, col= "gray", xlab ="sexo", ylab = "salario")
    stargazer(type="text", title ="difmean gen", TRUE) #exportar
       
    #Histograma de salario
    gp <-  ggplot() + geom_histogram(data = df18,aes(x=y_ingLab_m_ha));gp
    
   #GRAPH RONNY
 
    #Histograma edad
    gp3<- ggplot() + geom_histogram(data = df18, aes(x=age));gp3
       
    #Scatter wage-sexo
    gp4<- ggplot()+geom_point(data=df18, aes(y=y_ingLab_m_ha, x=age))+ "división sex"
    +ylab("Número de personas") + xlab("Sexo") + ggtitle("Número de personas por sexo")+ 
      scale_x_discrete(limit = c("Hombre", "Mujer"))
   
    #Gráfico de dispersión del salario promedio por sexo
    grafico1 <- plot(a, main = "Salario promedio por sexo", xlab = "sexo", ylab = "Salario promedio", pch = 21,  bg = "yellow", col = "red", cex = 1, lwd = 2)
    ggsave(plot= grafico1 , file = "views/Grafico_wage_sex.jpeg")

    #Gráfico de salario promedio por edad
    grafico2 <- plot(b,type="h",main = "Salario promedio por edad", xlab = "Edad", ylab = "Salario promedio", col = "Darkblue",lwd=2, ylim=c(0,9000000),xlim=c(15,85))
    ggsave(plot= grafico2 , file = "views/Grafico_wage_edad.jpeg") 

    #Tabla de correlación
    correlacion <- (as.data.frame(cor(df18))) ; correlacion
    output_corr <- capture.output(correlacion, file=NULL, append =FALSE)
    output_corr <-as.data.frame(output_corr) #convertir summary en tabla
    write.table(x = output_corr, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)
       
#PUNTO 3 - Age-wage profile
       
       #Tabla de regresión
regresion1 <- lm("log_inglab_h ~ age + age2", data = df18)
summary(regresion1)
       
       #Ver coeficiente de la regresión
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
#Este valor está en la posición 23
peak_age=(df18$age[23])
peak_age #45
       
#PUNTO CUATRO: The gender earnings GAP
          
   #Estimación incondicional modelo de género
       
    #Hallar logaritmo del salario
       
    log_inglab_h = base2$ingtot
    log_inglab_h<-ifelse((log_inglab_h)==0,1,log_inglab_h) #Reemplazar los salarios = 0 por 1
    log_inglab_h<- log(log_inglab_h)
    df18<- cbind(df18,log_inglab_h)
       
    #Estimar modelo
    regresion2<- lm(log_inglab_h~sex, data=df18)
    lm_summary2=as.data.frame(summary(regresion2)$coefficients)

    #Estimación incondicional modelo age-género
       
    regresion3<- lm(log_inglab_h~sex+age+age2, data=df18)
    lm_summary3=as.data.frame(summary(regresion3)$coefficients)
    y_predicho3<- predict(regresion3)
    base2<- cbind(df18, y_predicho3)
   
      #Peak age mujeres con modelo age 
      base_mujeres= subset(df18, base$sex==0)
    
      #Peak age hombres con modelo de age 
      base_hombres= subset(df18, base$sex==1)
      
     
      #Errores bootstrap modelo y~age+age2+sex
      eta_mod11<-rep(0,R)
      eta_mod22<-rep(0,R)
      eta_mod33<-rep(0,R)
      eta_mod44<-rep(0,R)
      
      set.seed(12345)
      n<- length(df18$)
      R<-1000
      y_predichos2<- matrix(NA,n,R)
      for (i in 1:R){
        db_sample<- sample_frac(df18,size=1, replace=TRUE)
        f2<- lm(log_inglab_h~sex+age+age2, db_sample)
        coefs2<- f2$coefficients
        
        eta_mod11[i]<- coefs2[1]
        eta_mod22[i]<- coefs2[2]
        eta_mod33[i]<- coefs2[3]
        eta_mod44[i]<- coefs2[4]
        
      } 
      for (i in 1:R){
        columnas2<- eta_mod11[i]+eta_mod22[i]*df18$sex+eta_mod33*base2$age+eta_mod44*df18$age2
        y_predichos2[,i]<-columnas2
      }
      ee2<-rowSds(y_predichos2)
      df22<-cbind(df18,ee2)
      IC_bajo2=df18$y_predicho3-1.96*ee2
      IC_alto2=df18$y_predicho3+1.96*ee2
      
      df18<- cbind(df18,IC_alto2, IC_bajo2)
      
      #Gráfico con modelo y=age+age2+sex divido entre el género por bootstrap
      ggplot(df18, aes(age, y_predicho3)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo2, ymax=IC_alto2), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")+facet_grid(sex~.)

      #Peak age hombres modelo y=age+age2+sex por bootstrap
      base_hombres2= subset(df18, base$sex==1)
      peak_hombre2<-(which.max(base_hombres$y_predicho3))
      peak_age_hombre2=(base_hombres$age[16])
      as.integer(max(base_hombres$ingtot))
      
      #Peak age mujeres modelo y=age+age2+sex por bootstrap
      base_mujeres2= subset(base2, base$sex==0)
      peak_mujeres2<-(which.max(base_mujeres$y_predicho3))
      peak_age_mujer2=(base_mujeres$age[23])
      as.integer(max(base_mujeres$ingtot))

      #Modelo con interacción age-sex para analisis de pendiente
      regresion4<- lm(log_inglab_h~sex+age+age2+sex:age, data=df18) 
      summary(regresionaux)
      
      stargazer(regresion4,regresion3, regresion2, regresion1,type="text")

      #Regresión condicional
      #Definir dummies como factores
      df18$maxEducLevel<-as.factor(df18$maxEducLevel)
      df18$estrato1<-as.factor(df18$estrato1)
      df18$regSalud <-as.factor(df18$regSalud)
      df18$cotPension<-as.factor(df18$cotPension)
      df18$sizeFirm<-as.factor(df18$sizeFirm)
      df18$oficio<-as.factor(df18$ofici)
      df18$informal<-as.factor(df18$informal)
      df18$relab<-as.factor(df18$relab)
       
      #Estimación modelo condicional
      regresion5<- lm(log_inglab_h~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension
                      +sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal
                      +relab, data=log_inglab_h)
      lm_summary5=as.data.frame(summary(regresion5$coefficients))
      
      
      #FWL
      df18<-df18 %>% mutate(res_y_a=lm(log_inglab_h~maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,df18)$residuals, #Residuales con ingreso
                              res_s_a=lm(sex~sex+maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,df18)$residuals, #Residuales con género
      )
      
      regresion6<-lm(res_y_a~res_s_a-1,df18)
      
      #Boostrap FWL
      
      set.seed(12345)
      eta.fn<-function(df18,i){
        coef(lm(res_y_a~res_s_a-1,df18, subset = i))
      }
      replicas<- boot(data = df18, statistic = eta.fn, R = 1000)
      
      
      stargazer(rregresion3, regresion5, regresion6,type="text")

       
#PUNTO 5: Predicting earnings
       
# a)
       
#Training(70%) y testing (30%)

sample <- sample(c(TRUE, FALSE), nrow(df18), replace=TRUE, prob=c(0.7,0.3))
train  <- df18[sample, ]
test   <- df18[!sample, ]
set.seed(10101)


#5b

#1
especificacion1 <-lm(y_ingLab_m_ha~age+sex+sex:age,data=train)
test$especificacion1<-predict(especificacion1,newdata = test)
#MSE especificación 1
with(test,mean((y_ingLab_m_ha-especificacion1)^2))

#2
especificacion2 <-lm(y_ingLab_m_ha~sex+maxEducLevel+age+estrato1+regSalud+cotPension+sizeFirm+oficio+informal+relab,data=train)
test$especificacion2<-predict(especificacion2,newdata = test)
#MSE especificación 2
with(test,mean((y_ingLab_m_ha-especificacion2)^2))

#3
especificacion3 <-lm(y_ingLab_m_ha~sex+maxEducLevel+age+estrato1+regSalud+cotPension+
                       sizeFirm+oficio+informal+relab+ sex:maxEducLevel+ sex:age+sex:oficio+sex:informal,data=train)
test$especificacion3<-predict(especificacion3,newdata = test)
#MSE especificación 3
with(test,mean((y_ingLab_m_ha-especificacion3)^2))

#4
especificacion4 <-lm(y_ingLab_m_ha~sex+maxEducLevel+age+estrato1+regSalud+cotPension+
                       sizeFirm+oficio+informal+relab+ sex:maxEducLevel +sex:age+sex:informal: maxEducLevel, data=train)
test$especificacion4<-predict(especificacion4,newdata = test)
#MSE especificación 4
with(test,mean((y_ingLab_m_ha-especificacion4)^2))

#5
especificacion5= lm(y_ingLab_m_ha~age, data=train) 
summary(especificacion5)
#MSE especificación 5
test$especificacion5<-predict(especificacion5,newdata = test)
with(test,mean((y_ingLab_m_ha-especificacion5)^2))