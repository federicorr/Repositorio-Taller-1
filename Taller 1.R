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
p_load(readr, rvest, tidyverse, dplyr)

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
sapply(df18, function(x) sum(is.na(x))
       
#5A
#Training(70%) y testing (30%)

id_train <- sample (1:nrow(df), size = 0.7*nrow(df), replace =F) 
train <- df["id_train"]
set.seed(10101)

id_test <- sample (1:nrow(df), size = 0.3*nrow(df), replace =F) 
test <- df["id_test"]
set.seed(10101)

