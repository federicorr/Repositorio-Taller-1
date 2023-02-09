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

