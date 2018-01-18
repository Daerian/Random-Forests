library(tidyverse)

winequality <- read_delim('db/winequality-red.csv', delim=";")
winequality$quality <- as.factor(winequality$quality)