library(tidyverse)

background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')


column_names <- names(background)
print(column_names)
