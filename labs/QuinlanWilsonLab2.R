library(tidyverse)

# retrieve class survey data
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab2-tidyverse/data/'

background <- paste(url, 'background-clean.csv', sep = '') %>%
  read_csv()

interest <- paste(url, 'interest-clean.csv', sep = '') %>%
  read_csv()

metadata <- paste(url, 'survey-metadata.csv', sep = '') %>%
  read_csv()

# filter rows
background %>%
  filter(math.comf > 3)

# select a column
background %>%
  select(math.comf)

# pull a column
background %>%
  pull(rsrch)

# define a new variable
background %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3)

# sequence of verbs
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch) 

background %>% 
  filter(rsrch == TRUE, updv.num == '6-8') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch, updv.num)

background %>%
  filter(rsrch == FALSE) %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch, updv.num)

background %>%
  select(contains('.comf')) %>%
  summarize_all(.funs = median)

background %>%
  group_by(updv.num) %>%
  select(contains('.comf')) %>%
  summarize_all(.funs = median)