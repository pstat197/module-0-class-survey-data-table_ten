library(tidyverse)

# retrieve class survey data
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab2-tidyverse/data/'

background <- paste(url, 'background-clean.csv', sep = '') %>%
  read_csv()

interest <- paste(url, 'interest-clean.csv', sep = '') %>%
  read_csv()

metadata <- paste(url, 'survey-metadata.csv', sep = '') %>%
  read_csv()

view(background)
view(interest)

# Gathering simple 1 var summary
interest %>%
  count(lang) %>%
  arrange(desc(n)) %>%
  mutate("prop" = n / sum(n))

# Gathering ids for R users
R_ids = interest %>%
  filter(lang == "R") %>%
  select(response_id)

R_ids

# Gathering ids for Python users
Python_ids = interest %>%
  filter(lang == "Python") %>%
  select(response_id)

Python_ids

# Get background information on all individuals who prefer R
R_background = background %>%
  filter(response_id %in% R_ids$response_id)

R_background

# Get background information on all individuals who prefer R
Python_background = background %>%
  filter(response_id %in% Python_ids$response_id)

Python_background

# R research proportions
R_rsrch_prop = R_background %>%
  select(rsrch) %>%
  count(rsrch) %>%
  arrange(desc(n)) %>%
  mutate("prop" = n / sum(n))

R_rsrch_prop

# Python research proportions
Python_rsrch_prop = Python_background %>%
  select(rsrch) %>%
  count(rsrch) %>%
  arrange(desc(n)) %>%
  mutate("prop" = n / sum(n))

Python_rsrch_prop