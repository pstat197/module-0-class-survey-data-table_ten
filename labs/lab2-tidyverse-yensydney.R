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

#proficiency ratings of all respondents with research experience and 6-8 upper division courses
background %>% filter(rsrch == TRUE & updv.num == "6-8") %>% select(prog.prof, math.prof, stat.prof)

#proficiency ratings of all respondents without research experience and the same number of upper division courses
background %>% filter(rsrch == FALSE & updv.num == "6-8") %>% select(prog.prof, math.prof, stat.prof)

#median comfort level of all students in each subject area.
background %>% select(contains(".comf")) %>% summarize_all(.funs = 'median')

#median comfort level of all students in each subject area after grouping by number of upper division classes taken
background %>% group_by(updv.num) %>% select(contains(".comf")) %>% summarize_all(.funs = 'median')
