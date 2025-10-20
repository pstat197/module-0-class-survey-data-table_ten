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

# Students with research experience and 6–8 upper-division courses
research_6to8 <- background %>%
  filter(rsrch == TRUE & updv.num == "6-8") %>%
  select(prog.prof, math.prof, stat.prof) %>%
  rename(
    Programming_Proficiency = prog.prof,
    Math_Proficiency = math.prof,
    Statistics_Proficiency = stat.prof
  )

# Students without research experience and 6–8 upper-division courses
no_research_6to8 <- background %>%
  filter(rsrch == FALSE & updv.num == "6-8") %>%
  select(prog.prof, math.prof, stat.prof) %>%
  rename(
    Programming_Proficiency = prog.prof,
    Math_Proficiency = math.prof,
    Statistics_Proficiency = stat.prof
  )

# Median comfort scores for all students
median_comfort <- background %>%
  select(contains(".comf")) %>%
  rename(
    Programming_Comfort = prog.comf,
    Math_Comfort = math.comf,
    Statistics_Comfort = stat.comf
  ) %>%
  summarize_all(.funs = median)

# Median comfort scores by number of upper-division courses
median_comfort_by_updv <- background %>%
  group_by(updv.num) %>%
  select(contains(".comf")) %>%
  rename(
    Programming_Comfort = prog.comf,
    Math_Comfort = math.comf,
    Statistics_Comfort = stat.comf
  ) %>%
  summarize_all(.funs = median)

