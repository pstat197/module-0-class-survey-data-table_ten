library(dplyr)

background <- read.csv('data/background-clean.csv')
interest <- read.csv('data/interest-clean.csv')
metadata <- read.csv('data/survey-metadata.csv')

data <- inner_join(background, interest, by="response_id")
head(data)
colnames(data)

data <- data %>%
  mutate(prefers_R = ifelse(lang == "R", 1, 0))

manova_result <- manova(cbind(prog.prof, prog.comf, math.prof, math.comf) ~ lang, data = data)
summary(manova_result)


