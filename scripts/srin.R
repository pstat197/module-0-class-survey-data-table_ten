library(tidyverse)
library(ggplot2)

background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')

all_data <- background %>% inner_join(interest, by = join_by(response_id)) %>% rename(c(domain_yn = dom.x, domain_type = dom.y, research_or_industry = do_you_have_any_preference_regarding_working_on_an_industry_project_or_a_research_lab_project))

all_data$lang[all_data$response_id == 29] <- "Python"

prop.table(table(all_data$lang))

lang_summary <- all_data %>%
  count(lang) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

ggplot(lang_summary, aes(x = reorder(lang, -prop), y = prop, fill = lang)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportions of Language Preference",
    x = "Preferred Language",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

table(all_data$type, all_data$lang)

ggplot(all_data, aes(x = type, fill = lang)) +
  geom_bar(position = "fill") +
  labs(
    title = "Language Preference by Project Type",
    x = "Preferred Project Type (Industry / Lab / Both)",
    y = "Proportion",
    fill = "Preferred Language"
  ) +
  scale_y_continuous(labels = scales::percent)

ggplot(all_data, aes(x = lang, fill = type)) +
  geom_bar(position = "fill") +
  labs(
    title = "Project Type Preference by Language Preference",
    x = "Preferred Language",
    y = "Proportion",
    fill = "Preferred Language"
  ) +
  scale_y_continuous(labels = scales::percent)
