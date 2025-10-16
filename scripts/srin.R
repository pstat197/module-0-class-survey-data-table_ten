library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')

all_data <- background %>% inner_join(interest, by = join_by(response_id)) %>% rename(c(domain_yn = dom.x, domain_type = dom.y, research_or_industry = do_you_have_any_preference_regarding_working_on_an_industry_project_or_a_research_lab_project))

all_data$lang[all_data$response_id == 29] <- "Python"

# Proportion Table Of People who Prefer to Use R, Python, No Pref
prop.table(table(all_data$lang))



lang_summary <- all_data %>%
  count(lang) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

## Bar Chart Of Language Preference w/ Percents

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

plot_data <- all_data %>%
  count(lang, type) %>%
  group_by(lang) %>%
  mutate(prop = n / sum(n))

# Distribution of those who prefer each type of Project
# By Language Preference
ggplot(plot_data, aes(x = lang, y = prop, fill = type)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_fill(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Project Type Preference by Language Preference",
    x = "Preferred Language",
    y = "Proportion",
    fill = "Project Type Preference"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()
# Findings:
# Of the people that prefer to use Python, a majority of them (79%)
# would prefer to work on an industry project.
# 
# Those who prefer to use R

skill_data <- all_data %>%
  filter(lang %in% c("R", "Python")) %>%
  pivot_longer(
    cols = c(prog.prof, math.prof, stat.prof),
    names_to = "skill_type",
    values_to = "proficiency"
  ) %>%
  mutate(
    skill_type = case_when(
      skill_type == "prog.prof" ~ "Programming",
      skill_type == "math.prof" ~ "Math",
      skill_type == "stat.prof" ~ "Statistics"
    ),
    proficiency = factor(proficiency, levels = c("beg", "int", "adv"))
  )

# Proficiency of Language Preference (don't use)
ggplot(skill_data, aes(x = proficiency, fill = lang)) +
  geom_bar(position = "dodge") +
  facet_wrap(~skill_type) +
  labs(
    title = "Skill Proficiency by Language Preference",
    x = "Proficiency Level",
    y = "Count",
    fill = "Language"
  ) +
  theme_minimal()

## Average Comfort Level of Math, Prog, Stat based on 
## Language Preference
comfort_summary <- all_data %>%
  group_by(lang) %>%
  summarise(
    avg_prog_comfort = mean(prog.comf, na.rm = TRUE),
    avg_math_comfort = mean(math.comf, na.rm = TRUE),
    avg_stat_comfort = mean(stat.comf, na.rm = TRUE),
    n = n()
  ) %>%
  pivot_longer(
    cols = starts_with("avg"),
    names_to = "comfort_type",
    values_to = "avg_comfort"
  ) %>%
  mutate(comfort_type = str_remove(comfort_type, "avg_") %>% 
           str_remove("_comfort") %>% 
           str_to_title())

ggplot(comfort_summary, aes(x = comfort_type, y = avg_comfort, fill = lang)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(avg_comfort, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(
    title = "Average Comfort Level by Language Preference",
    subtitle = "Scale: 1-5",
    x = "Skill Type",
    y = "Average Comfort",
    fill = "Language"
  ) +
  ylim(0, 5.5) +
  theme_minimal()

# ===== Most Common Courses Taken By Language Preference
# Shows that people who are prefer Python have taken more
# CS upper divs, and those who prefer R have taken more
# Ling upper divs

course_cols <- c("PSTAT100", "PSTAT115", "PSTAT126", 
                 "PSTAT131", "PSTAT160", "PSTAT174", "CS9", "CS16", "LING104", 
                 "LING110", "LING111", "CS130", "CS165", "ECON145", "PSTAT127", 
                 "PSTAT134", "CS5")

course_summary <- all_data %>%
  filter(lang %in% c("R", "Python", "No preference")) %>%
  group_by(lang) %>%
  summarise(across(all_of(course_cols), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = -lang, names_to = "course", values_to = "count") %>%
  group_by(lang) %>%
  mutate(prop = count / sum(count))

top_courses <- course_summary %>%
  group_by(lang) %>%
  slice_max(count, n = 10) %>%
  ungroup()

ggplot(top_courses, aes(x = reorder(course, count), y = count, fill = lang)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.2, size = 3) +
  facet_wrap(~lang, scales = "free") +
  coord_flip() +
  labs(
    title = "Most Common Courses Taken by Language Preference",
    x = "Course",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# course groups from your list
course_cols <- c("PSTAT100", "PSTAT115", "PSTAT120", "PSTAT122", "PSTAT126", 
                 "PSTAT131", "PSTAT160", "PSTAT174", "CS9", "CS16", "LING104", 
                 "LING110", "LING111", "CS130", "CS165", "ECON145", "PSTAT127", 
                 "PSTAT134", "CS5")

pstat_courses <- c("PSTAT100", "PSTAT115", "PSTAT120", "PSTAT122", "PSTAT126", 
                   "PSTAT131", "PSTAT160", "PSTAT174", "PSTAT127", "PSTAT134")
cs_courses <- c("CS9", "CS16", "CS130", "CS165", "CS5")
ling_courses <- c("LING104", "LING110", "LING111")
econ_courses <- c("ECON145")

# average number of courses taken by language preference
# more CS courses -> Python
# more PSTAT, Ling --> R

course_averages <- all_data %>%
  filter(lang %in% c("R", "Python", "No preference")) %>%
  mutate(
    pstat_total = rowSums(select(., all_of(pstat_courses)), na.rm = TRUE),
    cs_total = rowSums(select(., all_of(cs_courses)), na.rm = TRUE),
    ling_total = rowSums(select(., all_of(ling_courses)), na.rm = TRUE),
    econ_total = rowSums(select(., all_of(econ_courses)), na.rm = TRUE)
  ) %>%
  group_by(lang) %>%
  summarise(
    n = n(),
    avg_pstat = mean(pstat_total, na.rm = TRUE),
    avg_cs = mean(cs_total, na.rm = TRUE),
    avg_ling = mean(ling_total, na.rm = TRUE),
    avg_econ = mean(econ_total, na.rm = TRUE),
    avg_total = mean(pstat_total + cs_total + ling_total + econ_total, na.rm = TRUE)
  )

print(course_averages)

# Visualize as a grouped bar chart
course_avg_long <- course_averages %>%
  select(lang, avg_pstat, avg_cs, avg_ling, avg_econ) %>%
  pivot_longer(cols = starts_with("avg_"), 
               names_to = "course_type", 
               values_to = "avg_courses") %>%
  mutate(course_type = case_when(
    course_type == "avg_pstat" ~ "PSTAT",
    course_type == "avg_cs" ~ "CS",
    course_type == "avg_ling" ~ "LING",
    course_type == "avg_econ" ~ "ECON"
  ))

ggplot(course_avg_long, aes(x = course_type, y = avg_courses, fill = lang)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(avg_courses, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Average Number of Courses Taken by Language Preference",
    x = "Course Department",
    y = "Average Number of Courses",
    fill = "Language Preference"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Courses by language
ggplot(course_avg_long, aes(x = course_type, y = avg_courses, fill = course_type)) +
  geom_col() +
  geom_text(aes(label = round(avg_courses, 2)), vjust = -0.5, size = 4) +
  facet_wrap(~lang) +
  labs(
    title = "Average Number of Courses Taken by Language Preference",
    x = "Course Department",
    y = "Average Number of Courses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# table with mean ± sd
course_detail <- all_data %>%
  filter(lang %in% c("R", "Python", "No preference")) %>%
  mutate(
    pstat_total = rowSums(select(., all_of(pstat_courses)), na.rm = TRUE),
    cs_total = rowSums(select(., all_of(cs_courses)), na.rm = TRUE),
    ling_total = rowSums(select(., all_of(ling_courses)), na.rm = TRUE),
    econ_total = rowSums(select(., all_of(econ_courses)), na.rm = TRUE)
  ) %>%
  group_by(lang) %>%
  summarise(
    n = n(),
    `PSTAT (mean ± sd)` = paste0(round(mean(pstat_total), 2), " ± ", round(sd(pstat_total), 2)),
    `CS (mean ± sd)` = paste0(round(mean(cs_total), 2), " ± ", round(sd(cs_total), 2)),
    `LING (mean ± sd)` = paste0(round(mean(ling_total), 2), " ± ", round(sd(ling_total), 2)),
    `ECON (mean ± sd)` = paste0(round(mean(econ_total), 2), " ± ", round(sd(econ_total), 2))
  )

print(course_detail)
