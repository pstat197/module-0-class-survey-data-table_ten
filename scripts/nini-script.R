# Loading libraries and dataset
library(tidyverse)

background <- read_csv("data/background-clean.csv")
interest   <- read_csv("data/interest-clean.csv")

# Inner join by response ID
data <- inner_join(background, interest, by = "response_id")

# Only select needed columns
data <- data %>%
  select(lang, prog.prof, prog.comf, math.prof, math.comf)

library(dplyr)

# Turn text proficiency levels into numbers
data <- data %>%
  mutate(
    prog.prof.num = recode(prog.prof, "beg" = 1, "int" = 2, "adv" = 3
    ),
    math.prof.num = recode(math.prof, "beg" = 1, "int" = 2, "adv" = 3
    )
  )

# Pivot longer
data_long <- data %>%
  pivot_longer(
    cols = c(prog.prof.num, prog.comf, math.prof.num, math.comf),
    names_to = "skill_type",
    values_to = "score"
  ) %>%
  mutate(
    skill_type = recode(skill_type,
                        "prog.prof.num" = "Programming Proficiency",
                        "prog.comf" = "Programming Comfort",
                        "math.prof.num" = "Math Proficiency",
                        "math.comf" = "Math Comfort")
  )


# Run ANOVA: Does comfort/proficiency differ by language?
anova_result <- aov(score ~ lang * skill_type, data = data_long)
summary(anova_result)

# Visualize mean differences
ggplot(data_long, aes(x = lang, y = score, fill = skill_type)) +
  geom_boxplot() +
  labs(
    title = "Comfort and Proficiency by Language Preference",
    x = "Language Preference",
    y = "Score (1–5 scale or recoded proficiency)",
    fill = "Skill Type"
  ) +
  theme_minimal()
