library(tidyverse)
# install.packages('infer') # execute once then comment out

# data location
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab3-iteration/data/biomarker-clean.csv'

# function for outlier trimming
trim_fn <- function(x){
  x[x > 3] <- 3
  x[x < -3] <- -3
  
  return(x)
}

# read in and preprocess data
asd <- read_csv(url) %>%
  select(-ados) %>%
  # log transform
  mutate(across(.cols = -group, log10)) %>%
  # center and scale
  mutate(across(.cols = -group, ~ scale(.x)[, 1])) %>%
  # trim outliers
  mutate(across(.cols = -group, trim_fn))

# The loop produces an NA because there are only 3 values in the vector that the loop is trying to iterate through 4 times. 


n_tests <- 50
rslt <- tibble(protein = colnames(asd)[2:(n_tests + 1)],
               p = NA,
               diff = NA)
for(i in 1:n_tests){
  x <- asd %>% filter(group == 'ASD') %>% pull(i + 1)
  y <- asd %>% filter(group == 'TD') %>% pull(i + 1)
  rslt$p[i] <- t.test(x, y, var.equal = F)$p.value
  rslt$diff[i] <- t.test(x, y, var.equal = F)$estimate[1] - t.test(x, y, var.equal = F)$estimate[2]
}
rslt

asd_list <- asd %>% 
  select(1:(n_tests + 1)) %>%
  pivot_longer(cols = -group,
               names_to = 'protein',
               values_to = 'level') %>%
  group_by(protein) %>%
  group_split()

tt_fn <- function(i) {
  test_rslt <- t.test(level ~ group, data = asd_list[[i]])
  
  diff <- diff(test_rslt$estimate)
  se <- (diff(test_rslt$conf.int) / (2 * qt(0.95, test_rslt$parameter)))
  out <- c(diff = diff, se = se)
  out
}

rslt <- sapply(1:n_tests, tt_fn) %>%
  t() %>%
  as_tibble() %>%
  mutate(protein = unique(map_chr(asd_list[1:n_tests], ~ unique(.x$protein))))

rslt

asd_nested <- asd %>%
  pivot_longer(-group, 
               names_to = 'protein', 
               values_to = 'level') %>%
  nest(data = c(level, group))

asd_nested %>% head(5)

asd_nested %>% 
  slice(1L) %>% 
  unnest(cols = data) %>% 
  infer::t_test(formula = level ~ group,
                order = c('ASD', 'TD'),
                alternative = 'two-sided',
                var.equal = F)

# wrapper around infer::t_test
tt_fn <- function(.df){
  infer::t_test(.df, 
                formula = level ~ group,
                order = c('ASD', 'TD'),
                alternative = 'two-sided',
                var.equal = F)
}

# compute test results
tt_out <- asd_nested %>%
  slice(1:n_tests) %>%
  mutate(ttest = map(data, tt_fn)) %>%
  unnest(ttest)

# bonferroni correction
tt_out <- tt_out %>% 
  mutate(rank = row_number(),
         m = n(),
         p_adj = p_value * m / rank) %>%
  mutate(p_adj = pmin(p_adj, 1))

sig_proteins <- tt_out %>%
  filter(p_adj < 0.01) %>%
  select(protein, p_value, p_adj, estimate)
sig_proteins

n_tests <- 1317

tt_out <- asd_nested %>%
  slice(1:n_tests) %>%
  mutate(ttest = map(data, tt_fn)) %>%
  unnest(ttest) %>%
  arrange(p_value) %>%
  mutate(rank = row_number(),
         m = n(),
         p_adj = p_value * m / rank,
         p_adj = pmin(p_adj, 1))

sig_proteins <- tt_out %>%
  filter(p_adj < 0.01) %>%
  select(protein, p_value, p_adj, estimate)

sig_proteins
