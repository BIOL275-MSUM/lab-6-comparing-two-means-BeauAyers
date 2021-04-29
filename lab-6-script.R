
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

# do stuff ----------------------------------------------------------------

ttest_results <- t.test(formula = species ~ location, data = fish_long)

ttest_results

diff(ttest_results$estimate)


# Creating Graphs ---------------------------------------------------------

fish_sum <-
  fish_long %>%
  group_by(location) %>%
  summarize(
    sampl_size = n(),
    mean = mean(species),
    str_dev = sd(species),
    var = var(species),
    sem = sd(species) / sqrt(n()),
    ci_upper = mean + 2 * sem,
    ci_lower = mean - 2 * sem,
  )

fish_sum

ggplot(data = fish_long) +
  geom_jitter(mapping = aes(x = location, y = species)) +
  geom_crossbar(
    data = fish_sum,
    mapping = aes(x = location, y = mean, ymax = ci_upper, ymin = ci_lower),
    color = "cyan3"
  )

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange", "darkorchid")) +
  theme_minimal()


# Crabs -------------------------------------------------------------------

crabs <- read_csv("chap15q27FiddlerCrabFans.csv")


# Code for crab graphs ----------------------------------------------------

crab_sum <-
  crabs %>%
  group_by(crabType) %>%
  summarize(
    sampl_size = n(),
    mean = mean(bodyTemp),
    str_dev = sd(bodyTemp),
    var = var(bodyTemp),
    sem = sd(bodyTemp) / sqrt(n()),
    ci_upper = mean + 2 * sem,
    ci_lower = mean - 2 * sem,
  )

crab_sum

ggplot(data = crabs) +
  geom_jitter(mapping = aes(x = crabType, y = bodyTemp)) +
  geom_crossbar(
    data = crab_sum,
    mapping = aes(x = crabType, y = mean, ymax = ci_upper, ymin = ci_lower),
    color = "cyan3"
  )

crabs %>% 
  ggplot(aes(x = bodyTemp)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4", "#C24641")) +
  theme_minimal()


# ANOVA crabs -------------------------------------------------------------

aov_crabs <-
  aov(bodyTemp ~ crabType, data = crabs)
aov_crabs

summary(aov_crabs)
