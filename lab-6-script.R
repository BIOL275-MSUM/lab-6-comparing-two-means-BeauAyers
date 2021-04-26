
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
