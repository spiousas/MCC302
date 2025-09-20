library(palmerpenguins)
library(tidyverse)

table(penguins$species)

penguins_adelie <- penguins %>%
  filter(species=="Adelie")
head(penguins_adelie)

penguins_adelie %>% ggplot(aes(x = bill_depth_mm,
                               y = bill_length_mm)) +
  geom_point()


penguins_adelie %>% ggplot(aes(x = bill_depth_mm,
                               y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")
