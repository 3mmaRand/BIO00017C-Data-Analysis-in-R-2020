library(readxl)
library(tidyverse)

file <- "data/seeds-dataset.xlsx"
excel_sheets(file)
seeds <- read_excel(file, sheet = "seeds_dataset")

ggplot(data = seeds, aes(x = compactness, y = kernel_width)) +
  geom_point()


cor.test(data = seeds, ~ compactness + kernel_width)

df <- data.frame(x1 = rnorm(10000, 10), x2 = rnorm(10000,10))

ggplot(data = df, aes(x = x1, y = x2)) +
  geom_point()


cor.test(data = df, ~ x1 + x2)

shrimp <- read_table2("data/shrimp.txt")

ggplot(data = shrimp, aes(x = temperature, y = respiration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  them
