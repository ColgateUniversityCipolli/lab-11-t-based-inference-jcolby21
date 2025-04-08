#Question 1

library(pwr)
result <- pwr.t.test(d = 0.65, sig.level = 0.05, power = 0.80, type = "one.sample")
print(result)
#It would take 21 observations to detect moderate-to-large effect d=0.65


#Question 2
library(tidyverse)
library(readxl)
finch.dat <- read_excel("Lab11data.xlsx")

finch.tib = tibble(finch.dat) |>
  mutate(diff_vals=closer_vals - further_vals)
