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

#Question 3
#A)Further Data
library(e1071)
finch.further= finch.tib |>
  summarize(mean       = mean(finch.tib$further_vals),
            sd         = sd(finch.tib$further_vals),
            median     = median(finch.tib$further_vals),
            IQR        = IQR(finch.tib$further_vals),
            skewness   = skewness(finch.tib$further_vals),
            exkurtosis = kurtosis(finch.tib$further_vals))


ggplot(data=finch.tib) +                                         # Specify data to plot
  geom_histogram(aes(x=further_vals, y = after_stat(density)), # Density histogram
                 breaks=seq(-1,0.2, length.out=20),
                 color="grey30", fill="lightgray")+             # Specify bins
  geom_density(aes(x=further_vals), color="darkred")+             # Add density estimate
  geom_hline(yintercept=0)+                                   # Add x-axis
  theme_bw() +                                                # Remove gray background
  xlab("Further Values") +                   # x-axis label
  ylab("Density")                                             # y-axis label


#B)Closer Data
finch.closer= finch.tib |>
  summarize(mean       = mean(finch.tib$closer_vals),
            sd         = sd(finch.tib$closer_vals),
            median     = median(finch.tib$closer_vals),
            IQR        = IQR(finch.tib$closer_vals),
            skewness   = skewness(finch.tib$closer_vals),
            exkurtosis = kurtosis(finch.tib$closer_vals))

#C) Difference in Data
finch.diff= finch.tib |>
  summarize(mean       = mean(finch.tib$diff_vals),
            sd         = sd(finch.tib$diff_vals),
            median     = median(finch.tib$diff_vals),
            IQR        = IQR(finch.tib$diff_vals),
            skewness   = skewness(finch.tib$diff_vals),
            exkurtosis = kurtosis(finch.tib$diff_vals))
