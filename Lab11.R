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
                 breaks=seq(-1,0.2, length.out=15),
                 color="grey30", fill="lightgray")+             # Specify bins
  geom_density(aes(x=further_vals), color="darkred")+             # Add density estimate
  geom_hline(yintercept=0)+                                   # Add x-axis
  theme_bw() +                                                # Remove gray background
  xlab("Further Values") +                                  # x-axis label
  ylab("Density")                                             # y-axis label



#B)Closer Data
finch.closer= finch.tib |>
  summarize(mean       = mean(finch.tib$closer_vals),
            sd         = sd(finch.tib$closer_vals),
            median     = median(finch.tib$closer_vals),
            IQR        = IQR(finch.tib$closer_vals),
            skewness   = skewness(finch.tib$closer_vals),
            exkurtosis = kurtosis(finch.tib$closer_vals))

ggplot(data=finch.tib) +                                         # Specify data to plot
  geom_histogram(aes(x=closer_vals, y = after_stat(density)), # Density histogram
                 breaks=seq(-0.1,0.4, length.out=10),
                 color="grey30", fill="lightgray")+             # Specify bins
  geom_density(aes(x=closer_vals), color="darkred")+             # Add density estimate
  geom_hline(yintercept=0)+                                   # Add x-axis
  theme_bw() +                                                # Remove gray background
  xlab("Closer Values") +                                  # x-axis label
  ylab("Density")                                             # y-axis label


#C) Difference in Data
finch.diff= finch.tib |>
  summarize(mean       = mean(finch.tib$diff_vals),
            sd         = sd(finch.tib$diff_vals),
            median     = median(finch.tib$diff_vals),
            IQR        = IQR(finch.tib$diff_vals),
            skewness   = skewness(finch.tib$diff_vals),
            exkurtosis = kurtosis(finch.tib$diff_vals))

ggplot(data=finch.tib) +                                         # Specify data to plot
  geom_histogram(aes(x=diff_vals, y = after_stat(density)), # Density histogram
                 breaks=seq(-0.1,1.1, length.out=15),
                 color="grey30", fill="lightgray")+             # Specify bins
  geom_density(aes(x=diff_vals), color="darkred")+             # Add density estimate
  geom_hline(yintercept=0)+                                   # Add x-axis
  theme_bw() +                                                # Remove gray background
  xlab("Difference in Values") +                                  # x-axis label
  ylab("Density")                                             # y-axis label



#Question 4 
#A)Closer Values
library(effectsize)
hedges_g(x = finch.tib$closer_vals, mu = 0, alternative = "greater")
t.test(x=finch.tib$closer_vals, mu = 0, alternative = "greater")
#(t=8.3024, p=8.132 x 10^-8, g= 1.61, 95% CI: 1.10, Inf)


#B)Further Values
hedges_g(x = finch.tib$further_vals, mu = 0, alternative = "less")
t.test(x=finch.tib$further_vals, mu = 0, alternative = "less")
#(t=-7.778, p=2.587 x 10^-8, g= -1.51, 95% CI: -Inf, -1.02)

#C)Diff Values
hedges_g(x = finch.tib$diff_vals, mu = 0, alternative = "two.sided")
t.test(x=finch.tib$diff_vals, mu = 0, alternative = "two.sided")
#(t=8.5109, p=1.037 x 10^-8, g= 1.65, 95% CI: 1.04, 2.24)


#Question 5
# A) Closer Values
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000)) |>
  mutate(pdf.null = dt(t, df=length(finch.tib$closer_vals)-1))

# For plotting the observed point
ggdat.obs <- tibble(t = 8.3024, y = 0)  # observed t-statistic from t-test

# Resampling to approximate the sampling distribution
R <- 1000
resamples <- tibble(t = numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=finch.tib$closer_vals, size=length(finch.tib$closer_vals), replace=T)
  resamples$t[i] = (mean(curr.sample)-0)/(sd(curr.sample)/sqrt(length(finch.tib$closer_vals)))
}

# Critical value for one-sided test
t.crit <- qt(0.975, df=length(finch.tib$closer_vals)-1)

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, aes(x=t, y=pdf.null)) +
  geom_hline(yintercept=0) +
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t >= t.crit), aes(x=t, ymin=0, ymax=pdf.null), fill="grey", alpha=0.5) +
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red") +
  # Resampling Distribution
  stat_density(data=resamples, aes(x=t), geom="line", color="grey") +
  # clean up aesthetics
  theme_bw() +
  scale_x_continuous("t",
                     breaks = round(c(-5, t.crit, 5),2)) +
  ylab("Density") +
  ggtitle("One-Sided T-Test for Closer Values",
          subtitle="H0: mu = 0; Ha: mu > 0")

# B) Further Values
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000)) |>
  mutate(pdf.null = dt(t, df=length(finch.tib$further_vals)-1))

# For plotting the observed point
ggdat.obs <- tibble(t = -7.778, y = 0)  # observed t-statistic from t-test

# Resampling to approximate the sampling distribution
R <- 1000
resamples <- tibble(t = numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=finch.tib$further_vals, size=length(finch.tib$further_vals), replace=T)
  resamples$t[i] = (mean(curr.sample)-0)/(sd(curr.sample)/sqrt(length(finch.tib$further_vals)))
}

# Critical value for one-sided test
t.crit <- qt(0.025, df=length(finch.tib$further_vals)-1)

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, aes(x=t, y=pdf.null)) +
  geom_hline(yintercept=0) +
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t <= t.crit), aes(x=t, ymin=0, ymax=pdf.null), fill="grey", alpha=0.5) +
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red") +
  # Resampling Distribution
  stat_density(data=resamples, aes(x=t), geom="line", color="grey") +
  # clean up aesthetics
  theme_bw() +
  scale_x_continuous("t",
                     breaks = round(c(-5, t.crit, 5),2)) +
  ylab("Density") +
  ggtitle("One-Sided T-Test for Further Values",
          subtitle="H0: mu = 0; Ha: mu < 0")

# C) Difference in Values 
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000)) |>
  mutate(pdf.null = dt(t, df=length(finch.tib$diff_vals)-1))

# For plotting the observed point
ggdat.obs <- tibble(t = 8.5109, y = 0)  # observed t-statistic from t-test

# Resampling to approximate the sampling distribution
R <- 1000
resamples <- tibble(t = numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=finch.tib$diff_vals, size=length(finch.tib$diff_vals), replace=T)
  resamples$t[i] = (mean(curr.sample)-0)/(sd(curr.sample)/sqrt(length(finch.tib$diff_vals)))
}

# Critical values for two-sided test
t.crit.left <- qt(0.025, df=length(finch.tib$diff_vals)-1)
t.crit.right <- qt(0.975, df=length(finch.tib$diff_vals)-1)

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, aes(x=t, y=pdf.null)) +
  geom_hline(yintercept=0) +
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t <= t.crit.left), aes(x=t, ymin=0, ymax=pdf.null), fill="grey", alpha=0.5) +
  geom_ribbon(data=subset(ggdat.t, t >= t.crit.right), aes(x=t, ymin=0, ymax=pdf.null), fill="grey", alpha=0.5) +
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red") +
  # Resampling Distribution
  stat_density(data=resamples, aes(x=t), geom="line", color="grey") +
  # clean up aesthetics
  theme_bw() +
  scale_x_continuous("t",
                     breaks = round(c(-5, t.crit.left, t.crit.right, 5),2)) +
  ylab("Density") +
  ggtitle("Two-Sided T-Test for Diff Values",
          subtitle="H0: mu = 0; Ha: mu != 0")
