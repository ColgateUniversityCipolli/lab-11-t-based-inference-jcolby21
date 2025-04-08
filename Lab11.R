#Question 1

library(pwr)
result <- pwr.t.test(d = 0.65, sig.level = 0.05, power = 0.80, type = "one.sample")
print(result)
#It would take 21 observations to detect moderate-to-large effect d=0.65