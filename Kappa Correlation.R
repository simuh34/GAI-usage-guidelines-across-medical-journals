library(irr)
library(readxl)

c1 <- read_excel("Kappa correlation.xlsx", sheet = 1)
c2 <- read_excel("Kappa correlation.xlsx", sheet = 2)

kappa_results <- data.frame('Author Guideline' = 1:8, Kappa = numeric(8))
kappa_results <- data.frame('Author Guideline' = 1:8, Kappa = numeric(8))

for (i in 1:8) {
  kappa_results$Kappa[i] <- kappa2(data.frame(c1[, i], c2[, i]))$value
}
print(kappa_results)

