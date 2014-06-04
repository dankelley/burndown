library(burndown);
b <- read.burndown("/Users/kelley/src/R-kelley/burndown/data/burndown.dat")
#data(burndown)
summary(b)
plot(b)
