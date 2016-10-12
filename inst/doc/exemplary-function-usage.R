## ---- results='hide'-----------------------------------------------------
library(oddsratio)
suppressPackageStartupMessages(library(mgcv))
set.seed(1234)
n <- 200
sig <- 2
dat <- suppressMessages(gamSim(1, n = n,scale = sig))
dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))

fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x2", values = c(0.099, 0.198))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x4", values = c("A", "B"))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                   percentage = 20, slice = TRUE)

## ------------------------------------------------------------------------
dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dat$rank <- factor(dat$rank)
fit.glm <- glm(admit ~ gre + gpa + rank, data = dat, family = "binomial")

## ------------------------------------------------------------------------
calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))

