#documentation_test.r
#
source("R/FSDM.R")

#Look at causal effect as a function of anterior concept value
AntValues_ <- 1:99
PostRates_ <- sapply(AntValues_, function(x) {
  calcPosteriorRatio(x, 1.01, 50, 1)
}) - 1
plot(AntValues_, PostRates_, type = "l")

#Look at the sensitivity of change of posterior concept as function of value
PostValues_ <- 1:99
PostRates_ <- sapply(PostValues_, function(x) {
  calcPosteriorRatio(50, 1.01, x, 1)
}) - 1
plot(PostValues_, PostRates_, type = "l")
PostRates_[PostValues_ == 49]

#Show that causality relationships are symmetrical
library(jsonlite)
M <- createFuzzyModel("inst/models/ThisThat")
S <- createFuzzyScenario("inst/models/ThisThat/scenarios/Test1", M)
ThisThat_mx <- runFuzzyModel(M, S, "Linear")$ScaledSummary
#Show that they grow together
plot(1:11, ThisThat_mx[,"This"], pch = 1, type = "b", col = "red")
points(1:11, ThisThat_mx[,"That"] - 0.6, pch = 4, type = "b")
#Show that inverse relationship is correct
plot(1:11, ThisThat_mx[,"This2"], pch = 1, type = "b", col = "red")
points(1:11, ThisThat_mx[,"That4"] - 0.6, pch = 4, type = "b")
#Show that inverse relationship is correct
plot(1:11, ThisThat_mx[,"This3"], pch = 1, type = "b", col = "red")
points(1:11, ThisThat_mx[,"That5"] - 0.6, pch = 4, type = "b")
#Show that two moderate interactions add properly
plot(1:11, ThisThat_mx[,"This"], pch = 1, type = "b", col = "red")
plot(1:11, ThisThat_mx[,"This4"], pch = 1, type = "b", col = "red")
points(1:11, ThisThat_mx[,"That6"] - 0.6, pch = 4, type = "b")
#Show that two moderate interactions add properly
plot(1:11, ThisThat_mx[,"This"], pch = 2, type = "b", col = "red")
points(1:11, ThisThat_mx[,"This2"] - 0.6, pch = 1, type = "b", col = "blue")
points(1:11, ThisThat_mx[,"That3"] - 0.6, pch = 4, type = "b")
