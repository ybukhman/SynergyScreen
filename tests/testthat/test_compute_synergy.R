#  Test synergy computation method
#  Package SynergyScreen
#  Yury V Bukhman, 08 June 2015
library(SynergyScreen)

#  Load the data
data("sim15_screen3")

#  Transfer data to a new screen object
new_screen3 = new("SynergyScreen")
compound_list(new_screen3) = compound_list(sim15_screen3)
design(new_screen3) = design(sim15_screen3)
raw_data(new_screen3) = raw_data(sim15_screen3)
dre_list(new_screen3) = dre_list(sim15_screen3)
synergy_experiment_list(new_screen3) = synergy_experiment_list(sim15_screen3)

#  Normalize the data and compute synergies
new_screen3 = normalize(new_screen3)
new_screen3 = computeSynergies(new_screen3)

#  Test that the results of the new computations are close enough to the old ones
test_that("the results of the new computations are close enough to the old ones", {
  data1 <- cbind(synergy_data(new_screen3),synergy_data(sim15_screen3))
  expect_true(cor(log2(data1[,8]), log2(data1[,4]), use="complete.obs") > 0.95)
  expect_true(sum(is.na(data1[,4]) != is.na(data1[,8]))/nrow(data1) < 0.05)
  lm1 <- lm(log2(data1[,4]) ~ log2(data1[,8]))
  expect_true(abs(coef(lm1)[1]) < 0.1)
  expect_true(abs(coef(lm1)[2]-1) < 0.1)
})

