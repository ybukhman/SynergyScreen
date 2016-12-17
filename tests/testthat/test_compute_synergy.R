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

#  Test that the results of the new computations are the same as the old ones
test_that("the results of the new computations are the same as the old ones", {
  expect_equal(synergy_data(new_screen3),synergy_data(sim15_screen3))
})

