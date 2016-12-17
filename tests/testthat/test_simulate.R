#  Test simulate methods 
#  Package SynergyScreen
#  Yury V Bukhman, 03 June 2015
library(SynergyScreen)

#  Load a set of compounds, initialize a SynergyScreen object, and generate a design
cpds3 = readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_3.csv",package="SynergyScreen"))
screen3 = new("SynergyScreen",compound_list=cpds3)
screen3 = generateDesign(screen3)

#  Simulate dose-response characteristics of mixtures and dose-response data
screen3 = simulate(screen3)

#  Test that compounds have been simulated correctly
test_that("compounds have been simulated correctly", {
  expect_equal(length(compound_list(screen3)),15)
  expect_is(compound(screen3,"Cpd3")@ic50,"numeric")
  expect_true(compound(screen3,"Cpd5")@m > 0)
})
#  Test that design has been simulated correctly
test_that("design has been simulated correctly", {  
  expect_is(design(screen3),"data.frame")
  expect_equal(dim(design(screen3)),c(1152,8))
})
#  Test that raw data have been simulated correctly
test_that("raw data have been simulated correctly", {
  expect_is(raw_data(screen3),"data.frame")
  expect_equal(dim(raw_data(screen3)),c(1152,4))
  expect_true(min(raw_data(screen3)$response) > 0)
  expect_true(max(raw_data(screen3)$response) < 2)
})
#  Test that dose-response experiments have been simulated correctly
test_that("dose-response experiments have been simulated correctly", {  
  expect_equal(length(dre_list(screen3)),120)
  expect_is(dre(screen3,"Cpd13-Cpd15")@fraction,"numeric")
  expect_equal(sum(dre(screen3,"Cpd13-Cpd15")@fraction),1)
  expect_equal(length(dre(screen3,"Cpd13-Cpd15")@dose),5)
})
#  Test that synergy experiments have been set up correctly
test_that("synergy experiments have been set up correctly", {    
  expect_equal(length(synergy_experiment_list(screen3)),105)
  expect_equal(synergy_experiment(screen3,"Cpd5-Cpd6")@compound.dre.name,c("Cpd5","Cpd6"))
})
#  Test that no normalization and synergy computation has been done yet
test_that("no normalization and synergy computation has been done yet", {
  expect_equal(nrow(norm_data(screen3)),0)
  expect_equal(nrow(synergy_data(screen3)),0)
})
