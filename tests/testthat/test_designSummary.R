#  Test designSummary method of SynergyScreen
#  Package SynergyScreen
#  Yury V Bukhman, 13 August 2015
library(SynergyScreen)

# initialize a new screen
compounds = readCompoundFile(system.file("extdata/8_compounds/8_compounds.csv",package="SynergyScreen"))
screen = new("SynergyScreen", compound_list=compounds)

# test designSummary method for an exponential design type
screen = generateDesign(screen, type="12 doses exp")
design_sum = designSummary(screen)
test_that("designSummary method returns a data frame of expected format", {
  expect_is(design_sum,"data.frame")
  expect_identical(row.names((design_sum)),names(dre_list(screen)))
  expect_identical(names(design_sum),c("plate","row","column","cpd1","cpd1_max_dose",
                                       "cpd2","cpd2_max_dose","dilution_factor"))
  expect_identical(rownames(design_sum),names(dre_list(screen)))
  expect_true(all(design_sum$cpd1 %in% names(compound_list(screen))))
  expect_is(design_sum$cpd1_max_dose, "numeric")
  expect_true(all(design_sum$cpd2 %in% c(names(compound_list(screen)),NA)))
  expect_is(design_sum$cpd2_max_dose, "numeric")
  expect_is(design_sum$dilution_factor, "numeric")
  expect_more_than(min(design_sum$dilution_factor),1)
})
