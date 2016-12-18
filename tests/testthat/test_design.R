#  Test design methods 
#  Package SynergyScreen
#  Yury V Bukhman, 07 July 2015
library(SynergyScreen)

#  Test that design of an 8-compound screen with exponential dose series is generated the same way as before
# - generate the new design
compounds = readCompoundFile(system.file("extdata/8_compounds/8_compounds.csv",package="SynergyScreen"))
screen = new("SynergyScreen", compound_list=compounds)
screen = generateDesign(screen, type="12 doses exp")

# - compare to the old design
old.design = read.csv(system.file("extdata/8_cpds_2/8_compounds_design.csv",package="SynergyScreen"),
                      as.is=T)
old.design$row = factor(old.design$row)
old.design = as(old.design,"ScreenDesign")
test_that("design of an 8-compound screen with exp dose series is generated the same way as before",{
  expect_true(all.equal(design(screen),old.design))
})

# - compare to the old design summary
old.design.sum = read.csv(system.file("extdata/8_cpds_2/8_compounds_2_design_summary.csv",
                                  package="SynergyScreen"), as.is = T, row.names = 1)
test_that("design of an 8-compound screen with exp dose series is generated the same way as before",{
  expect_true(all.equal(designSummary(screen),old.design.sum))
})