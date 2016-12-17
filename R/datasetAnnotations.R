#  Dataset annotations
#  Package SynergyScreen
#  Yury V Bukhman, 05 June 2015

#' Simulated screen of 15 compounds and their pairwise combinations.
#'
#' This dataset is the result of a complete simulation of a 15-compound screen, with data normalization and synergy computation.  
#' Dose-response experiments included 5 doses, logarithmically spaced.  
#' To generate this kind of dataset, run \code{source(system.file("extdata/15_cpds_simulation/15_cpds_simulation.R",package="SynergyScreen"))}.
#' Since generating this dataset includes an element of randomness, do not expect identical results.
#'
#' @format Object of class \linkS4class{SynergyScreen}
"sim15_screen3"