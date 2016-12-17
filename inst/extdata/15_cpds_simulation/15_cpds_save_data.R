#  Save datasets from 15-compound simulation as reference for testing and examples 
#  Package SynergyScreen
#  Yury V Bukhman, 03 June 2015

sim15_screen1 = screen1
sim15_screen2 = screen2
sim15_screen3 = screen3
save(sim15_screen1,sim15_screen2,sim15_screen3,file="15_cpds_sim_results.RData", compress='xz')
save(sim15_screen3,file="sim15_screen3.RData", compress='xz')
