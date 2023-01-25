r <- readOGR(dsn = 'D:/Box Sync/DIFM/DIFM Data by Variable/2017/Past Prescription/ILSBarton-37/ILS-37 2016 NH3 Rx', 
             '37_U3_Strip_till_20170311')
unique(r@data$EC_Deep)
seed <- data.frame(EC_Deep = sort(unique(r@data$EC_Deep)),
           Seed = c(29000, 29000, 31000, 31000, 33000))

r@data <- left_join(r@data, seed, by = 'EC_Deep')

writeOGR(r,'D:/Box Sync/DIFM/DIFM Data by Variable/2017/Past Prescription/ILSBarton-37/ILS-37 2016 NH3 Rx',"37_U3_Strip_till_20170311_seed", driver="ESRI Shapefile")
