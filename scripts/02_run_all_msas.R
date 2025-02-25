source("scripts/01_setup.R")

## List of MSAs
msasWithCounties <- st_read("permanentAssets/shapefiles/msaCountyIntersection.shp")
msasNames <- unique(msasWithCounties$NAMEm)


## Commuting Flows
output.1 <- method.CommutingFlows(grabLEHD(msas[1]), TRUE)
output.2 <- method.CommutingFlows(grabLEHD(msas[2]), TRUE)
output.3 <- method.CommutingFlows(grabLEHD(msas[3]), TRUE)


test.1 <- grabLEHD("Los Angeles-Long Beach-Anaheim, CA Metro Area")

output.1 <- method.CommutingFlows(grabLEHD("Los Angeles-Long Beach-Anaheim, CA Metro Area"), TRUE)