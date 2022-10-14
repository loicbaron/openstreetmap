library(dplyr)
library(sf)

admin_shp <- st_read("data/ADMIN/admin.shp")

geo_red_river <- dplyr::filter(admin_shp, GEOLEVEL2 > 704001000 & GEOLEVEL2 < 704039000)
geo_mekong <- dplyr::filter(admin_shp, GEOLEVEL2 > 704082000 & GEOLEVEL2 < 704096000)

locations <- rbind(geo_red_river, geo_mekong)
saveRDS(locations, "data/objects/locations")
