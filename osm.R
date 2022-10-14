## Open Street Map
library(osmdata)

locations <- readRDS("data/objects/locations")

get_osm <- function(loc_name, loc_geo, key, value, features_type) {
  cat("get osm features for: ", loc_name, "\n")
  result <- opq(bbox = st_bbox(loc_geo), timeout = 100) %>%
    add_osm_feature(key, value) %>%
    osmdata_sf() %>%
    unname_osmdata_sf()
  return(result)
}

emergency_services <- c("hospital", "police", "fire_station")
osm_emergency <- get_osm(locations$ADM2_VI[1], locations$geometry[1], "amenity", emergency_services)
er <- st_intersection(osm_emergency$osm_polygons, locations$geometry[1])
nb_er <- nrow(as.data.frame(er))
C_GOV2 <- nb_er / locations$pop[1] * 100000
st_write(er, "output/er_polygons.shp", layer_options = "ENCODING=UTF-8", append=FALSE)

# This ERRORS with HTTP 504
emergencies <- mapply(get_osm, locations$ADM2_VI, locations$geometry, "amenity", emergency_services)
