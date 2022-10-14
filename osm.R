## Open Street Map
library(osmdata)
library(sf)
library(stringr)

locations <- readRDS("data/objects/locations")

# This can ERROR with HTTP 504
# Saving each location as an object
# we can re-run the code and skip if we already have it stored
get_osm <- function(loc_code, loc_name, loc_geo, key, value, features_type) {
  f <- str_glue("data/objects/{loc_code}")
  if (file.exists(f) == TRUE) {
    cat("already have features for: ", loc_name, "\n")
    osm_features <- readRDS(f)
  } else {
    cat("get osm features for: ", loc_name, "\n")
    osm_features <- opq(bbox = st_bbox(loc_geo), timeout = 1000) %>%
      add_osm_feature(key, value) %>%
      osmdata_sf() %>%
      unname_osmdata_sf()
    saveRDS(osm_features, f)
  }
  return(osm_features)
}

emergency_services <- c("hospital", "police", "fire_station")
osm_emergency <- get_osm(
  locations$GEOLEVEL2[1],
  locations$ADM2_VI[1],
  locations$geometry[1],
  "amenity",
  emergency_services
  )
er <- st_intersection(osm_emergency$osm_polygons, locations$geometry[1])
nb_er <- nrow(as.data.frame(er))
C_GOV2 <- nb_er / locations$pop[1] * 100000
st_write(er, "output/er_polygons.shp", layer_options = "ENCODING=UTF-8", append=FALSE)


# Error in `[[<-.data.frame`(`*tmp*`, i, value = list("20.979242,105.716335,21.10758,105.804311",  :
# replacement has 8 rows, data has 391
# In addition: Warning message:
#   In mapply(get_osm, locations$GEOLEVEL2, locations$ADM2_VI, locations$geometry,  :
#               longer argument not a multiple of length of shorter
locations$emergencies <- mapply(get_osm,
                      locations$GEOLEVEL2,
                      locations$ADM2_VI,
                      locations$geometry,
                      "amenity",
                      emergency_services
                      )

