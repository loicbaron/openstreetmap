## Open Street Map
library(osmdata)
library(sf)
library(stringr)

normalize_minmax <- function(x, ...) {
  return((x - min(x, ...)) /(max(x, ...) - min(x, ...)))
}

locations <- readRDS("data/objects/locations")

# This can ERROR with HTTP 504
# Saving each location as an object
# we can re-run the code and skip if we already have it stored
get_osm <- function(loc_code, loc_name, loc_geo, key, value) {
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

for (i in 1:nrow(locations)) {
  osm_data <- get_osm(
    locations$GEOLEVEL2[i],
    locations$ADM2_VI[i],
    locations$geometry[i],
    "amenity",
    emergency_services
  )
  features <- st_intersection(locations$geometry[i], osm_data$osm_points)
  locations$cnt[i] <- nrow(as.data.frame(features))
}
locations$freq <- locations$cnt / locations$pop * 1000
locations$norm <- normalize_minmax(locations$freq, na.rm = TRUE)
# C_GOV2
# Access to emergency services: hospitals, fire brigades, police stations
# Proxy: Density of  emergency services: hospitals, fire brigades, police stations per 1,000 inhabitants
st_write(locations, "output/emergencies_C_GOV2.shp", layer_options = "ENCODING=UTF-8", append=FALSE)
