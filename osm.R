## Open Street Map
library(osmdata)
library(sf)
library(stringr)

# This can ERROR with HTTP 504
# Saving each location as an object
# we can re-run the code and skip if we already have it stored
get_osm <- function(loc_code, loc_name, loc_geo, key, value, folder) {
  f <- str_glue("{folder}/{loc_code}")
  if (file.exists(f) == TRUE) {
    cat("already have features for: ", loc_name, "\n")
    osm_features <- readRDS(f)
  } else {
    cat("get osm features for: ", loc_name, "\n")
    osm_features <- opq(bbox = st_bbox(loc_geo), timeout = 1000) %>%
      add_osm_feature(key, value) %>%
      osmdata_sf() %>%
      unique_osmdata()
    saveRDS(osm_features, f)
  }
  return(osm_features)
}
