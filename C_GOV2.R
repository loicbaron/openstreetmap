library(sf)

source("helpers.R")
source("osm.R")

# C_GOV2
# Access to emergency services: hospitals, fire brigades, police stations
# Proxy: Density of  emergency services
# hospitals, fire brigades, police stations per 1,000 inhabitants
emergency_services <- c("hospital", "clinic", "police", "fire_station")
locations <- readRDS("data/objects/locations")

tmp <- "data/objects/C_GOV2"
mkdirs(tmp)
for (i in 1:nrow(locations)) {
    osm_data <- get_osm(
        locations$GEOLEVEL2[i],
        locations$ADM2_VI[i],
        locations$geometry[i],
        "amenity",
        emergency_services,
        tmp
    )
    print(osm_data)
    cnt <- 0
    if (!is.null(osm_data$osm_points)) {
        features_points <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_points))
        cnt <- cnt + nrow(as.data.frame(features_points))
    }
    if (!is.null(osm_data$osm_polygons)) {
        features_polygons <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_polygons))
        cnt <- cnt + nrow(as.data.frame(features_polygons))
    }
    if (!is.null(osm_data$osm_multipolygons)) {
        features_multipolygons <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_multipolygons))
        cnt <- cnt + nrow(as.data.frame(features_multipolygons))
    }
    locations$cnt[i] <- cnt
}
locations$freq <- locations$cnt / locations$pop * 1000
locations$norm <- normalize_minmax(locations$freq, na.rm = TRUE)
st_write(locations, "output/C_GOV2_emergencies.shp", layer_options = "ENCODING=UTF-8", append = FALSE)
