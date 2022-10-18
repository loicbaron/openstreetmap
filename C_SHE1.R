library(sf)

source("helpers.R")
source("osm.R")

locations <- readRDS("data/objects/locations")

# C_SHE1
# Access to shelter places
# Density of schools km2 per 1,000 inhabitants
# https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dschool
# density of primary and secondary schools per km2
education_services <- c("school", "college", "university")

tmp <- "data/objects/C_SHE1"
mkdirs(tmp)
for (i in 1:nrow(locations)) {
    osm_data <- get_osm(
        locations$GEOLEVEL2[i],
        locations$ADM2_VI[i],
        locations$geometry[i],
        "amenity",
        education_services,
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
locations$freq <- locations$cnt / as.numeric(locations$area) / locations$pop * 1000
locations$norm <- normalize_minmax(locations$freq, na.rm = TRUE)
st_write(locations, "output/C_SHE1_schools.shp", layer_options = "ENCODING=UTF-8", append = FALSE)
