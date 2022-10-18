library(sf)

source("helpers.R")
source("osm.R")

# C_TRA1
# Access to transportation network
# Density of transportation network:
# - roads (highways, trunks, primary / secondary / tertiary),
# - waterways (rivers / canals / streams),
# - ferry stations
# per 1,000 inhabitants
# main_road_types <- c("motorway", "trunk", "primary")
# roads <- get_osm_features(location$ADMIN_NAME, "highway", all_road_types)$osm_lines
# location_roads <- st_intersection(st_geometry(roads), st_geometry(location))
# sum(st_length(location_roads))

locations <- readRDS("data/objects/locations")

# highway=trunk, highway=primary, highway=secondary, highway=tertiary, highway=unclassified
all_road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified")

tmp <- "data/objects/C_TRA1/roads"
mkdirs(tmp)
for (i in 1:nrow(locations)) {
    osm_data <- get_osm(
        locations$GEOLEVEL2[i],
        locations$ADM2_VI[i],
        locations$geometry[i],
        "highway",
        all_road_types,
        tmp
    )
    print(osm_data)
    cnt <- 0
    if (!is.null(osm_data$osm_lines)) {
        roads <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_lines))
        cnt <- as.numeric(sum(st_length(roads)))
    }
    if (!is.null(osm_data$osm_multilines)) {
        roads <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_multilines))
        cnt <- as.numeric(sum(st_length(roads)))
    }
    locations$cnt[i] <- cnt
}

# https://wiki.openstreetmap.org/wiki/Map_features#Waterway
# waterway=stream for a naturally-forming waterway that is too narrow to be classed as waterway=river
# (the commonly accepted rule for OpenStreetMap is that a stream can be jumped across by an active, able-bodied person).
# A stream need not be permanently filled with water. In case of varying size or intermittent waterways
# the distinction from larger rivers based on the above criterion should be made with respect to the high water level.

# Use waterway=fairway for a linear way representation of a navigable route in a body of water such as a lake or sea,
# in cases where other values such as waterway=river or waterway=canal are not appropriate.
# Do not use instead of waterway=river or waterway=canal.
waterways_types <- c("river", "canal", "fairway")

tmp <- "data/objects/C_TRA1/waterways"
mkdirs(tmp)
for (i in 1:nrow(locations)) {
    osm_data <- get_osm(
        locations$GEOLEVEL2[i],
        locations$ADM2_VI[i],
        locations$geometry[i],
        "waterway",
        waterways_types,
        tmp
    )
    print(osm_data)
    cnt <- 0
    if (!is.null(osm_data$osm_lines)) {
        waterways <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_lines))
        cnt <- as.numeric(sum(st_length(waterways)))
    }
    if (!is.null(osm_data$osm_multilines)) {
        waterways <- st_intersection(locations$geometry[i], st_make_valid(osm_data$osm_multilines))
        cnt <- as.numeric(sum(st_length(waterways)))
    }
    locations$cnt[i] <- locations$cnt[i] + cnt
}

locations$freq <- locations$cnt / locations$pop * 1000
locations$norm <- normalize_minmax(locations$freq, na.rm = TRUE)
st_write(locations, "output/C_TRA1_roads_waterways.shp", layer_options = "ENCODING=UTF-8", append = FALSE)
