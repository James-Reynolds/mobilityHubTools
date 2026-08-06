
extract_osm_data <- function(
    hub_location = tibble::tibble(
        lat = 52.44817508768327,
        lon =  -2.0686081332378095),
    crs_local_metres = 27700,
    distance_to_dowload = 1100,
    key = c("amenity", "building", "highway", "landuse", "leisure", "natural",
            "railway", "waterway")
    )
{
### Create hub locations
hub_location <- hub_location %>%
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326) %>%
  sf::st_transform(crs = crs_local_metres) %>%
  sf::st_cast("POINT")

bb_vector <- hub_location %>%
  sf::st_buffer(distance_to_dowload) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_bbox() %>%
  as.numeric()
bb <- data.frame(min = bb_vector[1:2], max = bb_vector[3:4])
rownames(bb) <- c("x", "y")


output <- lapply(key, queuing_function)

###### PICK UP FROM HERE


#select lines, polygons etc into sf dataframe
#amenity <- lapply(amenity, function(x) bind_rows(x$osm_lines, x$osm_polygons, x$osm_multilines, x$osm_multipolygons, x$osm_points))
buildings <- lapply(buildings, function(x) bind_rows(x$osm_lines, x$osm_polygons, x$osm_multilines, x$osm_multipolygons))



### Convert to metres on Irish mapping grid
#amenity <- lapply(amenity, function(x) x %>%
#  st_transform(crs = 2157))
buildings <- lapply(buildings, function(x) x %>%
                      st_transform(crs = 2157))

### Calculate distances to hubs - straight line (as the crow flies)
#amenity$letterkenny <- amenity$letterkenny %>%
#  mutate(distance_to_hub = st_distance(amenity$letterkenny, hub_location$letterkenny))
#amenity$galway <- amenity$galway %>%
#  mutate(distance_to_hub = st_distance(amenity$galway, hub_location$galway))
#amenity$waterford <- amenity$waterford %>%
#  mutate(distance_to_hub = st_distance(amenity$waterford, hub_location$waterford))
#amenity$dundrum <- amenity$dundrum %>%
#  mutate(distance_to_hub = st_distance(amenity$dundrum, hub_location$dundrum))
buildings$letterkenny <- buildings$letterkenny %>%
  mutate(distance_to_hub = st_distance(buildings$letterkenny, hub_location$letterkenny))
buildings$galway <- buildings$galway %>%
  mutate(distance_to_hub = st_distance(buildings$galway, hub_location$galway))
buildings$waterford <- buildings$waterford %>%
  mutate(distance_to_hub = st_distance(buildings$waterford, hub_location$waterford))
buildings$dundrum <- buildings$dundrum %>%
  mutate(distance_to_hub = st_distance(buildings$dundrum, hub_location$dundrum))


buildings <- buildings %>%
  lapply(function(x)
    x %>% mutate(building = as_factor(building)))


