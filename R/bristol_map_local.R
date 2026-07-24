#' Build the larger scale local map as per the Bristol mobility hub example.
#'
#' @param amenity osm layers as sf, with crs set to local metres.
#' @param building osm layers as sf, with crs set to local metres.
#' @param highway osm layers as sf, with crs set to local metres.
#' @param leisure osm layers as sf, with crs set to local metres.
#' @param landuse osm layers as sf, with crs set to local metres.
#' @param natural osm layers as sf, with crs set to local metres.
#' @param railway osm layers as sf, with crs set to local metres.
#' @param waterway osm layers as sf, with crs set to local metres.
#' @param hub_location sf point with name of mobility hub and location, with crs set to local metres.
#' @param map_limits a numeric value setting the extents of the map
#' @param crs_local_metres the numeric value representing the crs for local metres
#' @param highlight_building character values of the names of buildings to be highlighted and labelled on the map
#' @param highlight_amenity character values of the names of amenities to be labelled on the map
#' @param highlight_leisure character values of the names of leisure facilities to be labelled on the map
#'
#' @returns a ggplot object of a map in the style of the Bristol mobility hubs
#' @export
#'
#' @examples
bristol_local_map <- function(
    amenity, building, highway, leisure, landuse, natural,  railway,
    waterway, hub_location,
    map_limits = 500,
    crs_local_metres = 27700,
    highlight_building = c("North Bristol Advice Centre", "The Hub"),
    highlight_amenity = c("Stoke Park Primary School",
                          "Saint Mary Magdalen and Saint Francis Lockleaze",
                          "St James Church"),
    highlight_leisure = c("Lockleaze Youth And Play Space",
                           "Gainsborough Square")
){


# wrangle label information
labels <- hub_location
labels$name <- "You are here"

labels <- labels %>%
  add_row(tibble(name = sort(highlight_building),
                 geometry = building %>%
                   arrange(name) %>%
                   filter(name %in% highlight_building) %>%
                   st_centroid() %>% st_geometry()))

labels <- labels %>%
  add_row(tibble(name = sort(highlight_amenity),
                 geometry = amenity %>%
                   arrange(name) %>%
                   filter(name %in% highlight_amenity) %>%
                   st_centroid() %>% st_geometry()))
labels <- labels %>%
  add_row(tibble(name = sort(highlight_leisure),
                 geometry = leisure %>%
                 arrange(name) %>%
                 filter(name %in% highlight_leisure) %>%
                   st_centroid() %>% st_geometry()))

labels <- labels %>%
  add_row(tibble(name = railway %>%
                   sf::st_drop_geometry() %>%
                   dplyr::filter(railway == "station") %>%
                   dplyr::select(name) %>%
                   unlist(),
                 geometry = railway %>%
                   dplyr::filter(railway == "station") %>%
                   st_centroid() %>% st_geometry()))

#build base layers
map_local <-
    bristol_map_base(amenity = amenity, building = building, highway = highway,
                     leisure = leisure, landuse = landuse, natural = natural,
                     railway = railway, waterway = waterway,
                     hub_location = hub_location,
                     map_limits = 500, crs_local_metres = crs_local_metres)


## build dataframe with transit stops and other locations to be represented by an icon
# hub location
icon_locations <- hub_location %>%
  sf::st_coordinates() %>%
  as.data.frame()
icon_locations$icon_filename <- system.file(
  "extdata/hub_location.png", package = "mobilityHubTools")

# bus stops
holding <- data.frame(
  highway[(highway %>% st_geometry_type() == "POINT"),] %>%
    filter(highway %in% c("bus_stop")) %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_bus.png", package = "mobilityHubTools"))
) else icon_locations
# railway stations
holding <- data.frame(
  railway %>% filter(railway %in% c("station")) %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_railway.png", package = "mobilityHubTools"))
) else icon_locations
# railway stations
holding <- data.frame(
  railway %>% filter(railway %in% c("station")) %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_bus.png", package = "mobilityHubTools"))
) else icon_locations
# groceries
holding <- data.frame(
  building %>% filter(shop %in% c("convenience", "supermarket")) %>%
    sf::st_centroid() %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_supermarket.png", package = "mobilityHubTools"))
) else icon_locations
# food
holding <- data.frame(
  amenity %>% filter(amenity %in% c("fast_food", "cafe", "food_court", "restaurant")) %>%
    sf::st_centroid() %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_food.png", package = "mobilityHubTools"))
) else icon_locations






## Puting it all together
# combine base layers with additional layers
map_local <- map_local +

# Highlight buildings
ggplot2::geom_sf(data = building %>%
                   filter(name %in% highlight_building) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = aes(), fill = "purple") +


#add bus stops, railway stations and other icons
  ggimage::geom_image(data = icon_locations,
                      mapping = aes(
                        x = X, y = Y,
                        image = icon_filename),
                      size = 0.01) +

# Hub walking radius
ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=400), fill = NA, colour = "white", size = 2) +

# hub marker
ggplot2::geom_sf(data = hub_location, aes(), fill = "red", size = 5) +


# Add 5 minute walk radius text
  ggplot2::geom_sf_label(data = data.frame(
  x = hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(X) %>% as.numeric() - 268.7,
  y =  hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(Y) %>% as.numeric() + 288.7) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = crs_local_metres) %>%
  sf::st_as_sfc(), aes(), label = "5 minute walk", colour = "black", angle = 45) +

  # Add hub location and other text
  ggrepel::geom_label_repel(
    data = labels %>%
      sf::st_transform(crs = crs_local_metres) %>%
      sf::st_make_valid() %>%
      sf::st_crop(hub_location %>%
                    sf::st_buffer(dist = map_limits) %>%
                    sf::st_bbox()),
    mapping = aes(label = name, geometry = geometry),
    stat = "sf_coordinates")



  return(map_local)
}
