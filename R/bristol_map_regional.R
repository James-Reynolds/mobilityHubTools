#' Build the smaller scale regional map as per the Bristol mobility hub example.
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
bristol_regional_map <- function(
    amenity, building, highway, leisure, landuse, natural,  railway,
    waterway, hub_location,
    map_limits = 1500,
    crs_local_metres = 27700,
    highlight_building = c("Horfield Library", "Horfield Health Centre"),
    highlight_amenity = c("nill"),
    highlight_leisure = c("Bristol County Ground"))
  {


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



#build base layers - but only include highlighted buildings and
# cycle routes, and exclude smaller roads
map_regional <-
    bristol_map_base(amenity = amenity,
                     building = building %>%
                       arrange(name) %>%
                       filter(name %in% highlight_building),
                     highway = highway %>%
                       filter(highway %in% c(
                         "motorway", "motorway_link",
                         "primary", "primary_link",
                         "residential",
                         "secondary", "secondary_link",
                         "tertiary",
                         "tertiary_link",
                         "trunk", "trunk_link",
                         "cycleway")) ,
                     leisure = leisure, landuse = landuse, natural = natural,
                     railway = railway, waterway = waterway,
                     hub_location = hub_location,
                     map_limits = 1500, crs_local_metres = crs_local_metres)

# combine base layers with additional layers
map_regional <- map_regional +

# Highlight buildings
ggplot2::geom_sf(data = building %>% filter(name %in% highlight_building) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = aes(), fill = "purple") +




# Hub walking radius
ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=1500), fill = NA, colour = "white", size = 2) +

# hub marker
ggplot2::geom_sf(data = hub_location, aes(), fill = "red", size = 5) +

# Add 5 minute walk radius text
  ggplot2::geom_sf_label(data = data.frame(
  x = hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(X) %>% as.numeric() - 1100,
  y =  hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(Y) %>% as.numeric() + 1100) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = crs_local_metres) %>%
  sf::st_as_sfc(), aes(), label = "15 minute walk", colour = "black", angle = 45) +


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

  return(map_regional)
}
