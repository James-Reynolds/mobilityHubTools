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
#' @param highlight_building_regional character values of the names of buildings to be highlighted and labelled on the map
#' @param highlight_amenity_regional character values of the names of amenities to be labelled on the map
#' @param highlight_leisure_regional character values of the names of leisure facilities to be labelled on the map
#'
#' @returns a ggplot object of a map in the style of the Bristol mobility hubs
#' @export
#'
#' @examples
bristol_regional_map <- function(
    amenity = rlist::list.load(system.file(
      "data/greensborough_amenity.rdata", package = "mobilityHubTools")),
    building = rlist::list.load(system.file(
      "data/greensborough_building.rdata", package = "mobilityHubTools")),
    highway = rlist::list.load(system.file(
      "data/greensborough_highway.rdata", package = "mobilityHubTools")),
    leisure = rlist::list.load(system.file(
      "data/greensborough_leisure.rdata", package = "mobilityHubTools")),
    landuse = rlist::list.load(system.file(
      "data/greensborough_landuse.rdata", package = "mobilityHubTools")),
    natural = rlist::list.load(system.file(
      "data/greensborough_natural.rdata", package = "mobilityHubTools")),
    railway = rlist::list.load(system.file(
      "data/greensborough_railway.rdata", package = "mobilityHubTools")),
    waterway = rlist::list.load(system.file("data/greensborough_waterway.rdata", package = "mobilityHubTools")),
    hub_location = rlist::list.load(system.file(
      "data/greensborough_hub_location.rdata", package = "mobilityHubTools")),
    map_limits = 1500,
    crs_local_metres = 27700,
    highlight_building_regional = c("Horfield Library", "Horfield Health Centre"),
    highlight_amenity_regional = c("nill"),
    highlight_leisure_regional = c("Bristol County Ground"),
    highlight_landuse_regional = c("Bonnington Walk Playing Fields"))
  {


# wrangle label information

labels <- hub_location
labels$name <- "You are here"

labels <- labels %>%
  tibble::add_row(
    tibble::tibble(building %>%
                     select(name) %>%
                     filter(name %in% highlight_building_regional) %>%
                     sf::st_centroid()))

labels <- labels %>%
  tibble::add_row(
    tibble::tibble(amenity %>%
                     select(name) %>%
                     filter(name %in% highlight_amenity_regional) %>%
                     sf::st_centroid()))
labels <- labels %>%
  tibble::add_row(
    tibble::tibble(leisure %>%
                     select(name) %>%
                     filter(name %in% highlight_leisure_regional) %>%
                     sf::st_centroid()))
labels <- labels %>%
  tibble::add_row(
    tibble::tibble(landuse %>%
                     select(name) %>%
                     filter(name %in% highlight_landuse_regional) %>%
                     sf::st_centroid()))

labels <- labels %>%
  tibble::add_row(
    tibble::tibble(railway %>%
                     dplyr::filter(railway == "station") %>%
                     select(name) %>%
                     sf::st_centroid()))


## build dataframe with transit stops and other locations to be represented by an icon
# hub location
icon_locations <- hub_location %>%
  sf::st_coordinates() %>%
  as.data.frame()
icon_locations$icon_filename <- system.file(
  "extdata/hub_location.png", package = "mobilityHubTools")

# railway stations
holding <- data.frame(
  railway %>% filter(railway %in% c("station")) %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% tibble::add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_railway.png", package = "mobilityHubTools"))
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
icon_locations <- if(nrow(holding) > 0) icon_locations %>% tibble::add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_supermarket.png", package = "mobilityHubTools"))
) else icon_locations



#build base layers - but only include highlighted buildings and
# cycle routes, and exclude smaller roads
map_regional <-
    bristol_map_base(amenity = amenity,
                     building = building %>%
                       dplyr::arrange(name) %>%
                       filter(name %in% highlight_building_regional),
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
ggplot2::geom_sf(data = amenity %>% filter(name %in% highlight_amenity_regional) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "purple") +

  ggplot2::geom_sf(data = building %>% filter(name %in% highlight_building_regional) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "purple") +



#add railway stations and other icons
  ggimage::geom_image(data = icon_locations,
                      mapping = ggplot2::aes(
                        x = X, y = Y,
                        image = icon_filename),
                      size = 0.03) +


# Hub walking radius
ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=1500), fill = NA, colour = "white", size = 2) +

# hub marker
ggplot2::geom_sf(data = hub_location, ggplot2::aes(), fill = "red", size = 5) +

# Add 15 minute walk radius text
  ggplot2::geom_sf_label(data = data.frame(
  x = hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(X) %>% as.numeric() - 1100,
  y =  hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
    dplyr::select(Y) %>% as.numeric() + 1100) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = crs_local_metres) %>%
  sf::st_as_sfc(), ggplot2::aes(), label = "15 minute walk", colour = "black", angle = 45) +


  # Add hub location and other text
  ggrepel::geom_label_repel(
    data = labels %>%
      sf::st_transform(crs = crs_local_metres) %>%
      sf::st_make_valid() %>%
      sf::st_crop(hub_location %>%
                    sf::st_buffer(dist = map_limits) %>%
                    sf::st_bbox()),
    mapping = ggplot2::aes(label = stringr::str_wrap(name, 20),
                           geometry = geometry),
    nudge_y = 50,
    size = 2,
    stat = "sf_coordinates")

  return(map_regional)
}
