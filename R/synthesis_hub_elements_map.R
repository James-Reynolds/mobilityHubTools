#' Build the base layers of maps as per the Bristol mobility hub example.
#'
#' @param amenity osm layers as sf, with crs set to local metres.
#' @param building osm layers as sf, with crs set to local metres.
#' @param highway osm layers as sf, with crs set to local metres.
#' @param railway osm layers as sf, with crs set to local metres.
#' @param hub_location sf point with name of mobility hub and location, with crs set to local metres.
#' @param map_limits a numeric value setting the extents of the map
#' @param crs_local_metres the crs to use to calculate distances in metres
#' @param annotation_map_tile_type The background map type (one of that returned by rosm::osm.types, passed to ggspatial::annotation_map_tile) or NA to have no background map
#' @param annotation_map_tile_zoom The background zoom level (passed to ggspatial::annotation_map_tile)
#'
#' @returns the developed synthesis local map
#' @export
#'
#' @examples
synthesis_hub_elements_map <- function(
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
    waterway = rlist::list.load(system.file(
      "data/greensborough_waterway.rdata", package = "mobilityHubTools")),
    railway = rlist::list.load(system.file(
      "data/greensborough_railway.rdata", package = "mobilityHubTools")),
    hub_location = rlist::list.load(system.file(
      "data/test_hub_location.rdata", package = "mobilityHubTools"))[[1]],
    map_limits = 100,
    crs_local_metres = 27700,
    annotation_map_tile_type = NA,
    annotation_map_zoom = 17,
    hub_element_locations = test_hub_element_locations$gainsborough_square
    )
{

  #drop points from building layers
    building <- building[!(building %>% sf::st_geometry_type() == "POINT"),]


  # wrangle label information

  labels <- hub_location
  labels$name <- "You are here"

  labels <- labels %>%
    tibble::add_row(hub_element_locations %>%
                      filter(!is.na(name)) %>%
                      select(name))
  labels <- labels %>%
    tibble::add_row(
    tibble::tibble(railway %>%
                     dplyr::filter(railway == "station") %>%
                     select(name) %>%
                     sf::st_centroid()))

## build dataframe with transit stops and other locations to be represented by an icon
# hub location
icon_locations <- hub_element_locations %>%
  sf::st_coordinates() %>%
  as.data.frame()
icon_locations$icon_filename <- hub_element_locations$icon_filename
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



# create base layers

  map <-
    ggplot2::ggplot() +
    # parks and grassland
    ggplot2::geom_sf(data = leisure %>%
                       filter(leisure %in% c("garden", "pitch", "park", "dog_park")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "lightgreen") +
    ggplot2::geom_sf(data = landuse %>%
                       filter(landuse %in% c("grass", "meadow", "farmyard", "vineyard")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "lightgreen") +
    ggplot2::geom_sf(data = natural %>%
                       filter(natural %in% c("grassland")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "lightgreen") +
    ggplot2::geom_sf(data = leisure %>%
                       filter(leisure %in% c("nature_reserve", "golf_course")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "darkgreen") +
    ggplot2::geom_sf(data = landuse %>%
                       filter(landuse %in% c("forest", "orchard")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "darkgreen") +
    ggplot2::geom_sf(data = natural %>%
                       filter(natural %in% c("heath", "moor", "scrub", "shrubbery", "wood")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "darkgreen") +
    ggplot2::geom_sf(data = natural %>%
                       filter(natural %in% c("beach", "dune", "sand")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "yellow") +
    # waterways
    ggplot2::geom_sf(data = waterway %>%
                       filter(waterway %in% c("river", "stream", "tidal_channel", "flowline", "canal", "drain", "ditch", "link", "fairway", "dam")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill = "blue") +
    ggplot2::geom_sf(data = natural %>%
                       filter(natural %in% c("bay", "shoal", "strait", "water", "wetland")) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), fill =  "blue")  +

    ggplot2::geom_sf(data = building %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ),
                     mapping = ggplot2::aes(), fill = "white") +

    # roads, trails and railways
    ggplot2::geom_sf(data = highway %>%
                       filter(highway %in% c("living_street", "motorway", "motorway_link",
                                             "primary", "primary_link", "residential",
                                             "secondary", "secondary_link", "service",
                                             "tertiary", "tertiary_link",
                                             "trunk", "trunk_link")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), colour = "darkgrey") +

    ggplot2::geom_sf(data = railway %>%
                       filter(railway %in% c("rail", "tram")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), colour = "black", linetype = "dashed") +


  ggplot2::geom_sf(data = hub_location %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_transform(crs = crs_local_metres) %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "black", size = 10) +

  # add cycleways
  ggplot2::geom_sf(data = highway %>%
                     filter(highway %in% c("pedestrian", "track", "footway", "path",  "cycleway")) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), linetype = 2) +



    #add railway stations and other icons
    ggimage::geom_image(data = icon_locations,
                        mapping = ggplot2::aes(
                          x = X, y = Y,
                          image = icon_filename)) +

  # Add hub location labels and other text
  ggrepel::geom_label_repel(
    data = labels %>%
      sf::st_transform(crs = crs_local_metres) %>%
      sf::st_make_valid() %>%
      sf::st_crop(hub_location %>%
                    sf::st_buffer(dist = map_limits) %>%
                    sf::st_bbox()),
    mapping = ggplot2::aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 5,
    nudge_y = 10,
    nudge_x = 5) +


  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.x=ggplot2::element_blank(), #remove x axis labels
    axis.ticks.x=ggplot2::element_blank(), #remove x axis ticks
    axis.text.y=ggplot2::element_blank(),  #remove y axis labels
    axis.ticks.y=ggplot2::element_blank(), #remove y axis ticks
    legend.position="bottom",
    legend.text = ggplot2::element_text(size = 8))   +
  ggspatial::annotation_scale(location = 'br') +
  ggspatial::annotation_north_arrow()



  return(map)
}





