
synthesis_hub_surrounds_map <- function(
    amenity = rlist::list.load(system.file(
      "data/greensborough_amenity.rdata", package = "mobilityHubTools")),
    building = rlist::list.load(system.file(
      "data/greensborough_building.rdata", package = "mobilityHubTools")),
    map_limits = 100,
    crs_local_metres = 27700,
    annotation_map_zoom = 18,
    annotation_map_type = "osm",
    highlight_building = c("North Bristol Advice Centre", "The Hub"),
    hub_location = rlist::list.load(system.file(
      "data/test_hub_location.rdata", package = "mobilityHubTools"))[[1]]
    )
{

  #drop points from building layers
    building <- building[!(building %>% sf::st_geometry_type() == "POINT"),]


  # wrangle label information

  labels <- hub_location
  labels$name <- "You are here"

  labels <- labels %>%
    tibble::add_row(
      tibble::tibble(building %>%
                       select(name) %>%
                       filter(name %in% highlight_building) %>%
                       sf::st_centroid()))

hub_surrounds_map <-
  ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = annotation_map_type, zoom = annotation_map_zoom) +
  ggplot2::geom_sf(data = hub_location %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_transform(crs = crs_local_metres) %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "black", size = 10) +
#set map size
  ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=map_limits), fill = NA, colour = "white", size = 0.0002) +

  # Highlight buildings

  ggplot2::geom_sf(data = building %>% filter(name %in% highlight_building) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "purple") +


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
    size = 7,
    nudge_y = 20) +


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


  return(hub_surrounds_map)
}





