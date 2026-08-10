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
synthesis_local_map <- function(
    amenity = rlist::list.load(system.file(
      "data/greensborough_amenity.rdata", package = "mobilityHubTools")),
    building = rlist::list.load(system.file(
      "data/greensborough_building.rdata", package = "mobilityHubTools")),
    highway = rlist::list.load(system.file(
      "data/greensborough_highway.rdata", package = "mobilityHubTools")),
    railway = rlist::list.load(system.file(
      "data/greensborough_railway.rdata", package = "mobilityHubTools")),
    hub_location = rlist::list.load(system.file(
      "data/greensborough_hub_location.rdata", package = "mobilityHubTools")),
    map_limits = 100,
    crs_local_metres = 27700,
    annotation_map_tile_type = "cartolight",
    annotation_map_zoom = 17
){

# wrangle label information
mapping_labels <- hub_location
mapping_labels$name <- "You are here"





# create base layers, with or without the osm annotation tiles
if(is.na(annotation_map_tile_type)) {
  map_local <-
    ggplot2::ggplot() +
    # white building outlines
    ggplot2::geom_sf(data = building %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_transform(crs = crs_local_metres) %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ),
                     mapping = ggplot2::aes(), fill = "white")

}  else {
    map_local <-
    ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = annotation_map_tile_type, zoom = annotation_map_zoom) +
      # white building outlines
      ggplot2::geom_sf(data = building %>%
                         sf::st_transform(crs = crs_local_metres) %>%
                         sf::st_make_valid() %>%
                         sf::st_crop(hub_location %>%
                                       sf::st_transform(crs = crs_local_metres) %>%
                                       sf::st_buffer(dist = map_limits) %>%
                                       sf::st_bbox()
                         ),
                       mapping = ggplot2::aes(), fill = "white")

  }


map_local <- map_local +
  ggplot2::geom_sf(data = hub_location %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_transform(crs = crs_local_metres) %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "black", size = 10) +

  # Add hub location and other text
  ggrepel::geom_label_repel(
    data = mapping_labels %>%
      sf::st_transform(crs = crs_local_metres) %>%
      sf::st_make_valid() %>%
      sf::st_crop(hub_location %>%
                    sf::st_buffer(dist = map_limits) %>%
                    sf::st_bbox()),
    mapping = ggplot2::aes(label = name, geometry = geometry),
    stat = "sf_coordinates") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x=ggplot2::element_blank(), #remove x axis labels
      axis.ticks.x=ggplot2::element_blank(), #remove x axis ticks
      axis.text.y=ggplot2::element_blank(),  #remove y axis labels
      axis.ticks.y=ggplot2::element_blank(), #remove y axis ticks
      legend.position="none",
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_blank())   +
    ggspatial::annotation_scale(location = 'br') +
    ggspatial::annotation_north_arrow()

  return(map_local)
}
