#' Build the base layers of maps as per the Bristol mobility hub example.
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
#' @param crs_local_metres the crs to use to calculate distances in metres
#'
#' @returns a map in the style of the Bristol mobility hubs
#' @export
#'
#' @examples
bristol_map_base <- function(
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
    map_limits = 500,
    crs_local_metres = 27700
){
  map_base <-
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
            mapping = ggplot2::aes(), fill = "white") +

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
              ), mapping = ggplot2::aes(), colour = "black") +
    ggplot2::geom_sf(data = highway %>%
              filter(highway %in% c("cycleway", "bridleway", "steps", "path")) %>%
                sf::st_transform(crs = crs_local_metres) %>%
                sf::st_make_valid() %>%
                sf::st_crop(hub_location %>%
                        sf::st_buffer(dist = map_limits) %>%
                        sf::st_bbox()
              ), mapping = ggplot2::aes(), colour = "purple") +

    ggplot2::geom_sf(data = railway %>%
              filter(railway %in% c("rail", "tram")) %>%
                sf::st_transform(crs = crs_local_metres) %>%
                sf::st_make_valid() %>%
                sf::st_crop(hub_location %>%
                        sf::st_buffer(dist = map_limits) %>%
                        sf::st_bbox()
              ), mapping = ggplot2::aes(), colour = "black", linetype = "dashed") +
    theme(
      axis.title = element_blank(),
      axis.text.x=element_blank(), #remove x axis labels
      axis.ticks.x=element_blank(), #remove x axis ticks
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank(), #remove y axis ticks
      legend.position="none",
      legend.text = element_text(size = 8),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, 'in'),
    )   +
    ggspatial::annotation_scale(location = 'br') +
    ggspatial::annotation_north_arrow()

  return(map_base)
}
