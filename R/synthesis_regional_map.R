
synthesis_regional_map <- function(
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
    map_limits = 1500,
    crs_local_metres = 27700,
    annotation_map_tile_type = "osm",
    annotation_map_zoom = 14,
    highlight_building_regional = c("Horfield Library", "Horfield Health Centre"),
    highlight_amenity_regional = c("University of the West of England (Frenchay Campus)",
                                   "St James Church"),
    highlight_leisure_regional = c("Bristol County Ground"),
    highlight_landuse_regional = c("Bonnington Walk Playing Fields"))
{

  # convert all inputs to the local crs
  amenity <- amenity %>%  sf::st_transform(crs =  crs_local_metres)
  building <- building %>%  sf::st_transform(crs =  crs_local_metres)
  highway <- highway %>%  sf::st_transform(crs =  crs_local_metres)
  leisure <- leisure %>%  sf::st_transform(crs =  crs_local_metres)
  landuse <- landuse %>%  sf::st_transform(crs =  crs_local_metres)
  natural <- natural %>%  sf::st_transform(crs =  crs_local_metres)
  waterway <- waterway %>%  sf::st_transform(crs =  crs_local_metres)
  railway <- railway %>%  sf::st_transform(crs =  crs_local_metres)
  hub_location <- hub_location %>%  sf::st_transform(crs =  crs_local_metres)


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



# create base layers, with or without the osm annotation tiles
if(is.na(annotation_map_tile_type)) {
  map <-
    ggplot2::ggplot() +
    # Hub walking radius
    ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=1500), fill = NA, colour = "black", size = 2) +



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
                       ), mapping = ggplot2::aes(), colour = "darkgrey") +

    ggplot2::geom_sf(data = railway %>%
                       filter(railway %in% c("rail", "tram")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ), mapping = ggplot2::aes(), colour = "black", linetype = "dashed") +


    ### THIS DOESN"T CURRENTLY WORK WITH THE ANNOTATION_MAP_TILE, hence it is in here
    #add railway stations and other icons
    ggimage::geom_image(data = icon_locations,
                        mapping = ggplot2::aes(
                          x = X, y = Y,
                          image = icon_filename),
                        size = 0.02) +
    # add cycleways
    ggplot2::geom_sf(data = highway %>%
                       filter(highway %in% c("pedestrian", "track", "footway", "path",  "cycleway")) %>%
                       sf::st_transform(crs = crs_local_metres) %>%
                       sf::st_make_valid() %>%
                       sf::st_crop(hub_location %>%
                                     sf::st_buffer(dist = map_limits) %>%
                                     sf::st_bbox()
                       ),
                     mapping = ggplot2::aes(), linetype = 2)

}  else {
    map <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(
      type = annotation_map_tile_type,
      zoom = annotation_map_zoom) +
      # Hub walking radius (to set map limits)
      ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=1500), fill = NA, colour = "black", size = 2)

  }


map <- map +
  ggplot2::geom_sf(data = hub_location %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_transform(crs = crs_local_metres) %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "black", size = 10) +


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


  # Hub walking radius 5 minutes
  ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=400), fill = NA, colour = "black", size = 2) +

  # Add 5 minute walk radius text
  ggplot2::geom_sf_label(data = data.frame(
    x = hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
      dplyr::select(X) %>% as.numeric() - 268.7,
    y =  hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
      dplyr::select(Y) %>% as.numeric() + 288.7) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = crs_local_metres) %>%
      sf::st_as_sfc(), ggplot2::aes(), label = "5 minute walk", colour = "black", angle = 45) +

  # Hub walking radius (to display on top of purple marking)
  ggplot2::geom_sf(data = hub_location %>% st_buffer(dist=1500), fill = NA, colour = "black", size = 2) +


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
    nudge_y = 70) +


  # Add 15 minute walk radius text
  ggplot2::geom_sf_label(data = data.frame(
    x = hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
      dplyr::select(X) %>% as.numeric() - 1050,
    y =  hub_location %>% sf::st_coordinates() %>% as.data.frame() %>%
      dplyr::select(Y) %>% as.numeric() + 1050) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = crs_local_metres) %>%
      sf::st_as_sfc(), ggplot2::aes(), label = "15 minute walk", colour = "black", angle = 45) +

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





