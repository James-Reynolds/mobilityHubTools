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
    hub_element_locations = tibble::tibble(
        lat = c(51.49091020337705,
                51.490725669406785,
                51.49028620779843,
                51.489975,
                51.490122982307746,
                51.48995900850345,
                51.49012161163161,
                51.49018090729279,
                51.49015928560444
        ),
        lon = c(-2.5638763617435965,
                -2.562516710126253,
                -2.5622785784761555,
                -2.563095,
                -2.5629193443267115,
                -2.562930526314595,
                -2.562804165030892,
                -2.562311516676315,
                -2.5624731875894544
        ),
        name = c("Gainsborough Sq. Stop A",
                 "Gainsborough Sq. Stop B",
                 "Cameron Wk. stop",
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
        icon_filename = c(
          system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
          system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
          system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
          system.file("extdata/bicycle_parking.svg", package = "mobilityHubTools"),
          system.file("extdata/bicycle_repair_station.svg", package = "mobilityHubTools"),
          system.file("extdata/e-scooter-svgrepo-com.svg", package = "mobilityHubTools"),
          system.file("extdata/bicycle-electric-2.svg", package = "mobilityHubTools"),
          system.file("extdata/toilets.svg", package = "mobilityHubTools"),
          system.file("extdata/RWBA_Behinderten-WC.svg", package = "mobilityHubTools")
        )) %>% sf::st_as_sf(coords = c("lon", "lat"),
                            crs = 4326) %>%
      sf::st_transform(crs =  27700) %>%
      sf::st_cast("POINT")
    )

{
  #drop points from building layers
    building <- building[!(building %>% sf::st_geometry_type() == "POINT"),]

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





