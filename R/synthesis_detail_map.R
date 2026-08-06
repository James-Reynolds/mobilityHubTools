#' Build a very small map showing detail of the mobility hub features.
#'
#' @param amenity osm layers as sf.
#' @param building osm layers as sf.
#' @param highway osm layers as sf.
#' @param leisure osm layers as sf.
#' @param landuse osm layers as sf.
#' @param natural osm layers as sf.
#' @param railway osm layers as sf.
#' @param waterway osm layers as sf.
#' @param hub_location sf point with name of mobility hub and location.
#' @param map_limits a numeric value setting the extents of the map in metres
#' @param crs_local_metres crs for local metres
#' @param highlight_building character names of buildings to be highlighted and labelled on the map
#' @param highlight_amenity character names of amenities to be labelled on the map
#' @param highlight_leisure character names of leisure facilities to be labelled on the map
#' @param hub_features sf dataframe of individual hub elements (points) and icon file links.
#'
#' @returns a ggplot object of a detailed map of the immediate hub surrounds
#' @export
#'
#' @examples
synthesis_detail_map <- function(
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
    map_limits = 110,
    crs_local_metres = 27700,
    highlight_building = c("North Bristol Advice Centre", "The Hub"),
    highlight_amenity = c("nill"),
    highlight_leisure = c("Gainsborough Square"),
    highlight_landuse = c("nill"),
    hub_features = data.frame(
      icon_filename = system.file(
        "extdata/bristol_bike_parking.png", package = "mobilityHubTools"),
      lat = 51.489995,
      lon = -2.563052) %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
)
      {


# wrangle label information
labels <- hub_location
labels$name <- "You are here"

labels <- labels %>%
  tibble::add_row(
    tibble::tibble(building %>%
                     select(name) %>%
                     filter(name %in% highlight_building) %>%
                     sf::st_centroid()))

labels <- labels %>%
  tibble::add_row(
    tibble::tibble(amenity %>%
                     select(name) %>%
                     filter(name %in% highlight_amenity) %>%
                     sf::st_centroid()))
labels <- labels %>%
  tibble::add_row(
    tibble::tibble(leisure %>%
                     select(name) %>%
                     filter(name %in% highlight_leisure) %>%
                     sf::st_centroid()))
labels <- labels %>%
  tibble::add_row(
    tibble::tibble(landuse %>%
                     select(name) %>%
                     filter(name %in% highlight_landuse) %>%
                     sf::st_centroid()))
labels <- labels %>%
  tibble::add_row(
    tibble::tibble(railway %>%
                     dplyr::filter(railway == "station") %>%
                     select(name) %>%
                     sf::st_centroid()))

#build base layers
map_detail <-
    bristol_map_base(amenity = amenity, building = building, highway = highway,
                     leisure = leisure, landuse = landuse, natural = natural,
                     railway = railway, waterway = waterway,
                     hub_location = hub_location,
                     map_limits = map_limits, crs_local_metres = crs_local_metres)


## build dataframe with transit stops and other locations to be represented by an icon
# hub location
icon_locations <- hub_location %>%
  sf::st_coordinates() %>%
  as.data.frame()
icon_locations$icon_filename <- system.file(
  "extdata/hub_location.png", package = "mobilityHubTools")

# bus stops
holding <- data.frame(
  highway[(highway %>% sf::st_geometry_type() == "POINT"),] %>%
    filter(highway %in% c("bus_stop")) %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates())
icon_locations <- if(nrow(holding) > 0) icon_locations %>% tibble::add_row(
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
icon_locations <- if(nrow(holding) > 0) icon_locations %>% tibble::add_row(
  holding %>% tibble::add_column(
    icon_filename = system.file("extdata/bristol_railway.png", package = "mobilityHubTools"))
) else icon_locations
# hub features
holding <- hub_features %>%
    sf::st_transform(crs = crs_local_metres) %>%
    sf::st_make_valid() %>%
    sf::st_crop(hub_location %>%
                  sf::st_buffer(dist = map_limits) %>%
                  sf::st_bbox()) %>%
    st_coordinates() %>%
  as.data.frame()
icon_locations <- if(nrow(holding) > 0) icon_locations %>% tibble::add_row(
  holding %>%
    tibble::add_column(hub_features %>%
      st_drop_geometry())
) else icon_locations



## Puting it all together
# combine base layers with additional layers
map_detail <- map_detail +

# Highlight buildings
ggplot2::geom_sf(data = building %>%
                   filter(name %in% highlight_building) %>%
                     sf::st_transform(crs = crs_local_metres) %>%
                     sf::st_make_valid() %>%
                     sf::st_crop(hub_location %>%
                                   sf::st_buffer(dist = map_limits) %>%
                                   sf::st_bbox()
                     ),
                   mapping = ggplot2::aes(), fill = "purple") +


#add bus stops, railway stations and other icons
  ggimage::geom_image(data = icon_locations,
                      mapping = ggplot2::aes(
                        x = X, y = Y,
                        image = icon_filename),
                      size = 0.05) +

# hub marker
ggplot2::geom_sf(data = hub_location, ggplot2::aes(), fill = "red", size = 5) +

  # Add hub location and other text
  ggrepel::geom_label_repel(
    data = labels %>%
      sf::st_transform(crs = crs_local_metres) %>%
      sf::st_make_valid() %>%
      sf::st_crop(hub_location %>%
                    sf::st_buffer(dist = map_limits) %>%
                    sf::st_bbox()),
    mapping = ggplot2::aes(label = name, geometry = geometry),
    stat = "sf_coordinates")

  return(map_detail)
}
