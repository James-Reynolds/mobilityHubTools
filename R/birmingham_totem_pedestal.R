#' Builds a totem pedestral sign in the style of Birmingham mobility hubs
#'
#' @param file_to_save_to a character string of the file to save the pdf to
#' @param hub_name_text the name of the hub
#' @param icons_to_include_in_header a list of 5 strings of the location of the icons to include in the header
#' @param directions_image a character string of the location of an image showing directions
#' @param map_local a ggplot object
#' @param points_of_interest_image a character string of the location of an image showing local points of interest
#' @param map_regional a ggplot object
#' @param acknowledgements_text a character string including acknowledgements and copyright details.
#'
#' @returns nothing, but outputs a pdf to the named file
#' @export
#'
#' @examples
#' birmingham_totem_pedestal(
#'  file_to_save_to = "layout_test.pdf",
#'  hub_name_text = "Gainsbourgh Square")

birmingham_totem_pedestal <- function(
    file_to_save_to,
    hub_name_text,
    icons_to_include_in_header = list(
      system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle_repair_station.svg", package = "mobilityHubTools"),
      system.file("extdata/e-scooter-svgrepo-com.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle-electric-2.svg", package = "mobilityHubTools"),
      system.file("extdata/toilets.svg", package = "mobilityHubTools")),
    directions_image = system.file(
      "extdata/birmingham_directions.png", package = "mobilityHubTools"),
    map_local = bristol_local_map(),
    points_of_interest_image =  system.file(
      "extdata/birmingham_points_of_interest.png", package = "mobilityHubTools"),
    map_regional = bristol_regional_map(),
    acknowledgements_text = "Map data from OpenStreetMap, available under the Open Database License. © OpenStreetMap contributors. See openstreetmap.org/copyright \n \nIcons from https://github.com/gmgeo/osmic https://www.svgrepo.com/svg/450115/e-scooter"
)
{

  grid::grid.rect(gp = grid::gpar(lty = "dashed"))
  hub_name_viewport <- grid::viewport(x = 0, y = 11.5/12, w = 1, h = 0.5/12, just = c("left", "bottom"), name = "hub_name_viewport")
  hub_facilities_viewport <- grid::viewport(x = 0, y = 11/12, w = 1, h = 0.5/12, just = c("left", "bottom"), name = "hub_facilities_viewport")
  direction_information <- grid::viewport(x = 0, y = 10/12, w = 1, h = 1/12, just = c("left", "bottom"), name = "directional_information_viewport")
  local_map_viewport <- grid::viewport(x = 0, y = 5/12, w = 1, h = 5/12, just = c("left", "bottom"), name = "local_map_viewport")
  hub_facilities_viewport_details <- grid::viewport(x = 0, y = 3/12, w = 5/9, h = 2/12, just = c("left", "bottom"), name = "hub_facilities_viewport_details")
  regional_map_viewport <- grid::viewport(x = 5/9, y = 3/12, w = 4/9, h = 2/12, just = c("left", "bottom"), name = "regional_map_viewport")
  logos_viewport <- grid::viewport(x = 5/9, y = 2/12, w = 4/9, h = 1/12, just = c("left", "bottom"), name = "logos_viewport")

  grid::pushViewport(hub_name_viewport)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text(hub_name_text, y = 0.5)
  grid::upViewport()
  grid::pushViewport(hub_facilities_viewport)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("Hub icons go here", y = 0.5)
  grid::upViewport()
  grid::pushViewport(direction_information)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("Direction information goes here", y = 0.5)
  grid::upViewport()
  grid::pushViewport(local_map_viewport)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("Local Map goes here", y = 0.5)
  grid::upViewport()
  grid::pushViewport(hub_facilities_viewport_details)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("Hub facility details", y = 0.5)
  grid::upViewport()
  grid::pushViewport(regional_map_viewport)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("Area Map goes here", y = 0.5)
  grid::upViewport()
  grid::pushViewport(logos_viewport)
  grid::grid.rect(gp = grid::gpar(col = "white"))
  grid::grid.text("logos_viewport etc", y = 0.5)
  grid::popViewport()



  grid::grid.rect(gp = grid::gpar(lty = "blank"))

  r <- grid::rectGrob(gp=grid::gpar(fill="white"))

  #Define layout
  gs <- lapply(1:8, function(ii)
    grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
  gridExtra::grid.arrange(grobs=gs, ncol=9,
                          top="top label", bottom="bottom\nlabel",
                          left="left label", right="right label")
  grid::grid.rect(gp=grid::gpar(fill=NA))


  #Wrangle icons above hub name
  image <- lapply(icons_to_include_in_header, function (x) svgparser::read_svg(x))
  grob_1 <- image[[1]]
  grob_2 <- image[[2]]
  grob_3 <- image[[3]]
  grob_4 <- image[[4]]
  grob_5 <- image[[5]]

  gs[[1]] <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="white", lty = 1)),
    gridExtra::grid.arrange(
      grob_1, grob_2, grob_3, grob_4, grob_5,
      ncol = 5)
  )

  #Insert Local Travel Point branding
  hub_name_viewport <- grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(
      "Local Travel Point",
      ,
      gp=grid::gpar(
        fontsize=50, col = "black", fontface="bold", lty = "blank")))
  gs[[2]] <- gridExtra::grid.arrange(hub_name_viewport)




  #Define hub name
  hub_name_viewport <- grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(
      hub_name_text,
      gp=grid::gpar(
        fontsize=70, col = "black", fontface="bold", lty = "blank")))
  gs[[3]] <- gridExtra::grid.arrange(hub_name_viewport)



  # Insert directions image below hub name and icons
  gs[[4]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="white", lty = 1)),
                            gridExtra::grid.arrange(grid::rasterGrob(magick::image_read(
                              directions_image)),
                              ncol = 1))

  gs[[5]] <- map_local
  # Insert facilities description image below hub name and icons
  gs[[6]] <- map_regional
  gs[[7]] <- grid::rasterGrob(magick::image_read(points_of_interest_image))
  gs[[8]] <- gridExtra::grid.arrange(
    grid::grobTree(
      grid::rectGrob(
        gp=grid::gpar(fill="white")),
      grid::textGrob(acknowledgements_text,
                     x = 0.95, y=0.05, gp = grid::gpar(fontsize = 10, col = "black"), just = c("right", "bottom"))))


  # 24 rows, each roughly 100mm high
  # 5 columns, each roughly 200mm wide
  lay <- rbind(c(1,1,1,1,1),
               c(1,1,1,1,1),
               c(2,2,2,2,2),
               c(3,3,3,3,3),
               c(4,4,4,4,4),
               c(4,4,4,4,4),
               c(4,4,4,4,4),
               c(4,4,4,4,4),
               c(4,4,4,4,4),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(6,6,6,6,6),
               c(7,7,7,7,7),
               c(7,7,7,7,7),
               c(7,7,7,7,7),
               c(7,7,7,7,7),
               c(7,7,7,7,7),
               c(8,8,8,8,8),
               c(8,8,8,8,8))
  gridExtra::grid.arrange(grobs = gs, layout_matrix = lay)
  grDevices::dev.copy2pdf(file = file_to_save_to, width = 11, height = 40.8, fonts = NULL)
  grDevices::dev.off()


}
