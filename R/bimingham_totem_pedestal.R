#' Builds a totem pedestral sign in the style of Birmingham Local Travel Point
#'
#' @param file_to_save_to a character string of the file to save the png to
#' @param hub_name_text the name of the hub
#' @param icons_to_include_in_header a list of 5 strings of the location of the icons to include in the header, including blanks
#' @param directions_image a character string of the location of an image showing directions
#' @param map_local ggplot of the local map to include
#' @param points_of_interest_image a character string of the location of an image showing points of interest
#' @param map_regional ggplot of the regional map to include
#' @param logo_image a character string of the location of an image showing agency logos
#' @returns nothing, but outputs a png to the named file
#' @export
#'
#' @examples
birmingham_totem_pedestal <- function(
    file_to_save_to = "layout_test.pdf",
    hub_name_text = "Andrew Road",
    icons_to_include_in_header = list(
      system.file(
      "extdata/birmingham_icon_hub.png", package = "mobilityHubTools"),
      system.file(
        "extdata/birmingham_icon_bike.png", package = "mobilityHubTools"),
      system.file(
        "extdata/birmingham_icon_bus.png", package = "mobilityHubTools"),
      system.file(
        "extdata/birmingham_icon_blank.png", package = "mobilityHubTools"),
      system.file(
      "extdata/birmingham_icon_blank.png", package = "mobilityHubTools")),
    directions_image = system.file(
      "extdata/birmingham_directions.png", package = "mobilityHubTools"),
    map_local = map_local_test,
    points_of_interest_image =  system.file(
      "extdata/bristol_facilities.png", package = "mobilityHubTools"),
    map_regional = map_regional_test,
    logo_image =  system.file(
      "extdata/bristol_blank.png", package = "mobilityHubTools")
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
  image <- lapply(icons_to_include_in_header, function (x) grid::rasterGrob(magick::image_read(x)))
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
  gs[[4]] <- grid::grobTree(grid::rectGrob(gp=gpar(fill="white", lty = 1)),
                            gridExtra::grid.arrange(rasterGrob(magick::image_read(
                              directions_image)),
                              ncol = 1))

  gs[[5]] <- map_local
  # Insert facilities description image below hub name and icons
  gs[[6]] <- map_regional
  gs[[7]] <- rasterGrob(magick::image_read(points_of_interest_image))
  gs[[8]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="white", lty = "blank")),
                            gridExtra::grid.arrange(rasterGrob(magick::image_read(
                              logo_image)),
                              ncol = 1)
  )




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
