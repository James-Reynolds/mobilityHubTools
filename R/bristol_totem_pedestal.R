#' Builds a totem pedestral sign in the style of Bristol mobility hubs
#'
#' @param file_to_save_to a character string of the file to save the pdf to
#' @param hub_name_text the name of the hub
#' @param icons_to_include_in_header a list of 10 strings of the location of the icons to include in the header, including blanks
#' @param directions_image a character string of the location of an image showing directions
#' @param map_local ggplot object, output of bristol_map_local function
#' @param facilities_image a character string of the location of an image showing details of the hub facilities
#' @param map_regional the output of the bristol_map_regional function
#' @param acknowledgements_text a character string including acknowledgements and copyright details.
#'
#' @returns nothing, but outputs a pdf to the named file
#' @export
#'
#' @examples
#' bristol_totem_pedestal(
#' file_to_save_to = "gainsborough_bristol-style_totem.pdf",
#' hub_name_text = "Gainsborough Square")

bristol_totem_pedestal <- function(
    file_to_save_to,
    hub_name_text,
    icons_to_include_in_header = list(
      system.file("extdata/No_image.svg", package = "mobilityHubTools"),
      system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle_parking.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle_repair_station.svg", package = "mobilityHubTools"),
      system.file("extdata/e-scooter-svgrepo-com.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle-electric-2.svg", package = "mobilityHubTools"),
      system.file("extdata/toilets.svg", package = "mobilityHubTools"),
      system.file("extdata/RWBA_Behinderten-WC.svg", package = "mobilityHubTools"),
      system.file("extdata/No_image.svg", package = "mobilityHubTools"),
      system.file("extdata/No_image.svg", package = "mobilityHubTools")),
    directions_image = system.file(
      "extdata/bristol_greensborough_directions.png", package = "mobilityHubTools"),
    map_local = bristol_local_map(),
    facilities_image =  system.file(
      "extdata/bristol_facilities.png", package = "mobilityHubTools"),
    map_regional = bristol_regional_map(),
    acknowledgements_text = "Map data from OpenStreetMap, available under the Open Database License. © OpenStreetMap contributors. See openstreetmap.org/copyright \n \nIcons from https://github.com/gmgeo/osmic https://www.svgrepo.com/svg/450115/e-scooter \nand https://commons.wikimedia.org/wiki/File:RWBA_Behinderten-WC.svg"
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
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text(hub_name_text, y = 0.5)
grid::upViewport()
grid::pushViewport(hub_facilities_viewport)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("Hub icons go here", y = 0.5)
grid::upViewport()
grid::pushViewport(direction_information)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("Direction information goes here", y = 0.5)
grid::upViewport()
grid::pushViewport(local_map_viewport)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("Local Map goes here", y = 0.5)
grid::upViewport()
grid::pushViewport(hub_facilities_viewport_details)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("Hub facility details", y = 0.5)
grid::upViewport()
grid::pushViewport(regional_map_viewport)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("Area Map goes here", y = 0.5)
grid::upViewport()
grid::pushViewport(logos_viewport)
grid::grid.rect(gp = grid::gpar(col = "grey"))
grid::grid.text("logos_viewport etc", y = 0.5)
grid::popViewport()



grid::grid.rect(gp = grid::gpar(lty = "blank"))

r <- grid::rectGrob(gp=grid::gpar(fill="darkgreen"))

#Define layout
gs <- lapply(1:9, function(ii)
  grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
gridExtra::grid.arrange(grobs=gs, ncol=9,
             top="top label", bottom="bottom\nlabel",
             left="left label", right="right label")
grid::grid.rect(gp=grid::gpar(fill=NA))


#Define hub name at top
hub_name_viewport <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen")), grid::textGrob(hub_name_text, gp=grid::gpar(fontsize=60, col="white", fontface="bold", lty = "blank")))
gs[[1]] <- gridExtra::grid.arrange(hub_name_viewport)


#Wrangle icons below hub name
image <- lapply(icons_to_include_in_header, function (x) svgparser::read_svg(x))
grob_1 <- image[[1]]
grob_2 <- image[[2]]
grob_3 <- image[[3]]
grob_4 <- image[[4]]
grob_5 <- image[[5]]
grob_6 <- image[[6]]
grob_7 <- image[[7]]
grob_8 <- image[[8]]
grob_9 <- image[[9]]
grob_10 <- image[[10]]

gs[[2]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)),
                          gridExtra::grid.arrange(grob_1, grob_2, grob_3, grob_4, grob_5, grob_6,
                                 grob_7, grob_8, grob_9, grob_10,
                                 ncol = 10)
                    )

# Insert directions image below hub name and icons
gs[[3]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)),
                          gridExtra::grid.arrange(grid::rasterGrob(magick::image_read(
                      directions_image)),
                      ncol = 1)
                    )

gs[[4]] <- map_local
# Insert facilities description image below hub name and icons
gs[[5]] <- grid::rasterGrob(magick::image_read(facilities_image))

gs[[6]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)))
gs[[7]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)))
gs[[8]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="darkgreen")),
    grid::textGrob(acknowledgements_text,
                   x = 0.95, y=0.05, gp = grid::gpar(fontsize = 10, col = "white"), just = c("right", "bottom"))))
gs[[9]] <- map_regional



# 24 rows, each roughly 42.5mm high, total height 1.02m, 40.8in
# 5 columns, each roughly 85mm wide, total width 42.5cm, 17 inches
# 10 rows x 5 columns is a square
lay <- rbind(c(1,1,1,1,1),
             c(1,1,1,1,1),
             c(2,2,2,2,2),
             c(3,3,3,3,3),
             c(3,3,3,3,3),
             c(3,3,3,3,3),
             c(3,3,3,3,3),
             c(3,3,3,3,3),
             c(3,3,3,3,3),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(4,4,4,4,4),
             c(5,5,5,9,9),
             c(5,5,5,9,9),
             c(5,5,5,9,9),
             c(5,5,5,9,9),
             c(6,7,8,8,8))
gridExtra::grid.arrange(grobs = gs, layout_matrix = lay)
grDevices::dev.copy2pdf(file = file_to_save_to, width = 17, height = 40.8, fonts = NULL)
grDevices::dev.off()


}


