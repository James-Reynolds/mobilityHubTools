#' Builds a totem pedestral sign in the style of Bristol mobility hubs
#'
#' @param file_to_save_to a character string of the file to save the png to
#' @param hub_name_text the name of the hub
#' @param directions_image a character string of the location of an image showing directions
#' @param map_local the output of the bristol_map_local function
#' @param facilities_image a character string of the location of an image showing details of the hub facilities
#' @param map_regional the output of the bristol_map_regional function
#' @param logo_image a character string of the location of an image showing agency logos
#'
#' @returns nothing, but outputs a png to the named file
#' @export
#'
#' @examples
bristol_totem_pedestal <- function(
    file_to_save_to = "layout_test.png",
    hub_name_text = "Gainsborough Square",
    directions_image = "inst/extdata/bristol_greensborough_directions.png",
    map_local = map_local_test,
    facilities_image = "inst/extdata/bristol_facilities.png",
    map_regional = map_regional_test,
    logo_image = "inst/extdata/bristol_blank.png"
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
image <- lapply(icons_to_incude_in_header, function (x) grid::rasterGrob(magick::image_read(x)))
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
gs[[3]] <- grid::grobTree(grid::rectGrob(gp=gpar(fill="darkgreen", lty = 1)),
                          gridExtra::grid.arrange(rasterGrob(magick::image_read(
                      directions_image)),
                      ncol = 1)
                    )

gs[[4]] <- map_local
# Insert facilities description image below hub name and icons
gs[[5]] <- rasterGrob(magick::image_read(facilities_image))

gs[[6]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)))
gs[[7]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = 1)))
gs[[8]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="darkgreen", lty = "blank")),
                    gridExtra::grid.arrange(rasterGrob(magick::image_read(
                      logo_image)),
                      ncol = 1)
)
gs[[9]] <- map_regional



# 24 rows, each roughly 100mm high
# 5 columns, each roughly 200mm wide
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
             c(6,6,7,8,8))
gridExtra::grid.arrange(grobs = gs, layout_matrix = lay)

grDevices::dev.copy(png, "layout_test.png", width = 1000, height = 2400)
grDevices::dev.off()


}


