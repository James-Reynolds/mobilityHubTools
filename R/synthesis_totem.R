#' Builds a totem pedestal that is a sythesis version of various styles
#'
#' @param file_to_save_to a character string of the file to save the png to
#' @param element1_what_is_this_top_text a character string displayed at the top of the totem
#' @param element2_where_is_it_top_text a character string of the name of the hub location
#' @param element2_where_is_it_top_text_detail a character string describing the location of the hub (address details)
#' @param element3_what_is_here_top a list of 10 strings of the location of the icons to include in the header, including blanks
#' @param element4_up_what_is_nearby a character string (with arrows) to indicate what is nearby in front of the totem
#' @param element4_left_what_is_nearby a character string (with arrows) to indicate what is nearby to the left of the totem
#' @param element4_right_what_is_nearby a character string (with arrows) to indicate what is nearby to the right of the totem
#' @param element4_layout_matrix a matrix of characters setting the layout of the up, left and right directions. Defaults to 3 lines
#' @param element5_regional_map a ggplot object
#' @param element6_what_is_this_bottom_text a character string describing what the hub is and what is nearby
#' @param element7_hub_elements_map a ggplot object
#' @param element8_how_can_you_travel a character string describing travel options from the hub.
#' @param element9_hub_surrounds_map a ggplot object
#' @param element10_what_is_nearby_bottom_text a character string describing what is nearby and other relevant details.
#' @param element11_transport_map_small a ggplot object
#' @param element12_transport_map_large a ggplot object
#' @param element13_cycle_map a ggplot object
#' @param element14_acknowledgements_text a character string including acknowledgements and copyright details.
#'
#' @returns nothing, but outputs a pdf to the named file
#' @export
#'
#' @examples
synthesis_totem <- function(
    file_to_save_to,
    element1_what_is_this_top_text = "Mobility Hub / Local Travel Point",
    element2_where_is_it_top_text,
    element2_where_is_it_top_text_detail,
    element3_what_is_here_top = list(
      system.file("extdata/bus_stop.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle_parking.svg", package = "mobilityHubTools"),
      system.file("extdata/bicycle_repair_station.svg", package = "mobilityHubTools"),
      system.file("extdata/e-scooter-svgrepo-com.svg",package = "mobilityHubTools"),
      system.file("extdata/bicycle-electric-2.svg", package = "mobilityHubTools"),
      system.file("extdata/toilets.svg", package = "mobilityHubTools"),
      system.file("extdata/RWBA_Behinderten-WC.svg", package = "mobilityHubTools"),
      system.file("extdata/No_image.svg", package = "mobilityHubTools"),
      system.file("extdata/No_image.svg", package = "mobilityHubTools"),
      system.file("extdata/No_image.svg", package = "mobilityHubTools")),
    element4_up_what_is_nearby,
    element4_left_what_is_nearby,
    element4_right_what_is_nearby,
    element4_layout_matrix = rbind(c(1,1,1,1,1),
                                   c(2,2,2,2,2),
                                   c(3,3,3,3,3)),
    element5_regional_map = synthesis_regional_map(),
    element6_what_is_this_bottom_text = "What is this: \nWelcome to this Mobility Hub / Local Travel Point,\n a location where shared mobility and other services\n offer a range of options to support sustainable travel. \n\nWhat is here: (see left)\nThis mobility hub includes...",
    element7_hub_elements_map = synthesis_hub_elements_map(),
    element8_how_can_you_travel = "How can you travel from here: (see maps below) \n By transit ....\n On foot...\n By bike....(see map below right).",
    element9_hub_surrounds_map = synthesis_hub_surrounds_map(),
    element10_what_is_nearby_bottom_text = "What is nearby: (see maps above)\n

\n \nWhere am I:\n ...\nThe latitude is ... and the longitude is ....\nwhat3words.com calls this location ...\n\n ",
    element11_transport_map_small = synthesis_hub_surrounds_map(
      map_limits = 100, annotation_map_zoom = 17,
      annotation_map_type = "osmtransport", highlight_building = ""),
    element12_transport_map_large = synthesis_hub_surrounds_map(
      map_limits = 2000, annotation_map_zoom = 13,
      annotation_map_type = "osmtransport", highlight_building = ""),
    element13_cycle_map = synthesis_hub_surrounds_map(
      map_limits = 2000, annotation_map_zoom = 13,
      annotation_map_type = "opencycle", highlight_building = ""),
    element14_acknowledgements_text = "Map data from OpenStreetMap, available under the Open Database License. \n © OpenStreetMap contributors. See openstreetmap.org/copyright \n \n Map tiles from: OSM standard layer © OpenStreetMap contributors.; \n and Thunderforest Open CycleMap and Transport layers by Andy Allan, https://www.thunderforest.com/ \n \nIcons from https://github.com/gmgeo/osmic https://www.svgrepo.com/svg/450115/e-scooter \n and https://commons.wikimedia.org/wiki/File:RWBA_Behinderten-WC.svg"
)

{
  r <- grid::rectGrob(gp=grid::gpar(fill="white"))

  #Define layout
  gs <- lapply(1:14, function(ii)
    grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
  gridExtra::grid.arrange(grobs=gs, ncol=14,
                          top="top label", bottom="bottom\nlabel",
                          left="left label", right="right label")
  grid::grid.rect(gp=grid::gpar(fill=NA))


#Define what this is at top ELEMENT 1 - WHAT IS THIS
gs[[1]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element1_what_is_this_top_text,
                   gp=grid::gpar(
                     fontsize=60,
                     col="black",
                     fontface="bold",
                     lty = "blank"))))

#Define ELEMENT 2 - Where is this
grobs_for_where_is_it_top <- list()
grobs_for_where_is_it_top[[1]] <- grid::grobTree(
  grid::rectGrob(
    gp=grid::gpar(fill="white", col = NA)),
  grid::textGrob(element2_where_is_it_top_text,
                 gp=grid::gpar(
                   fontsize=100,
                   col="black",
                   fontface="bold",
                   lty = "blank")))

grobs_for_where_is_it_top[[2]] <-
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white", col = NA)),
    grid::textGrob(element2_where_is_it_top_text_detail,
                   gp=grid::gpar(
                     fontsize=60,
                     col="black",
                     fontface="bold",
                     lty = "blank")))
gs[[2]] <- gridExtra::grid.arrange(
  grobs = grobs_for_where_is_it_top,
  layout_matrix = rbind(c(1,1,1,1,1),
                          c(1,1,1,1,1),
                          c(2,2,2,2,2)))

#Wrangle icons below hub name ELEMENT 3
grob_1 <- svgparser::read_svg(element3_what_is_here_top[[1]])
grob_2 <- svgparser::read_svg(element3_what_is_here_top[[2]])
grob_3 <- svgparser::read_svg(element3_what_is_here_top[[3]])
grob_4 <- svgparser::read_svg(element3_what_is_here_top[[4]])
grob_5 <- svgparser::read_svg(element3_what_is_here_top[[5]])
grob_6 <- svgparser::read_svg(element3_what_is_here_top[[6]])
grob_7 <- svgparser::read_svg(element3_what_is_here_top[[7]])
grob_8 <- svgparser::read_svg(element3_what_is_here_top[[8]])
grob_9 <- svgparser::read_svg(element3_what_is_here_top[[9]])
grob_10 <- svgparser::read_svg(element3_what_is_here_top[[10]])

gs[[3]] <- grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="white", lty = 1)),
                          gridExtra::grid.arrange(grob_1, grob_2, grob_3, grob_4, grob_5, grob_6,
                                                  grob_7, grob_8, grob_9, grob_10,
                                                  ncol = 10)
)


  grobs_for_element4_what_is_nearby_top <- list()

  grobs_for_element4_what_is_nearby_top[[1]] <- grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white", col = NA)),
    grid::textGrob(element4_up_what_is_nearby,
                   gp=grid::gpar(
                     fontsize=30,
                     col="black",
                     fontface="bold",
                     lty = "blank"))
    )
  grobs_for_element4_what_is_nearby_top[[2]] <- grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white", col = NA)),
    grid::textGrob(element4_left_what_is_nearby,
                   x = 0.05,
                   y = 0.9,
                   just = c("left", "top"),
                   gp=grid::gpar(
                     fontsize=30,
                     col="black",
                     fontface="bold",
                     lty = "blank")))
  grobs_for_element4_what_is_nearby_top[[3]] <- grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white", col = NA)),
    grid::textGrob(element4_right_what_is_nearby,
                   x = 0.95,
                   y = 0.1,
                   just = c("right", "bottom"),
                   gp=grid::gpar(
                     fontsize=30,
                     col="black",
                     fontface="bold",
                     lty = "blank")))


gs[[4]] <- gridExtra::grid.arrange(
  grobs = grobs_for_element4_what_is_nearby_top,
  layout_matrix = element4_layout_matrix)

gs[[5]] <- element5_regional_map

#Define ELEMENT 6 - What is this and Where is this (close up)
gs[[6]] <- gridExtra::grid.arrange(
    grid::grobTree(
      grid::rectGrob(
        gp=grid::gpar(fill="white")),
      grid::textGrob(element6_what_is_this_bottom_text,
                     x = 0.01, y=0.95, gp = grid::gpar(fontsize = 18), just = c("left", "top"))))


gs[[7]] <- element7_hub_elements_map

gs[[8]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element8_how_can_you_travel,
                   x = 0.01, y=0.05, gp = grid::gpar(fontsize = 14), just = c("left", "bottom"))))


gs[[9]] <- element9_hub_surrounds_map

gs[[10]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element10_what_is_nearby_bottom_text,
                   x = 0.01, y=0.95, gp = grid::gpar(fontsize = 14), just = c("left", "top"))))


gs[[11]] <- element11_transport_map_small

gs[[12]] <- element12_transport_map_large

gs[[13]] <- element13_cycle_map

gs[[14]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element14_acknowledgements_text,
                   x = 0.95, y=0.05, gp = grid::gpar(fontsize = 10), just = c("right", "bottom"))))


  # 24 rows, each roughly 42mm high, total height 1092mm, 43.6 inches
  # 5 columns, each roughly 84mm wide, total width 420mm, 16.8 inches
  # Two fit side-by-side on a A0 piece of paper, with a little bit to spare
  # A0 is 841mm wide and 1189mm high, or 33.1 x 46.8in
  # 10 rows x 5 columns is a square
              # first group only four
  lay <- rbind(c(1,1,1,1,1),
               c(2,2,2,2,2),
               c(2,2,2,2,2),
               c(3,3,3,3,3),
               # second group of five
               c(4,4,4,4,4),
               c(4,4,4,4,4),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               # third group of five
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               # fourth group of five
               c(5,5,5,5,5),
               c(5,5,5,5,5),
               c(7,7,7,6,6),
               c(7,7,7,6,6),
               c(7,7,7,9,9),
               # fifth group of five
               c(7,7,7,9,9),
               c(7,7,7,9,9),
               c(7,7,7,9,9),
               c(8,8,8,10,10),
               c(11,12,13,10,10),
               c(11,12,13,14,14)

               )
  gridExtra::grid.arrange(grobs = gs, layout_matrix = lay)
  grDevices::dev.copy2pdf(file = file_to_save_to, width = 16.8, height = 43.6, fonts = NULL)
  grDevices::dev.off()


}
