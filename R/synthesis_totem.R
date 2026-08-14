synthesis_totem <- function(
    file_to_save_to = "layout_test.pdf",
    element1_what_is_this_top_text = "Mobility Hub / Local Travel Point",
    element2_where_is_it_top_text = "Gainsborough Square",
    element2_where_is_it_top_text_detail = "Lockleaze, Bristol, BS7 9AP",

# icons from osmic-master https://github.com/gmgeo/osmic
# electric scooter icon from    https://commons.wikimedia.org/wiki/File:Tabler-icons_scooterrom https://www.svgrepo.com/svg/450115/e-scooter
# accessible toilet icon from https://commons.wikimedia.org/wiki/File:RWBA_Behinderten-WC.svg

       element3_what_is_here_top = list(
      system.file(
        "extdata/bus_stop.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/bicycle_parking.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/bicycle_repair_station.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/e-scooter-svgrepo-com.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/bicycle-electric-2.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/toilets.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/RWBA_Behinderten-WC.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/No_image.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/No_image.svg",
        package = "mobilityHubTools"),
      system.file(
        "extdata/No_image.svg",
        package = "mobilityHubTools")),

    element4_up_what_is_nearby = "^ Buses: Gainsborough Square Stop B (Routes 24, 72, 73 eastbound)",
    element4_left_what_is_nearby = "\U2190 Buses: Gainsborough Square Stop A (Routes 24, 77 northbound)\n\U2190 North Bristol Advice Centre",
    element4_right_what_is_nearby = "Buses: Cameron Walk stop (Route 24, 72 westbound) \U2192\n St James Church \U2192",
    element4_layout_matrix = rbind(c(1,1,1,1,1),
                                   c(2,2,2,2,2),
                                   c(3,3,3,3,3)),
    element5_regional_map = synthesis_regional_map(),




    element6_what_is_this_bottom_text = "What is this: \nWelcome to this Mobility Hub / Local Travel Point,\n a location where shared mobility and other services\n offer a range of options to support sustainable travel. \n\nWhat is here: (see left)\nThis mobility hub includes nearby bus stops,\n bicycle parking and a repair station, \n shared scooters and electric bikes, and toilets (in the Hub).",
    element7_hub_elements_map = synthesis_hub_elements_map(),
    element8_what_is_this_bottom_text = "How can you travel from here: (see maps below) \nBuses 24, 77 northbound leave from Gainsborough Square Stop A (left and across the square),\nBuses 24, 72 and 73 eastbound leave from Gainsbourough Square Stop B (in front of you).\n Buses 24, 72 westbound leave from a stop in Cameron Walk (to your right)  \n Ashley Down and Filton Abbey Wd Railway Stations are around a 15 minute walk away (see map below centre).\n The Frome Valley Greenway and Concorde Way are also nearby (see map below right).",
    element9_hub_surrounds_map = synthesis_hub_surrounds_map(),
    element10_what_is_this_bottom_text = "What is nearby: (see maps above)\nThe Hub is accross the street behind you.  \nThe North Bristol Advice Centre is across Gainsborough Square to your left.\nShops, food and services are also across Gainborough Square.\nTo get to St James Church go right along Cameron Walk\n until you get to Romney Avenue.
\n \nWhere am I:\n Gainsborough Square, Lockleaze, Bristol BS7 9AP.\nThe latitude is 51.4904 and the longitude is -2.5627.\nwhat3words.com calls this location hero.blend.sock\n\n ",
    element14_acknowledgements_text = "Map data from OpenStreetMap, available under the Open Database License. \n © OpenStreetMap contributors. See openstreetmap.org/copyright \n \n Map tiles from: OSM standard layer © OpenStreetMap contributors.; \n and Thunderforest Open CycleMap and Transport layers by Andy Allan, https://www.thunderforest.com/"





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
    grid::textGrob(element8_what_is_this_bottom_text,
                   x = 0.01, y=0.05, gp = grid::gpar(fontsize = 14), just = c("left", "bottom"))))


gs[[9]] <- element9_hub_surrounds_map

gs[[10]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element10_what_is_this_bottom_text,
                   x = 0.01, y=0.95, gp = grid::gpar(fontsize = 14), just = c("left", "top"))))


gs[[11]] <- synthesis_hub_surrounds_map(
  map_limits = 100,
  annotation_map_zoom = 17,
  annotation_map_type = "osmtransport",
  highlight_building = "")

gs[[12]] <- synthesis_hub_surrounds_map(
  map_limits = 2000,
  annotation_map_zoom = 13,
  annotation_map_type = "osmtransport",
  highlight_building = "")


gs[[13]] <- synthesis_hub_surrounds_map(
  map_limits = 2000,
  annotation_map_zoom = 13,
  annotation_map_type = "opencycle",
  highlight_building = "")

gs[[14]] <- gridExtra::grid.arrange(
  grid::grobTree(
    grid::rectGrob(
      gp=grid::gpar(fill="white")),
    grid::textGrob(element14_acknowledgements_text,
                   x = 0.95, y=0.0, gp = grid::gpar(fontsize = 10), just = c("right", "bottom"))))


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
