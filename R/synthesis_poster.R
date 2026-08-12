synthesis_poster <- function(
    file_to_save_to = "layout_test.pdf",
    element1_what_is_this_top_text = "Mobility Hub / Local Travel Point",
    element2_where_is_it_top_text = "Gainsborough Square",
    element2_where_is_it_top_text_detail = "Lockleaze, Bristol, BS7 9AP",

# icons from osmic-master
# electric scooter icon from    https://commons.wikimedia.org/wiki/File:Tabler-icons_scooter-electric.svg
# e-scooter icon from https://www.svgrepo.com/svg/450115/e-scooter
# accessible toilet icon from https://commons.wikimedia.org/wiki/File:RWBA_Behinderten-WC.svg
# arrows from https://www.svgrepo.com/svg/31619/next-arrow
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
    element4_right_what_is_nearby = "Buses: Cameron Walk stop (Route 24, 72 westbound) \U2192\n LockLeaze Neighbourhood Trust \U2192",
    element4_layout_matrix = rbind(c(1,1,1,1,1),
                                   c(2,2,2,2,2),
                                   c(3,3,3,3,3)),
    element5_regional_map = synthesis_regional_map(),




    element6_what_is_this_bottom_text = " What is this: \n Welcome to this Mobility Hub / Local Travel Point,\n a location were shared and other mobility services\n offer a range of travel options. \n\n Where is it:\n Gainsborough Square, Lockleaze, Bristol BS7 9AP.\n The latitude is 51.4904 and the longitude is -2.5627.\n what3words.com calls this location hero.blend.sock\n"

)

{
  r <- grid::rectGrob(gp=grid::gpar(fill="white"))

  #Define layout
  gs <- lapply(1:11, function(ii)
    grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
  gridExtra::grid.arrange(grobs=gs, ncol=11,
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


gs[[7]] <- map


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
               c(6,6,8,8,8),
               c(6,6,8,8,8),
               c(7,7,8,8,8),
               # fifth group of five
               c(7,7,8,8,8),
               c(7,7,8,8,8),
               c(7,7,8,8,8),
               c(9,9,9,9,9),
               c(9,9,9,9,9),
               c(10,10,10,11,11)

               )
  gridExtra::grid.arrange(grobs = gs, layout_matrix = lay)
  grDevices::dev.copy2pdf(file = file_to_save_to, width = 16.8, height = 43.6, fonts = NULL)
  grDevices::dev.off()


}
