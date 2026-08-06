
### from https://github.com/ropensci/osmdata/blob/main/vignettes/query-split.Rmd
split_bbox <- function (bbox, grid = 2, eps = 0.05) {
  xmin <- bbox ["x", "min"]
  ymin <- bbox ["y", "min"]
  dx <- (bbox ["x", "max"] - bbox ["x", "min"]) / grid
  dy <- (bbox ["y", "max"] - bbox ["y", "min"]) / grid
  bboxl <- list ()

  for (i in 1:grid) {
    for (j in 1:grid) {
      b <- matrix (c (
        xmin + ((i - 1 - eps) * dx),
        ymin + ((j - 1 - eps) * dy),
        xmin + ((i + eps) * dx),
        ymin + ((j + eps) * dy)
      ),
      nrow = 2,
      dimnames = dimnames (bbox)
      )
      bboxl <- append (bboxl, list (b))
    }
  }
  bboxl
}


queuing_function <- function(
    layer_to_download = "amenity"){

queue <- split_bbox (bb)
result <- list()

while (length (queue) > 0) {
  print (queue [[1]])
  opres <- NULL
  opres <- try ({
    osmdata::opq (bbox = queue [[1]], timeout = 25) |>
      osmdata::add_osm_feature (key = layer_to_download ) |>
      osmdata::osmdata_sf ()
  })

  if (class (opres) [1] != "try-error") {
    result <- append (result, list (opres))
    queue <- queue [-1]
  } else {
    bboxnew <- split_bbox (queue [[1]])
    queue <- append (bboxnew, queue [-1])
  }
}
return(do.call (c, result))
}
