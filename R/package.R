#' Vivid
#' @docType package
#' @name vivid-package
#' @author Ian Fellows \email{ian@fellstat.com}
#' @import shiny parallel
NULL



.onLoad <- function(...) {
  addResourcePath("vivid", system.file("www", package = "vivid"))
}
