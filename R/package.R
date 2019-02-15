#' Vivid
#' @docType package
#' @name vivid-package
#' @author Ian Fellows \email{ian@fellstat.com}
#' @import shiny parallel
NULL



.onLoad <- function(libname, pkgname) {
  addResourcePath("vivid", system.file("www", package = "vivid"))

  register_gizmo(
    gizmo_name="wrangle_data",
    ui=wrangle_ui,
    server=wrangle_server,
    lib="vdata",
    get_state=wrangle_get_state,
    restore_state=wrangle_restore_state,
    opts=list()
  )
}


.onDetach <- function(libpath){
  if(!is.null(.globals$server_r)){
    .globals$server_r$stop()
    .globals$server_r$stop_monitor()
    globals$server_r <- NULL
  }
  if(!is.null(.globals$standalone_server_r)){
    .globals$standalone_server_r$stop()
    .globals$standalone_server_r$stop_monitor()
    globals$standalone_server_r <- NULL
  }
}
