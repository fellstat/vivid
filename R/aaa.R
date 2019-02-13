.globals <- new.env(parent = emptyenv())

.globals$remote_r <- NULL

.globals$standalone_server_r <- NULL

.globals$server_r <- NULL

.globals$gizmos <- list()

.globals$vivid_server <- list()

.globals$vivid_standalone_server <- list()

.globals$standalone_states <- list()

.globals$is_execution_process <- TRUE


vivid_globals <- function() .globals
