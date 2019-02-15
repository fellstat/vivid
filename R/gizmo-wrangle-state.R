


#wrangle_get_state <- function(input, output, session){
#  state <- list()
#  elements <- session$userData[[session$ns("state")]]$list
#  for(i in seq_along(elements)){
#    state[[i]] <- list(
#      name=elements[[i]]$name,
#      state=elements[[i]]$get_state()
#    )
#  }
#  state
#}

#wrangle_restore_state <- function(input, output, session, state){
# #TODO
#  invisible()
#}
