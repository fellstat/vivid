gen_uuid <- function(){
  paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
}

vid_fun <- function(uuid){
  if(missing(uuid))
    uuid <- gen_uuid()
  fun <- function(x) paste0(uuid,"-", x)
  attr(fun, "uuid") <- uuid
  fun
}

## Evaluates an expression in global environment and returns the result and uuid.
geval <- local(function(expr, uuid, substitute = FALSE, envir = .GlobalEnv, enclos = baseenv(), ...) {
  if (substitute) expr <- substitute(expr)
  val <- try(eval(expr, envir = envir, enclos = enclos))
  list(val, uuid)
})


vivid_globals <- function(){
  .globals
}
