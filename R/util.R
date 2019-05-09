gen_uuid <- function(){
  aa=paste(sample(c(letters[1:6]),1,replace=TRUE),collapse="")
  bb=paste(sample(c(letters[1:6],0:9),30-1,replace=TRUE),collapse="")
  paste0(aa,bb)
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


gevalQ <- local(function(expr, uuid, queue, substitute = FALSE, envir = .GlobalEnv, enclos = baseenv(), ...) {
  if (substitute) expr <- substitute(expr)
  val <- try(eval(expr, envir = envir, enclos = enclos))
  ret <- list(val, uuid)
  queue$producer$fire("callback_exec", ret)
  ret
})


vivid_globals <- function(){
  .globals
}


.get_objects <- function(filter=NULL, envir=.GlobalEnv) {
  if(is.data.frame(envir))
    objects <- names(envir)
  else
    objects <- ls(envir = envir)
  cls <- list()
  if (length(objects) > 0){
    for (i in 1:length(objects)) {
      #d <- get(objects[i], envir = envir)
      d <- envir[[objects[i]]]
      cls[[i]] <- class(d)

    }
  }
  if(!is.null(filter)){
    is_of_cls <-  unlist(lapply(cls, function(x) any(x %in% filter)))
    objects <- objects[is_of_cls]
    cls <- cls[is_of_cls]
  }
  tibble::tibble(objects=objects, classes=cls)
}

#' @export
.get_data <- function(envir=.GlobalEnv) .get_objects("data.frame")

format_vars <- function(vars){
  paste0(vars, collapse = ", ")
}

# quickly clean a string
#' @importFrom stringi stri_trans_general stri_replace_all_regex
clean_string <- function(str) {
  str <- stri_trans_general(str = str, id = "Latin-ASCII")
  str <- stri_replace_all_regex(str = str, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
  return(str)
}

instr <- function(str1,str2,startpos=1,n=1){
    aa=unlist(strsplit(substring(str1,startpos),str2))
    if(length(aa) < n+1 ) return(0);
    return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}
