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

texasCi <- function(){
  library(shinyTree)
  library(ggplot2)
  
  envirs=base::search()
  Tree0s=list()
  
  l1stopened=TRUE
  for (envir in envirs) {
    pkgname=envir
    if(substr(envir,1,8)=="package:"){pkgname=substr(envir,9,1000)}
    if(substr(envir,1,6)=="tools:"){pkgname=substr(envir,7,1000)}
    ccs=sapply(lapply(ls(as.environment(envir)), get), is.data.frame)
    dds=ls(as.environment(envir))[(ccs==TRUE)]
    Tree1s=list()
	l2stopened=TRUE
    for (dd in dds) {
      Tree2s=list()
      if(substr(envir,1,8)=="package:"){
        TreeA=list() 
		TreeAt=list()		
        try(eval(parse(text=paste0("TreeA=(names(",pkgname,"::", dd,"))"))))
		try(eval(parse(text=paste0("TreeAt=(sapply(",pkgname,"::", dd,", class))"))))
        for (kk in 1:length(TreeA)){
		   Treea=TreeA[kk]
		   Treeat=TreeAt[kk]
          try(eval(parse(text=paste0("Tree2s=c(Tree2s, '",Treea,"'=list(structure(\"",Treea,"\",sticon='fa fa-tag fa-tag-",select_color(Treeat),"')))")))) #,stopened=TRUE
        }
      } else if (substr(envir,1,6)=="tools:"){
        
      } else if (envir==".GlobalEnv"){
        TreeA=list()
		TreeAt=list() 
        
        try(eval(parse(text=paste0("TreeA=(names(",".GlobalEnv","$", dd,"))"))))
		try(eval(parse(text=paste0("TreeAt=(sapply(",".GlobalEnv","$", dd,", class))"))))
        #TreeA=datasets()
        for (kk in 1:length(TreeA)){
		   Treea=TreeA[kk]
		   Treeat=TreeAt[kk]
          try(eval(parse(text=paste0("Tree2s=c(Tree2s, '",Treea,"'=list(structure(\"",Treea,"\",sticon='fa fa-tag fa-tag-",select_color(Treeat),"')))")))) #,stopened=TRUE
        }
      }			
      if(length(Tree2s))try(eval(parse(text=paste0("Tree1s=c(Tree1s,'",dd,"'=list(structure(Tree2s,sttype='df-node',sticon='tags',stopened=",toString(l2stopened),")))"))))
	  l2stopened=FALSE
    }
    if(length(Tree1s))try(eval(parse(text=paste0("Tree0s=c(Tree0s,'",pkgname,"'=list(structure(Tree1s,sttype='pkg-node',sticon='fas fa-box',stopened=",toString(l1stopened),")))"))))
	l1stopened=FALSE
  }
  Tree0s
}

select_color <- function (intype){
	if (toString(intype)=='integer'){
		 'brown'
	}else if (toString(intype)=='numeric'){
		 'orange'
	}else if (toString(intype)=='character'){
		 'green'
	}else{
		 'black'
	}
}