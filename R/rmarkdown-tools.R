

run_chunk <- function(chunk, envir=parent.env()){
  if(is.null(knitr::opts_knit$get("output.dir")))
    knitr::opts_knit$set(output.dir = getwd())
  rmd <- knitr::knit(text=chunk, envir=envir)
  markdown::markdownToHTML(text=rmd, fragment.only=TRUE)
}

parse_chunk_r_code <- function(chunk){
  res <- list()
  lines <- knitr:::split_lines(chunk)
  i <- 1
  while(i < length(lines) + 1){
    if(grepl("^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+.*)\\}\\s*$", lines[i])){
      is_r <- grepl("^[\t >]*```+\\s*\\{(r+.*)\\}\\s*$", lines[i])
      j <- i + 1
      block <- list()
      while(!grepl("^[\t >]*```+\\s*$", lines[j]) && j < length(lines)){
        if(is_r)
          block[[j-i]] <- lines[j]
        else
          block[[j-i]] <- paste("#",lines[j])
        j <- j + 1
      }
      res <- c(res, block)
        # res[[length(res)+1]] <- unlist(block)
      i <- j
    }else{
      inline <- stringr::str_match_all(lines[i], "(?<!(^|\n)``)`r[ #]([^`]+)\\s*`")[[1]][,3]
      if(stringr::str_trim(lines[i]) != "")
        res[[length(res)+1]] <- paste("#", lines[i])
      else
        res[[length(res)+1]] <- ""
      if(length(inline) > 0){
        res[[length(res)+1]] <- inline
      }
    }
    i <- i + 1
  }
  paste(unlist(res), collapse="\n")
}

