

run_chunk <- function(chunk,  envir){
  if(is.null(knitr::opts_knit$get("output.dir")))
    knitr::opts_knit$set(output.dir = getwd())
  knitr::opts_knit$set(rmarkdown.pandoc.to="html")
  md <- knitr::knit(text=chunk, envir=envir, quiet=TRUE)
  dir <- tempfile()
  on.exit(try(unlink(dir, recursive=TRUE)))
  dir.create(dir)
  cat(md, file=paste0(dir,"/input.md"))
  rmarkdown::render(
    input=paste0(dir,"/input.md"),
    output_format=rmarkdown::html_document(
      toc=FALSE,
      number_sections = FALSE,
      section_divs=FALSE,
      template=NULL,
      theme=NULL,
      self_contained = FALSE
    )
  )
  result <- paste0(readLines(paste0(dir,"/input.html")), collapse="\n")
  r <- sub(".*</title>", "", result)
  r <- sub("<body>", "", r)
  r <- sub("</body>", "", r)
  r <- sub("</head>", "", r)
  r <- sub("</html>", "", r)
  r
  #html <- markdown::markdownToHTML(text=rmd, fragment.only=TRUE)
  dependancies <- knitr:::knit_meta("html_dependency")
  html <- shiny::HTML(r)
  htmltools::htmlDependencies(html) <- dependancies
  html
}

# run_chunk <- function(chunk, envir=parent.env()){
#   #if(is.null(knitr::opts_knit$get("output.dir")))
#   #  knitr::opts_knit$set(output.dir = getwd())
#   #rmd <- knitr::knit(text=chunk, envir=envir, quiet=TRUE)
#   #markdown::markdownToHTML(text=rmd, fragment.only=TRUE)
#   dir <- tempfile()
#   on.exit(try(unlink(dir, recursive=TRUE)))
#   dir.create(dir)
#   cat(chunk, file=paste0(dir,"/input.Rmd"))
#   rmarkdown::render(
#     input=paste0(dir,"/input.Rmd"),
#     output_format=rmarkdown::html_document(toc=FALSE,number_sections = FALSE, section_divs=FALSE, template=NULL,theme=NULL),
#     output_file="output.html",
#     output_dir=dir,
#     intermediates_dir=dir,
#     knit_root_dir=dir,
#     clean=FALSE,
#     envir=envir,
#     quiet=TRUE,
#   )
#   result <- paste0(readLines(paste0(dir,"/output.html")), collapse="\n")
#   r <- sub(".*</title>", "", result)
#   r <- sub("<body>", "", r)
#   r <- sub("</body>", "", r)
#   r <- sub("</head>", "", r)
#   r <- sub("</html>", "", r)
#   r
# }

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

