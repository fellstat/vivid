

server_rstudio <- function(input, output, session){
  observeEvent(input$doc_to_markdown,{
    doc_id <- session$userData$active_doc
    doc <- session$userData$docs[[doc_id]]
    md <- list()
    for(i in seq_along(doc)){
      gizmo_md <- session$userData$r_markdown[[doc[[i]]$id]]()
      md[[i]] <- gizmo_md
    }
    md <- paste(unlist(md), collapse="\n")
    if(is.null(parent_queue())){
    rstudioapi::documentNew(md, "rmarkdown")
    }else{
      parent_queue()$producer$fireEval({
        rstudioapi::documentNew(md, "rmarkdown")
      }, env=list(md=md))
    }
  })

  observeEvent(input$doc_to_r_script,{
    doc_id <- session$userData$active_doc
    doc <- session$userData$docs[[doc_id]]
    md <- list()
    for(i in seq_along(doc)){
      gizmo_md <- session$userData$r_markdown[[doc[[i]]$id]]()
      md[[i]] <- parse_chunk_r_code(gizmo_md)
    }
    md <- paste(unlist(md), collapse="\n")
    if(is.null(parent_queue())){
      rstudioapi::documentNew(md, "r")
    }else{
      parent_queue()$producer$fireEval({
        rstudioapi::documentNew(md, "r")
      }, env=list(md=md))
    }
  })

}
