create_gizmo <- function(input, output, session, gizmo_name, doc_id=session$userData$active_doc, id=gen_uuid(), state=NULL){
  gizmo <- .globals$gizmos[[gizmo_name]]
  ui <- gizmo$ui
  server <- gizmo$server

  ns <- NS(id)
  insertUI(
    paste0("#", doc_id),
    "beforeEnd",
    vivid_panel(ns, ui),
    session=session,
    immediate = TRUE
  )

  outs <- callModule(server, id, state=state$gizmo)
  txt <- outs$code
  gizmo$get_state <- outs$get_state

  output[[ns("rmarkdown")]] <- renderText({
    txt()
  })

  session$userData$r_markdown[[id]] <- txt
  output[[ns("__r_output")]] <- renderText(state$r_output)

  observeEvent(input[[ns("__run_r_markdown")]],{
    rmd <- txt()
    if(!is.null(rmd) && rmd != ""){
      remote_eval(
        {
          vivid::run_chunk(chunk, envir=.GlobalEnv)
        },
        function(result){
          output[[ns("__r_output")]] <- renderText(result)
          session$userData$r_output[[id]] <- result
        },
        envir = list(chunk=rmd)
      )
    }
  })

  #if(!is.null(state)){
  #  later::later(function(){
  #    callModule(restore_state, id, state=state$state$gizmo, session=session)
  #    output[[ns("__r_output")]] <- renderText(state$state$r_output)
  #  }, 1)
  #}
  n <- length(session$userData$docs[[doc_id]])
  if(is.null(session$userData$docs[[doc_id]])){
    stop("Invalid Document ID")#session$userData$docs[[doc_id]] <- list()
  }
  session$userData$docs[[doc_id]][[n + 1]] <- list(
    id=id,
    gizmo_name=gizmo_name,
    gizmo=gizmo
  )
}

register_gizmo <- function(gizmo_name, ui, server, lib,  opts){
  .globals$gizmos[[gizmo_name]] <- list(
    ui=ui,
    server=server,
    library=lib,
    opts=opts
  )
}

add_gizmo_server_hook <- function(input, output, session, menu_id, gizmo_name=menu_id){
  observeEvent(input[[menu_id]], {
    create_gizmo(input, output, session, gizmo_name)
  })
}
