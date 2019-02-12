create_gizmo <- function(input, output, session, gizmo_name, doc_id=session$userData$active_doc, id=gen_uuid(), state=NULL){
  ui <- .globals$gizmos[[gizmo_name]]$ui
  server <- .globals$gizmos[[gizmo_name]]$server
  restore_state <- .globals$gizmos[[gizmo_name]]$restore_state

  ns <- NS(id)
  insertUI(
    if (!is.null(state) && exists('selector', where=state)) {state$selector} else {paste0("#", doc_id)}, # paste0("#", doc_id),
    if (!is.null(state) && exists('where', where=state)) {state$where} else {"beforeEnd"}, # "beforeEnd",
    vivid_panel(ns, ui),
    session=session,
    immediate = TRUE
  )
  output[[ns("__ctrl_number")]] <- renderText(paste0('ID: #',id,'    '))


  set_rmarkdown_reactive <- function(txt){
    output[[ns("rmarkdown")]] <- renderText({
      txt()
    })
    session$userData$r_markdown[[id]] <- txt
    observeEvent({
      input[[ns("__run_r_markdown")]]
    },
    {
      output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'working'))
      rmd <- txt()
      if(!is.null(rmd) && rmd != ""){
        remote_eval(
          {
            vivid::run_chunk(chunk, envir=.GlobalEnv)
          },
          function(result){
            output[[ns("__r_output")]] <- renderText(result)
            output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'done'))
            session$userData$r_output[[id]] <- result
          },
          envir = list(chunk=rmd)
        )
      }
    })
  }

  observeEvent({
    input[[ns("__ctrl_up")]]
  },
  {
    loc=create_gizmo_get_loc(session$userData$docs[[doc_id]], id)
    if (loc!=1){
      removeUI(
        paste0("#", ns("vivid-panel"))
      )
      gizmo <- session$userData$docs[[doc_id]][[loc]]
      childScope <- session$makeScope(gizmo$id)
      gizmo_state <- withReactiveDomain(childScope, {
        gizmo$gizmo$get_state(childScope$input, childScope$output, childScope)
      })
      id2=gen_uuid()
      nsm1 <- NS(session$userData$docs[[doc_id]][[loc-1]]$id)
      state <- list()
      state$gizmo <- gizmo_state
      state$rmarkdown <- input[[ns("rmarkdown")]]
      state[["r_output"]] <- session$userData$r_output[[id]]
      state$selector <- paste0("#", nsm1("vivid-panel"))
      state$where <- "beforeBegin"
      output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'MOVING UP'))
      state$state=state
      create_gizmo(session$input, session$output, session, gizmo_name=gizmo_name, id=id2, state=state)
      ns2 <- NS(id2)
      output[[ns2("__ctrl_status")]] <- renderText(paste0('  Status:', 'MOVED UP ', 'FROM: ', 'ID: #',id,'    ' ))
      session$userData$docs[[doc_id]][[loc]]=session$userData$docs[[doc_id]][[loc-1]]
      session$userData$docs[[doc_id]][[loc-1]]=session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]
      session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]=NULL
      session$userData$r_output[[id2]]=session$userData$r_output[[id]]
    }else{
      output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'already first one'))
    }
  })

  observeEvent({
    input[[ns("__ctrl_down")]]
  },
  {
    loc=create_gizmo_get_loc(session$userData$docs[[doc_id]], id)
    if (loc!=length(session$userData$docs[[doc_id]])){
      removeUI(
        paste0("#", ns("vivid-panel"))
      )
      gizmo <- session$userData$docs[[doc_id]][[loc]]
      childScope <- session$makeScope(gizmo$id)
      gizmo_state <- withReactiveDomain(childScope, {
        gizmo$gizmo$get_state(childScope$input, childScope$output, childScope)
      })
      id2=gen_uuid()
      nsp1 <- NS(session$userData$docs[[doc_id]][[loc+1]]$id)
      state <- list()
      state$gizmo <- gizmo_state
      state$rmarkdown <- input[[ns("rmarkdown")]]
      state[["r_output"]] <- session$userData$r_output[[id]]
      state$selector <- paste0("#", nsp1("vivid-panel"))
      state$where <- "afterEnd"
      output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'MOVING DOWN'))
      state$state=state
      create_gizmo(session$input, session$output, session, gizmo_name=gizmo_name, id=id2, state=state)
      ns2 <- NS(id2)
      output[[ns2("__ctrl_status")]] <- renderText(paste0('  Status:', 'MOVED DOWN ', 'FROM: ', 'ID: #',id,'    ' ))
      session$userData$docs[[doc_id]][[loc]]=session$userData$docs[[doc_id]][[loc+1]]
      session$userData$docs[[doc_id]][[loc+1]]=session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]
      session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]=NULL
      session$userData$r_output[[id2]]=session$userData$r_output[[id]]
    }else{
      output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'already last one'))
    }
  })

  observeEvent({
    input[[ns("__ctrl_close")]]
  },
  {
    loc=create_gizmo_get_loc(session$userData$docs[[doc_id]], id)
    output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'removing'))
    removeUI(
      paste0("#", ns("vivid-panel"))
    )
    session$userData$docs[[doc_id]]=session$userData$docs[[doc_id]][-loc];
  })

  observeEvent({
    input[[ns("__ctrl_clone")]]
  },
  {
    loc=create_gizmo_get_loc(session$userData$docs[[doc_id]], id)
    gizmo <- session$userData$docs[[doc_id]][[loc]]
    childScope <- session$makeScope(gizmo$id)
    gizmo_state <- withReactiveDomain(childScope, {
      gizmo$gizmo$get_state(childScope$input, childScope$output, childScope)
    })
    id2=gen_uuid()
    state <- list()
    state$gizmo <- gizmo_state
    state$rmarkdown <- input[[ns("rmarkdown")]]
    state[["r_output"]] <- session$userData$r_output[[id]]
    state$selector <- paste0("#", ns("vivid-panel"))
    state$where <- "afterEnd"
    output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'clone source'))
    state$state=state
    create_gizmo(session$input, session$output, session, gizmo_name=gizmo_name, id=id2, state=state)
    ns2 <- NS(id2)
    output[[ns2("__ctrl_status")]] <- renderText(paste0('  Status:', 'cloned ', 'FROM: ', 'ID: #',id,'    ' ))
    session$userData$r_output[[id2]]=session$userData$r_output[[id]]
    session$userData$docs[[doc_id]]=append(session$userData$docs[[doc_id]], list(session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]), loc)
    session$userData$docs[[doc_id]][[length(session$userData$docs[[doc_id]])]]=NULL
  })
  if(is.null(session$userData$docs[[doc_id]])){
    stop("Invalid Document ID")#session$userData$docs[[doc_id]] <- list()
  }
  
  n <- length(session$userData$docs[[doc_id]])
  loc <- n+1
  session$userData$docs[[doc_id]][[loc]] <- list(
    id=id,
    gizmo_name=gizmo_name,
    gizmo=.globals$gizmos[[gizmo_name]]
  )
  if(!is.null(state)){
    callModule(restore_state, id, state=state$state$gizmo, session=session)
    output[[ns("__r_output")]] <- renderText(state$state$r_output)
    output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'restored'))
  }
  callModule(server, id, set_rmarkdown_reactive=set_rmarkdown_reactive, session=session)




  output[[ns("__ctrl_status")]] <- renderText(paste0('  Status:', 'ready'))
}

create_gizmo_get_loc <- function(gizmo, id){
  result = NULL
  for(i in seq_along(gizmo)){
    if (gizmo[[i]]$id == id){
      result = i
      break
    }
  }
  result
}


register_gizmo <- function(input, output, session, menu_id, gizmo_name=menu_id){
  observeEvent(input[[menu_id]], {
    create_gizmo(input, output, session, gizmo_name)
  })
}
