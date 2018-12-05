
doc_data <- function(doc_id, value, session=getDefaultReactiveDomain()){
  if(missing(value)){
  return(session$userData$docs[[doc_id]])
  }else
    session$userData$docs[[doc_id]] <- value
}

doc_attr <- function(doc_id, attribute, value, session=getDefaultReactiveDomain()){
  if(is.null(session$userData$docs[[doc_id]])){
    stop("Unknown document")
  }
  if(missing(value))
    return(attr(session$userData$docs[[doc_id]], attribute))
  else
    attr(session$userData$docs[[doc_id]], attribute) <- value
}



save_document <- function(document_id, input, session, file, name=NULL, with_workspace=TRUE){
  doc <- session$userData$docs[[document_id]]
  for(i in seq_along(doc)){
    gizmo <- doc[[i]]
    ns <- NS(gizmo$id)
    childScope <- session$makeScope(gizmo$id)
    gizmo_state <- withReactiveDomain(childScope, {
      gizmo$gizmo$get_state(childScope$input, childScope$output, childScope)
    })
    doc[[i]]$state <- list()
    doc[[i]]$state$gizmo <- gizmo_state
    doc[[i]]$state$rmarkdown <- input[[ns("rmarkdown")]]
    doc[[i]]$state[["r_output"]] <- session$userData$r_output[[gizmo$id]]
  }
  if(with_workspace){
    remote_eval(
      {
        ..vivid_document.. <- doc
        save(list=ls(all.names = TRUE), file=file, envir=parent.frame())
      },
      function(result){
        doc_attr(document_id, "path", file)
        doc_attr(document_id, "name", name)
        if(!is.null(name))
          shinyjs::runjs(paste0("$('#", document_id, "-a').replaceWith('", name, "')"))
      },
      envir = list(doc = doc, file=file)
    )
  }else{
    ..vivid_document.. <- doc
    save(..vivid_document.., file=file)
    doc_attr(document_id, "path", file)
    doc_attr(document_id, "name", name)
    if(!is.null(name))
      shinyjs::runjs(
        paste0(
          "$('#",
          document_id,
          "-a').html(\"<button class='close closeTab' type='button'>x</button>",
          name,
          "\")"
        )
      )
  }
}

load_document <- function(doc, input, output, session, file){
  remote_eval(
    {

      load(file, envir=.GlobalEnv)
      local({
        doc <- ..vivid_document..
        rm("..vivid_document..", envir=.GlobalEnv)
        doc
      })
    },
    function(result){
      for(i in seq_along(result)){
        id <- gen_uuid()
        create_gizmo(session$input, session$output, session, gizmo_name=result[[i]]$gizmo_name, id=id, state=result[[i]])
      }
      session$userData$docs[[doc]]
    },
    envir = list(file=file)
  )
}

add_new_document <- function(title, contents=NULL, session = getDefaultReactiveDomain()){
  doc_id <- gen_uuid()
  session$userData$docs[[doc_id]] <- list()
  insertUI(
    "#doc-tabs",
    "beforeEnd",
    tags$li(
      id=paste0(doc_id,"-li"),
      tags$a(
        id=paste0(doc_id,"-a"),
        href=paste0("#",doc_id),
        tags$button(
          class="close closeTab",
          type="button",
          "x"
        ),
        title
      )
    ),
    session=session,
    immediate = TRUE
  )
  insertUI(
    "#doc-tabs-panes",
    "beforeEnd",
    tags$div(
      class="tab-pane",
      id=doc_id,
      contents
    ),
    session=session,
    immediate = TRUE
  )
  doc_id
}

set_active_document <- function(doc_id,  session = getDefaultReactiveDomain()){
  active_doc <- session$userData$active_doc
  shinyjs::runjs(paste0("$('#",doc_id,"-a').tab('show')"))
  session$userData$active_doc <- doc_id
}

close_document <- function(doc_id,  session = getDefaultReactiveDomain()){
  active_doc <- session$userData$active_doc
  docs <- session$userData$docs[[doc_id]]
  if(length(docs[[doc_id]]) > 0){
  }
  shinyjs::runjs(paste0("$('#",doc_id,"-li').remove()"))
  shinyjs::runjs(paste0("$('#",doc_id,"').remove()"))
  session$userData$docs[[doc_id]] <- NULL
  if(active_doc == doc_id && length(session$userData$docs[[doc_id]]) > 0){
    dids <- session$userData$docs
    set_active_document(names(dids)[length(dids)], session)
  }else if(length(session$userData$docs) == 0){
    did <- add_new_document("Untitled", session=session)
    set_active_document(did, session=session)
  }
}



server_documents <- function(input, output, session = getDefaultReactiveDomain()){
  observeEvent(input$active_doc,{
    did <- substring(input$active_doc,2)
    set_active_document(did, session)
  })

  observeEvent(input$close_doc,{
    did <- substring(input$close_doc,2)
    close_document(did, session)
  })

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

  shinyFiles::shinyFileSave(input, "save_file", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFiles::shinyFileChoose(input, "load_vvd", roots = volumes, session = session, filetypes=c('vvd'))

  observeEvent(input$save_doc, {
    d <- modalDialog(
      title="Save Document",
      size="l",
      verbatimTextOutput("save_doc_path"),
      shinyFiles::shinySaveButton("save_file",
                                  "Save file",
                                  "Save file as...",
                                  filetype = list(vivid = "vvd")),
      checkboxInput("save_doc_wwork", "Save With Workspace"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_doc_ok", "OK")
      )
    )
    showModal(d)
    #active_doc <- session$userData$active_doc
    #save_document(active_doc, input, session, file="~/tmp/document.vvd", with_workspace=FALSE)
  })

  output$save_doc_path <- renderText({
    dir <- shinyFiles::parseSavePath(volumes, input$save_file)
    if(nrow(dir) > 0){
      file <- dir$datapath
      return(file)
    }
    NULL
  })

  observeEvent(input$save_doc_ok,{
    active_doc <- session$userData$active_doc
    dir <- shinyFiles::parseSavePath(volumes, input$save_file)
    if(nrow(dir) > 0){
      file <- dir$datapath
      save_document(active_doc, input, session, file=file, name=dir$name, with_workspace=input$save_doc_wwork)
    }else{
      showError("No File Path Selected")
    }
    removeModal()
  })

  observeEvent(input$new_doc, {
    did <- add_new_document("Untitled")
    set_active_document(did)
  })

  observeEvent(input$load_doc, {
    d <- modalDialog(
      title="Load Document",
      size="l",
      verbatimTextOutput("load_doc_path"),
      shinyFiles::shinyFilesButton("load_vvd", "Load Document", "Please select a file", FALSE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_doc_ok", "OK")
      )
    )
    showModal(d)
    #active_doc <- session$userData$active_doc
    #save_document(active_doc, input, session, file="~/tmp/document.vvd", with_workspace=FALSE)
  })

  output$load_doc_path <- renderText({
    dir <- shinyFiles::parseFilePaths(volumes, input$load_vvd)
    if(nrow(dir) > 0){
      file <- dir$datapath
      return(file)
    }
    NULL
  })

  observeEvent(input$load_doc_ok,{
    dir <- shinyFiles::parseFilePaths(volumes, input$load_vvd)
    if(nrow(dir) > 0){
      file <- dir$datapath
      did <- add_new_document(dir$name)
      set_active_document(did)
      load_document(did, input, output, session, file=file)
    }else{
      showError("No File Selected")
    }
    removeModal()
  })
}
