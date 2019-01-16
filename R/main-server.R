


vivid_server <- function(){

  server <- function(input, output, session) {

    .globals$vivid_server$child_queue$consumer$start()

    session$userData$docs <- list()

    session$userData$r_markdown <- list()

    session$userData$r_output <- list()

    server_documents(input, output, session)

    server_rstudio(input, output, session)


    observeEvent(input$interrupt_r, {
      interrupt_r()
    })

    register_gizmo(input, output, session, "gizmo_test","helloworld")

    register_gizmo(input, output, session, "menu_insert_markdown_block","markdown")

    make_menu()
    did <- add_new_document("Untitled")
    set_active_document(did)
  }
  server
}



vivid <- function(child_process=TRUE, ...){
  if(child_process)
    return(launch_vivid_child_server(...))

  .globals$vivid_server$parent_queue <- ipc::queue()
  .globals$vivid_server$child_queue <- ipc::queue()
  .globals$remote_r <- QueueLinkedR$new(parent_queue(), child_queue())
  .globals$vivid_server$parent_queue$consumer$start(env=.GlobalEnv)
  launch_vivid(...)
}


launch_vivid <- function(...){
  ui <- vivid_ui()
  server <- vivid_server()
  runApp(shinyApp(ui, server), ...)
}


confirmDialog <- function(..., title="Message", callback=NULL, button_labels=c("Cancel","OK"), session = getDefaultReactiveDomain()){
  uuid <- gen_uuid()
  ns <- NS(uuid)
  modal <- modalDialog(..., title=title, easyClose=FALSE, footer=tagList(
    actionButton(ns("cancel"), button_labels[1]),
    actionButton(ns("ok"), button_labels[2])
  ))
  # if(!is.null(callback)){
  #   observeEvent(session$input[[ns("cancel")]], {
  #     callback(button_labels[1])
  #     #removeModal(session=session)
  #   },
  #   domain=session)
  #   observeEvent(session$input[[ns("cancel")]], {
  #     callback(button_labels[2])
  #     #removeModal(session=session)
  #   },
  #   domain=session)
  # }
  showModal(modal, session=session)
}
