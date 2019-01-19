

launch_gizmo <- function(gizmo,
  state=NULL,
  child_process=FALSE,
  browser=dialogViewer("")){
  ns <- function(x) x
  ui <- miniPage(
    miniContentPanel(
      gizmo$ui(ns)
    ),
    miniButtonBlock(
      actionButton(ns("__gizmo__cancel"), "Cancel"),
      actionButton(ns("__gizmo__to_document"), "To Document"),
      actionButton(ns("__gizmo__execute"), "Run")
    )
  )

  server <- function(input, output, session){

    set_rmarkdown_reactive <- function(txt){

      observeEvent(input[[ns("__gizmo__execute")]],{
        rmd <- txt()
        if(!is.null(rmd) && rmd != ""){
          rmd <- parse_chunk_r_code(rmd)
          rstudioapi::sendToConsole(rmd)
          stopApp()
        }
      })

      observeEvent(input[[ns("__gizmo__to_document")]],{
        rmd <- txt()
        if(!is.null(rmd) && rmd != ""){
          context <- rstudioapi::getSourceEditorContext()
          if(endsWith(tolower(context$path),".r"))
            rmd <- parse_chunk_r_code(rmd)
          rstudioapi::insertText(text=rmd, id=context$id)
          stopApp()
        }
      })
    }
    gizmo$server(input, output, session, set_rmarkdown_reactive)
  }

  runApp(shinyApp(ui, server), launch.browser = browser)
}

# launch_gizmo(.globals$gizmos$helloworld)
