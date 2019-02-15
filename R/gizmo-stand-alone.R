

launch_gizmo_local <- function(gizmo,
                               gizmo_name,
  state=NULL,
  web_browser=paneViewer()){
  ns <- function(x) x
  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      gizmo$ui(ns)
    ),
    miniUI::miniButtonBlock(
      actionButton(ns("__gizmo__cancel"), "Close"),
      actionButton(ns("__gizmo__reset"), "Reset"),
      actionButton(ns("__gizmo__to_document"), "To Document"),
      actionButton(ns("__gizmo__execute"), "Run")
    )
  )

  server <- function(input, output, session){

    set_rmarkdown_reactive <- function(txt){

      observeEvent(input[[ns("__gizmo__execute")]],{
        rmd <- txt()
        if(!is.null(rmd) && rmd != ""){
          remote_eval({
              code <- parse_chunk_r_code(rmd)
              rstudioapi::sendToConsole(code)
            },
            envir = list(rmd=rmd)
          )
        }
      })

      observeEvent(input[[ns("__gizmo__to_document")]],{
        rmd <- txt()
        if(!is.null(rmd) && rmd != ""){
          remote_eval({
            context <- rstudioapi::getSourceEditorContext()
            if(endsWith(tolower(context$path),".r"))
              code <- parse_chunk_r_code(rmd)
            else
              code <- rmd
            rstudioapi::insertText(text=code, id=context$id)
          },
            envir = list(rmd=rmd)
          )
        }
      })

      observeEvent(input[[ns("__gizmo__cancel")]],{
        remote_eval({
          globals <- vivid:::.globals
          globals$standalone_states[[gizmo_name]] <- state
        }, envir=list(
          state=gizmo$get_state(input, ouput, session),
          gizmo_name=gizmo_name
        )
        )
        stopApp()
      })

      observeEvent(input[[ns("__gizmo__reset")]],{
        if(.globals$is_execution_process){
          remote_eval({
            vivid::launch_gizmo_local(gizmo, gizmo_name)
          }, envir=list(
            gizmo=gizmo,
            gizmo_name=gizmo_name
          )
          )
        }else{
          remote_eval({
            vivid::launch_gizmo_remote(gizmo, gizmo_name)
          }, envir=list(
            gizmo=gizmo,
            gizmo_name=gizmo_name
          )
          )
        }
        stopApp()
      })
    }
    gizmo$server(input, output, session, set_rmarkdown_reactive)
    if(!is.null(state)){
      later::later(function(){
        gizmo$restore_state(input, ouput, session, state)
      }, 1)
    }
  }

  runApp(shinyApp(ui, server), launch.browser = web_browser)
}

launch_gizmo_remote <- function(gizmo,
                                gizmo_name,
                               state=NULL,
                               web_browser=paneViewer()){
  start_standalone_server_r()
  #.globals$standalone_server_r$interrupt()
  .globals$standalone_server_r$eval(
    {
      parent_browser <- function(url){
        parent_queue()$producer$fireEval(
          {
            launch.browser(url)
          },
          env=list(
            url=url,
            launch.browser=launch.browser
          )
        )
      }
      launch_gizmo_local(gizmo = gizmo, gizmo_name = gizmo_name, state=state, web_browser=parent_browser)
    },
    function(result){
    },
    envir=list(launch.browser=web_browser,
               gizmo=gizmo,
               state=state,
               gizmo_name=gizmo_name)
  )
}

run_standalone <- function(gizmo_name, remote=TRUE){
  launch <- if(remote) launch_gizmo_remote else launch_gizmo_local
  launch(
    .globals$gizmos[[gizmo_name]],
    gizmo_name,
    .globals$standalone_states[[gizmo_name]])
}

# launch_gizmo_remote(vivid:::.globals$gizmos$helloworld,"helloworld", vivid:::.globals$standalone_states[["helloworld"]])
