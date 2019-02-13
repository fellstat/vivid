

launch_vivid_child_server <- function(browser = getOption("shiny.launch.browser", interactive()), millis=205){

  start_server_r()
  .globals$server_r$eval(
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
      launch_vivid(launch.browser = parent_browser)
    },
    function(result){
      print(result)
      message("Vivid Shiny Application Stopped")
    },
    envir=list(launch.browser=browser)
  )

}

stop_vivid_child_server <- function(){
  child_queue()$producer$fireEval(shiny::stopApp())
}

child_queue <- function(queue){
  if(!missing(queue))
    .globals$vivid_server$child_queue <- queue
  .globals$vivid_server$child_queue
}

parent_queue <- function(queue){
  if(!missing(queue))
    .globals$vivid_server$parent_queue <- queue
  .globals$vivid_server$parent_queue
}

remote_r <- function(rr){
  if(!missing(rr))
    .globals$remote_r <- rr
  .globals$remote_r
}
