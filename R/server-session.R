

launch_vivid_child_server <- function(launch.browser = getOption("shiny.launch.browser", interactive()), millis=205){
  if(!is.null(.globals$vivid_server$remote_r)){
    try(.globals$vivid_server$remote_r$stop())
  }
  #.globals$vivid_server$remote_r <-  RemoteR$new()

  .globals$vivid_server$parent_queue <- ipc::queue()
  .globals$vivid_server$child_queue <- ipc::queue()
  .globals$remote_r <- QueueLinkedR$new(parent_queue(), child_queue())
  .globals$server_r <- RemoteR$new()
  .globals$vivid_server$parent_queue$consumer$start(env = .GlobalEnv)

  start_server_r()

  .globals$server_r$eval(
    {
      unlink("server.log")
      con <- file("server.log")
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")

      library(vivid)
      library(ipc)
      library(shiny)
      #cq <- ipc::queue()
      #child_queue(cq)
      #child_queue()$start()
      parent_queue(pq)
      child_queue(cq)
      remote_r(QueueLinkedR$new(parent_queue(), child_queue()))

      #cq$consumer$start()

      parent_browser <- function(url){
        pq$producer$fireEval(
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
      #child_queue()
    },
    function(result){
      print(result)
      message("Vivid Shiny Application Stopped")
    },
    envir=list(
      pq=.globals$vivid_server$parent_queue,
      cq=.globals$vivid_server$child_queue,
      launch.browser=launch.browser)
  )

  .globals$vivid_server$parent_queue$consumer$start()
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
