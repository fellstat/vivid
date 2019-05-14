
start_server_r <- function(millis=250){
  if(is.null(.globals$server_r)){
    .globals$server_r <- RemoteR$new()
    .globals$server_r$start_monitor(millis = millis)

    .globals$vivid_server$parent_queue <- ipc::queue()
    .globals$vivid_server$child_queue <- ipc::queue()
    .globals$remote_r <- QueueLinkedR$new(parent_queue(), child_queue())
    .globals$vivid_server$parent_queue$consumer$start(env = .GlobalEnv)
    .globals$server_r$eval(
      {
        unlink("server.log")
        con <- file("server.log")
        sink(con, append=TRUE)
        sink(con, append=TRUE, type="message")
        library(vivid)
        library(ipc)
        library(shiny)
        vivid_globals <- vivid:::.globals
        vivid_globals$is_execution_process <- FALSE
        #cq <- ipc::queue()
        #child_queue(cq)
        #child_queue()$start()
        parent_queue(pq)
        child_queue(cq)
        remote_r(QueueLinkedR$new(parent_queue(), child_queue()))
		TRUE
      },
      function(result){
        #print(result)
        message("Vivid Remote R Started")
      },
      envir=list(
        pq=.globals$vivid_server$parent_queue,
        cq=.globals$vivid_server$child_queue
      )
    )
  }
}


start_standalone_server_r <- function(millis=250){
  if(is.null(.globals$standalone_server_r)){
    .globals$standalone_server_r <- RemoteR$new()
    .globals$standalone_server_r$start_monitor(millis = millis)

    .globals$vivid_standalone_server$parent_queue <- ipc::queue()
    .globals$vivid_standalone_server$child_queue <- ipc::queue()
    .globals$standalone_remote_r <- QueueLinkedR$new(
      .globals$vivid_standalone_server$parent_queue,
      .globals$vivid_standalone_server$child_queue
    )
    .globals$vivid_standalone_server$parent_queue$consumer$start(env = .GlobalEnv)
    .globals$standalone_server_r$eval(
      {
        unlink("standalone.log")
        con <- file("standalone.log")
        sink(con, append=TRUE)
        sink(con, append=TRUE, type="message")
        library(vivid)
        library(ipc)
        library(shiny)
        vivid_globals <- vivid:::.globals
        vivid_globals$is_execution_process <- FALSE
        parent_queue(pq)
        child_queue(cq)
        remote_r(QueueLinkedR$new(parent_queue(), child_queue()))
        child_queue()$consumer$start(env=.GlobalEnv)
      },
      function(result){
        #print(result)
        message("Vivid Remote R Started")
      },
      envir=list(
        pq=.globals$vivid_standalone_server$parent_queue,
        cq=.globals$vivid_standalone_server$child_queue
      )
    )
  }
}


remote_eval <- function(expr, callback=NULL, envir=NULL, substitute=TRUE){
  if(.globals$is_execution_process){
    if(substitute)
      expr <- substitute(expr)
    res <- eval(expr, envir=envir, enclos=.GlobalEnv)
    if(!is.null(callback))
      callback(res)
    return(invisible())
  }
  if(substitute)
    expr <- substitute(expr)
  .globals$remote_r$eval(expr, callback, envir, FALSE)
}

interrupt_r <- function(){
  .globals$remote_r$interrupt()
}


RemoteR <- R6::R6Class(
  "RemoteR",
  private = list(
    is_running = FALSE,

    monitor_running = FALSE,

    stop_monitor_flag = FALSE,

    callbacks = list(),

    pid = NULL,

    cluster = NULL,

    monitor_later_handle = NULL,

    n_active_jobs = 0

  ),
  public = list(

    initialize = function(source){
      print("creating")
      self$start()
    },

    start = function(){
      if(!private$is_running){
        private$n_active_jobs <- 0
        private$cluster <- parallel::makePSOCKcluster(1)
        private$is_running <- TRUE
        private$pid <- unlist(parallel::clusterCall(private$cluster,function(...) Sys.getpid()))
      }
    },

    stop = function(){
      if(private$is_running){
        self$interrupt()
        parallel::stopCluster(private$cluster)
        private$is_running <- FALSE
        private$n_active_jobs <- 0
        private$callbacks <- list()
        private$stop_monitor_flag <- TRUE
      }
    },

    results_ready = function(timeout=0){
      base::socketSelect(list(private$cluster[[1]]$con), write=FALSE, timeout=timeout)
    },

    eval = function(expr, callback=NULL, envir=NULL, substitute=TRUE){
      if(substitute)
        expr <- substitute(expr)
      if(!is.null(envir))
        expr <- do.call('substitute', list(as.call(expr), env=envir))
      uuid <- gen_uuid()
      private$callbacks[[uuid]] <- callback
      parallel:::sendCall(private$cluster[[1]], fun = geval, args = list(expr, uuid))
      private$n_active_jobs <- private$n_active_jobs + 1
      invisible()
    },

    start_monitor = function(millis=250){
      if(private$monitor_running)
        return(FALSE)
      private$monitor_running <- TRUE
      private$stop_monitor_flag <- FALSE
      callback <- function(){
        if (private$stop_monitor_flag){
          private$monitor_running <- FALSE
          return()
        }
        tryCatch({
          while(self$results_ready()){
            ret <- parallel:::recvResult(private$cluster[[1]])
            private$n_active_jobs <- private$n_active_jobs - 1
            fun <- private$callbacks[[ret[[2]]]]
            value <- ret[[1]]
            private$callbacks[[ret[[2]]]] <- NULL
            if(!is.null(fun))
              (function(x){
                force(x)
                fun(x)
              })(value)
          }
        })
        private$monitor_later_handle <- later::later(callback, millis / 1000)
      }
      callback()
      TRUE
    },

    stop_monitor = function(){
      private$stop_monitor_flag <- TRUE
      if(!is.null(private$monitor_later_handle))
        private$monitor_later_handle()
    },

    interrupt = function(){
      for(i in seq_len(private$n_active_jobs)){
        system(paste("kill -INT", private$pid,"&"))
        if(i < private$n_active_jobs)
          Sys.sleep(.01)
      }
      private$n_active_jobs <- 0
      private$callbacks <- list()
    },

    finalize = function() {
      self$stop_monitor()
      tryCatch(
        self$stop(),
        error=function(err){
          if(err$message != "invalid connection")
            stop(err)
        }
      )
    }

  )
)


vivid_queue <- function(...){
  q <- ipc::queue(...)

}


QueueLinkedR <- R6::R6Class(
  "QueueLinkedR",
  private = list(

    is_running = FALSE,

    callbacks = list(),

    pid = NULL,

    n_active_jobs = 0,

    exec_queue = NULL,

    server_queue = NULL,

    session = NULL

  ),
  public = list(

    initialize = function(exec_queue, server_queue){
      private$exec_queue <- exec_queue
      private$server_queue <- server_queue
      ind <- server_queue$consumer$addHandler(function(signal, obj, env){

        # work around for session getting erased during callback execution
        if(is.null(shiny::getDefaultReactiveDomain()))
          assign("domain", private$session, envir=shiny:::.globals)

        fun <- private$callbacks[[obj[[2]]]]
        value <- obj[[1]]
        private$callbacks[[obj[[2]]]] <- NULL
        if(!is.null(fun))
          (function(x){
            force(x)
            fun(x)
          })(value)
      }, "callback_exec")
      for(i in seq_len(ind-1))
        private$server_queue$consumer$removeHandler("callback_exec", 1)
      self$eval(Sys.getpid(), function(pid) {
        print(pid)
        private$pid <- pid
      })
    },

    start = function(){
      if(!private$is_running){
        private$is_running <- TRUE
      }
    },

    stop = function(){
      if(private$is_running){
        private$is_running <- FALSE
        private$n_active_jobs <- 0
        private$callbacks <- list()
      }
    },

    eval = function(expr, callback=NULL, envir=NULL, substitute=TRUE){
      library(vivid)
      if(substitute)
        expr <- substitute(expr)
      if(!is.null(envir))
        expr <- do.call('substitute', list(as.call(expr), env=envir))
      uuid <- gen_uuid()
      private$callbacks[[uuid]] <- callback
      private$exec_queue$producer$fireCall("gevalQ", uuid=uuid, expr=expr, queue=private$server_queue)
      private$n_active_jobs <- private$n_active_jobs + 1
      invisible()
    },

    interrupt = function(){
      try({
        system(paste("kill -INT", private$pid,"&"))
      })
      #private$n_active_jobs <- max(private$n_active_jobs - 1, 0)
    },

    set_session = function(session){
      private$session <- session
    },

    finalize = function() {
      self$stop()
    }

  )
)

