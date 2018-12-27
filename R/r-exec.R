
.globals <- new.env(parent = emptyenv())

.globals$remote_r <- NULL

.globals$gizmos <- list()

start_r <- function(millis=250){
  .globals$remote_r <- RemoteR$new()
  .globals$remote_r$start_monitor(millis = millis)
}


remote_eval <- function(expr, callback=NULL, envir=NULL, substitute=TRUE){
  if(is.null(.globals$remote_r)){
    start_r()
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
      for(i in 1:private$n_active_jobs)
        system(paste("kill -INT", private$pid,"&"))
      private$n_active_jobs <- 0
      private$callbacks <- list()
    },

    finalize = function() {
      self$stop_monitor()
      self$stop()
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

    server_queue = NULL

  ),
  public = list(

    initialize = function(exec_queue, server_queue){
      private$exec_queue <- exec_queue
      private$server_queue <- server_queue
      ind <- server_queue$consumer$addHandler(function(signal, obj, env){
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
      for(i in 1:private$n_active_jobs)
        system(paste("kill -INT", private$pid,"&"))
      private$n_active_jobs <- 0
      private$callbacks <- list()
    },

    finalize = function() {
      self$stop()
    }

  )
)

