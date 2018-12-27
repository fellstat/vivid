context("queue-linked-r")

test_that("QueueLinkedR", {
  #ppq <- parent_queue()
  #pcq <- child_queue()
  pq <- ipc::queue()
  cq <- ipc::queue()
  #parent_queue(pq)
  #child_queue(cq)

  #pq$consumer$start()

  qlr <- QueueLinkedR$new(pq, cq)

  qlr$eval(paste("a","b"), print)

  po <- pq$consumer$consume()
  co <- cq$consumer$consume()

  parent_queue(ppq)
  child_queue(pcq)
})
