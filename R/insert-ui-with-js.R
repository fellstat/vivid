insert_ui_with_js <- function (selector, where = c("beforeBegin", "afterBegin", "beforeEnd",
                                                "afterEnd"), ui, multiple = FALSE, immediate = TRUE, session = shiny::getDefaultReactiveDomain()){
  # traverse html tree looking for scripts
  extract_scripts <- function(ui, scripts){
    if(!is.list(ui))
      return(scripts)
    if(!is.null(ui$name)){
      if(ui$name == "script"){
        scripts <- c(scripts, ui$children[[1]])
      }
    }
    children <- if(is.null(ui$children)) ui else ui$children
    if(is.list(children)){
      for(child in children){
        scripts <- extract_scripts(child, scripts)
      }
    }
    scripts
  }
  scripts <- extract_scripts(ui, c())

  # insert elements
  insertUI(
    selector = selector,
    where = where,
    ui = ui,
    multiple = multiple,
    immediate = immediate,
    session = session)

  #run code
  for(script in scripts)
    shinyjs::runjs(script)
}
