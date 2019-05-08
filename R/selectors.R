


data_input <- function(input_id, label="Input Dataset:", multiple=FALSE){
  shinyWidgets::pickerInput(inputId = input_id,
              label = label,
              choices = c(""),
              multiple=multiple,
              options = list(`actions-box` = TRUE,
                             `live-search`=TRUE,
                             `none-selected-text`="Choose Data")
  )
}


init_data_input <- function(session, input_id, init_selected){
  remote_eval(vivid:::.get_data()$objects, function(obj){
    names(obj) <- obj
    print(obj)
    shinyWidgets::updatePickerInput(session, inputId = input_id, choices = obj, selected=init_selected)
  })
}


variable_input <- function(input_id, label="", multiple=FALSE){
  shinyWidgets::pickerInput(
    inputId = input_id,
    label=label,
    selected = "",
    choices = c(),
    multiple = multiple,
    options = list(`actions-box` = TRUE,
                   `live-search`=TRUE,
                   `none-selected-text`="Variable"),
    width="100%"
  )
}


init_variable_input <- function(session, input_id, data_id, init_data_name=NULL, init_selected=NULL){
  if(is.null(init_data_name))
    init_data_name <- ""
  observeEvent(session$input[[data_id]],{
    dat <- session$input[[data_id]]
    if(!is.null(dat) && !is.na(dat) && dat != ""){
      expr <- parse(text=paste0("vivid:::.get_objects(envir=", dat, ")"))
      remote_eval(
        expr,
        callback = function(x){
          vars <- x$objects
          sel <- init_selected
          if(!is.null(sel) && !is.na(sel) && !(sel %in% vars)){
            vars <- list(variables=vars, unknown=sel)
          }
          updatePickerInput(session, input_id, choices = vars, selected=sel)

        },
        substitute = FALSE
      )
    }
  }, domain = session)
}


update_variable_input <- function(input_id, data_id, init_data_name="", session=shiny::getDefaultReactiveDomain()){
  observeEvent(session$input[[data_id]],{
    dat <- session$input[[data_id]]
    if(!is.null(dat) && !is.na(dat) && dat != ""){
      expr <- parse(text=paste0("vivid:::.get_objects(envir=", dat, ")"))
      remote_eval(
        expr,
        callback = function(x){
          vars <- x$objects
          sel <- isolate(session$input[[input_id]])
          if(!is.null(sel) && !is.na(sel) && (dat == "" || init_data_name == dat) && !(sel %in% vars)){
            vars <- list(variables=vars, unknown=sel)
          }
          updatePickerInput(session, input_id, choices = vars, selected=sel)
        },
        substitute = FALSE
      )
    }
  }, domain = session)
}
