drop_dups_ui <- function(id="", variables=c()){
  ns <- NS(id)
  pickerInput(
    ns("dup_vars"),
    "Check For Duplicates On:",
    choices = variables,
    multi=TRUE,
    options = list(`actions-box` = TRUE,
                   `live-search`=TRUE,
                   `none-selected-text`="Use All Variables")
  )
}

drop_dups_server <- function(input, ouput, session, data, variables, state=NULL){
  if(!is.null(state)){

  }
  code <- reactive({
    vars <- input$dup_vars
    paste0("distinct(", format_vars(vars), ")")
  })

  output_variables <- reactiveVal()
  set_input_variables <- function(v){
    observe({
      updatePickerInput(session, "dup_vars", choices = v()$objects)
    })
    output_variables(v())

  }
  set_input_variables(variables)

  get_state <- function(){
    list(dup_vars = input$dup_vars)
  }
  list(
    code = code,
    set_input_variables = set_input_variables,
    output_variables = output_variables,
    name="drop_dups",
    get_state=get_state
  )
}
