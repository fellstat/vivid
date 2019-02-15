
numeric_comparisons <- c(">",
                         ">=",
                         "<",
                         "<=",
                         "==",
                         "!=")
numeric_comparison_desc <- c("Greater than",
                             "Greater Than or Equal",
                             "Less Than",
                             "Less Than or Equal",
                             "Equal",
                             "Not Equal")

filter_ui <- function(id=""){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        pickerInput(
          ns("num_var"),
          "",
          choices = c(),
          multi=FALSE,
          options = list(`actions-box` = TRUE,
                         `live-search`=TRUE,
                         `none-selected-text`="Variable"),
          width="100%"
        )
      ),
      column(3,
        pickerInput(
          ns("num_comp"),
          "",
          selected="Greater than (>)",
          choices = numeric_comparisons,
          multi=FALSE,
          choicesOpt = list(
            subtext=numeric_comparison_desc
          ),
          width="100%"
        )
      ),
      column(3,
        numericInput(ns("num_value"),"", value=NA)
      ),
      column(2,
             br(),
        actionButton(ns("num_add"), "Add")
      )

    ),
    fluidRow(
      column(4,
             pickerInput(
               ns("cont_var"),
               "",
               choices = c(),
               multi=FALSE,
               options = list(`actions-box` = TRUE,
                              `live-search`=TRUE,
                              `none-selected-text`="Variable"),
               width="100%"
             )
      ),
      column(3,
             pickerInput(
               ns("cont_comp"),
               "",
               choices = c("Includes","Excludes", "Missing","Not Missing"),
               multi=FALSE,
               selected="Includes",
               width="100%"
             )
      ),
      column(3,
             conditionalPanel(
               paste0("input['",ns("cont_comp"), "']!=\"Missing\" & ", "input['",ns("cont_comp"), "']!=\"Not Missing\""),
               pickerInput(
                 ns("cont_values"),
                 "",
                 choices = c(),
                 multi=TRUE,
                 options = list(`actions-box` = TRUE,
                                `live-search`=TRUE,
                                `none-selected-text`="Values"),
                 width="100%"
               )
             )
      ),
      column(2,
             br(),
             actionButton(ns("cont_add"), "Add")
      )

    ),
    hr(),
    fluidRow(
      column(12,
             shiny::tagAppendAttributes(
               textAreaInput(
                  ns("filter"),
                  label = "Filter Expression",
                  placeholder = "A logical expression like: variable > 2",
                  width="100%",
                  resize="none"
                ),
                style = 'width: 100%;'
              )
      )
    )
  )

}

filter_server <- function(input, ouput, session, data, variables, state=NULL){

  if(!is.null(state)){
    #input$filter <- state$filter
  }
  code <- reactive({
    vars <- input$dup_vars
    paste0("dplyr::filter(", input$filter, ")")
  })

  input_variables <- variables
  output_variables <- variables
  set_input_variables <- function(v){
    observe({
      updatePickerInput(session, "num_var", choices = v()$objects)
      updatePickerInput(session, "cont_var", choices = v()$objects)
    })
  }
  set_input_variables(variables)


  observeEvent(input$num_add,{
    if(!(input$num_var %in% input_variables()$objects))
      return()
    txt <- input$filter
    isnum <- input_variables()[input_variables()$objects == input$num_var, "classes", drop=TRUE][1] == "numeric"
    if(isnum && input$num_comp == "=="){
      ex <- paste0("dplyr::near(", input$num_var, ", ", input$num_value,")")
    }else if(isnum && input$num_comp == "!="){
      ex <- paste0("!dplyr::near(", input$num_var, ", ", input$num_value,")")
    }else{
      ex <- paste(input$num_var, input$num_comp, input$num_value)
    }
    if(nchar(txt) != 0)
      txt <- paste0(txt," & ")
    txt <- paste0(txt, ex)
    updateTextAreaInput(session, "filter", value = txt)
  })

  observeEvent(input$cont_var,{
    if(!(input$cont_var %in% input_variables()$objects))
      return()
    if(length(input_variables()[input_variables()$objects == input$cont_var, "classes", drop=TRUE][[1]]) == 0){
      updatePickerInput(session, "cont_values", choices = as.character(c()))
    }else{
      expr <- parse(text=paste0("unique(", data(),"[[\"", input$cont_var,"\"]])"))
      remote_eval(expr, callback = function(x){
        print(length(x))
        updatePickerInput(session, "cont_values", choices = as.character(x))
      }, substitute = FALSE)
    }
  })


  observeEvent(input$cont_add,{
    if(!(input$cont_var %in% input_variables()$objects))
      return()
    txt <- input$filter
    isnum <- input_variables()[input_variables()$objects == input$cont_var, "classes", drop=TRUE][1] %in% c("numeric", "integer")
    if(!isnum)
      vals <- unlist(lapply(input$cont_values, function(x) paste0("\"", x, "\"")))
    else
      vals <- input$cont_values
    exp <- paste0("(", input$cont_var, " %in% c(", paste0(vals, collapse = ", "), "))")
    if(input$cont_comp != "Includes")
      exp <- paste0("!", exp)
    if(input$cont_comp == "Missing")
      exp <- paste0("is.na(", input$cont_var,")")
    if(input$cont_comp == "Not Missing")
      exp <- paste0("!is.na(", input$cont_var,")")
    if(nchar(txt) != 0)
      txt <- paste0(txt," & ")
    txt <- paste0(txt, exp)
    updateTextAreaInput(session, "filter", value = txt)
  })

  get_state <- function(){
    list(filter=input$filter)
  }

  list(
    code = code,
    set_input_variables = set_input_variables,
    output_variables = output_variables,
    name="filter",
    get_state = get_state
  )
}
