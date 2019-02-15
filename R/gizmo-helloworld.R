
test_gizmo_ui <- function(ns){
  textInput(ns("helloworld"), "input")
}

test_gizmo_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "helloworld", value=state$helloworld)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("## ", input[["helloworld"]],"\n```{r}\ntest_var <- if(!exists('test_var')) 1 else test_var+1\nprint(test_var)\ndata(mtcars)\nmtcars\n```\nThe above is a test")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      helloworld=input[["helloworld"]],
      `__version__` = "1.0"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$helloworld <- list(
  ui=test_gizmo_ui,
  server=test_gizmo_server,
  library="vivid",
  opts=list()
)


run_hello_world <- function() run_standalone("helloworld")
