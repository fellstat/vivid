
test_gizmo_ui <- function(ns){
  textInput(ns("helloworld"), "input")
}

test_gizmo_server <- function(input, output, session, set_rmarkdown_reactive){
  txt_react <- reactive({
    txt <- paste0("## ", input[["helloworld"]],"\n```{r}\ntest_var <- if(!exists('test_var')) 1 else test_var+1\nprint(test_var)\ndata(mtcars)\nmtcars\n```\nThe above is a test")
    txt
  })
  set_rmarkdown_reactive(txt_react)
}

test_gizmo_get_state <- function(input, output, session){
  list(helloworld=input[["helloworld"]])
}

test_gizmo_restore_state <- function(input, output, session, state){
  updateTextInput(session, "helloworld", value=state$helloworld)
}


.globals$gizmos$helloworld <- list(
  ui=test_gizmo_ui,
  server=test_gizmo_server,
  library="vivid",
  get_state=test_gizmo_get_state,
  restore_state=test_gizmo_restore_state,
  opts=list()
)


runHelloWorld <- function() runStandalone("helloworld")
