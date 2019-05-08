
gizmo_3d_ui <- function(ns) {
  fluidPage(
    data_input(ns("data")),
    fluidRow(
      column(4,
             variable_input(ns("x"),"X")
      ),
      column(4,
             variable_input(ns("y"),"Y")
      ),
      column(4,
             variable_input(ns("z"),"Z")
      )
    )
  )
}


gizmo_3d_server <- function(input, output, session, state = NULL) {
  init_data <- if(is.null(state)) NULL else state$data
  init_x <- if(is.null(state)) NULL else state$x
  init_y <- if(is.null(state)) NULL else state$y
  init_z <- if(is.null(state)) NULL else state$z
  init_data_input(session, "data", init_selected=init_data)
  init_variable_input(session, "x", "data", init_data_name = init_data, init_selected=init_x)
  init_variable_input(session, "y", "data", init_data_name = init_data, init_selected=init_y)
  init_variable_input(session, "z", "data", init_data_name = init_data, init_selected=init_z)

  txt_react <- reactive({
    txt <- paste0(
"
```{r}
plotly::plot_ly(", input$data, ", x = ~", input$x, ", y = ~", input$y, ", z = ~", input$z, ") %>%
  plotly::add_markers()
```
")
    txt
  })

  get_state <- function(){
    list(data = input[["data"]], x = input[["x"]], y = input[["y"]], z = input[["z"]], `__version__` = "0.1")
  }

  list(
    code = txt_react,
    get_state = get_state
  )
}

.globals$gizmos$scatter_3d <- list(
  ui = gizmo_3d_ui,
  server = gizmo_3d_server,
  library = "vivid",
  opts = list()
)

run_package_data <- function()
  run_standalone("scatter_3d")
