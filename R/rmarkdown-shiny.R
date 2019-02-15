markdown_gizmo_ui <- function(ns){
          tagAppendAttributes(
            textAreaInput(
              ns("markdown"),
              NULL,
              width="100%",
              height="400px",
              resize="vertical",
            ),
            style = 'width: 100%;'
          )
}

markdown_gizmo_server <- function(input, output, session, state=NULL){
  txt_react <- reactive({
    txt <- input[["markdown"]]
    txt
  })

  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextAreaInput(session, "markdown", value=state$markdown)
    })
  }

  get_state <- function(){
    list(
      markdown=input[["markdown"]],
      `__version__`="0.1"
      )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$markdown <- list(
  ui=markdown_gizmo_ui,
  server=markdown_gizmo_server,
  library="vivid",
  opts=list()
)
