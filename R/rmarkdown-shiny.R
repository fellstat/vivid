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

markdown_gizmo_server <- function(input, output, session, set_rmarkdown_reactive){
  txt_react <- reactive({
    txt <- input[["markdown"]]
    txt
  })
  set_rmarkdown_reactive(txt_react)
}

markdown_gizmo_get_state <- function(input, output, session){
  list(markdown=input[["markdown"]])
}

markdown_gizmo_restore_state <- function(input, output, session, state){
  updateTextAreaInput(session, "markdown", value=state$markdown)
}


.globals$gizmos$markdown <- list(
  ui=markdown_gizmo_ui,
  server=markdown_gizmo_server,
  library="vivid",
  get_state=markdown_gizmo_get_state,
  restore_state=markdown_gizmo_restore_state,
  opts=list()
)
