

vivid_panel <- function(ns, gizmo){
  tags$div(
    id=ns("vivid-panel"),
    class="panel panel-default",
    actionLink(
        ns("__ctrl_up"),
        icon("arrow-circle-up")
    ),
    actionLink(
        ns("__ctrl_down"),
        icon("arrow-circle-down")
    ),
    actionLink(
        ns("__ctrl_close"),
        icon("times-circle")
    ),
    tags$div(
      id=ns("::panel_accordion"),
      #role="tablist",
      #`aria-multiselectable`="true",
      panel_card(
        ns("gizmo_card"),
        "Controls",
        gizmo(ns),
        actionButton(
          ns("__run_r_markdown"),
          "Run"
        )
      ),
      panel_card(
        ns("markdown_card"),
        "R Markdown",
        verbatimTextOutput(
          ns("rmarkdown")
        ),
        actionButton(
          ns("__run_r_markdown"),
          "Run"
        ),
        show=FALSE
      ),
      panel_card(
        ns("output_card"),
        "Results",
        htmlOutput(
          ns("__r_output")
        )
      )
    )
  )
}
