

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
        ),
		prettyToggle(
            inputId = ns("__auto_pause"), 
            value = TRUE,
            label_on = NULL, #label_on = "Auto",
            label_off = NULL, #label_off = "Pause",
            outline = TRUE,
            plain = TRUE,
            bigger = TRUE, 
            inline = TRUE,
            icon_on = icon("play-circle-o", class = "fa-2x"),
            icon_off = icon("pause-circle-o", class = "fa-2x")
          )
      ),
      panel_card(
        ns("markdown_card"),
        "R Markdown",
        verbatimTextOutput(
          ns("rmarkdown")
        ),
        #actionButton(
        #  ns("__run_r_markdown"),
        #  "Run"
        #),
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
