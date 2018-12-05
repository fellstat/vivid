
action_link <- function(input_id, label, ...){
  value <- restoreInput(id = input_id, default = NULL)
  tags$a(id=input_id,label, class = "action-button", `data-val` = value, href="#", ...)
}

vivid_navbar <- function(...){
  tags$nav(
    class="navbar navbar-default navbar-fixed-top",
    tags$div(
      class="container",
      tags$a(class="navbar-brand", "VIVID"),
      tags$div(
        class="navbar-collapse collapse",
        tags$ul(
          id="vivid-navbar-ul",
          class="nav navbar-nav"
        ),
        tags$ul(
          id="vivid-navbar-ul-right",
          class="nav navbar-nav navbar-right",
          tags$li(
            class=NULL,
            action_link("interrupt_r", "Stop R")
          )
        )
      )
    ),
    ...
  )

}

vivid_nav_docs <- function(){
  tags$div(
    tags$ul(
      class="nav nav-tabs",
      id="doc-tabs"
    )
  )
}


vivid_ui <- function(){
  ui <- shinyUI(
    bootstrapPage(
      shinyjs::useShinyjs(),
      tags$script(src = "vivid/vivid.js"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "vivid/vivid.css"
      ),
      vivid_navbar(vivid_nav_docs()),
      br(),
      br(),
      br(),
      br(),
      br(),
      tags$div(
        id="vivid-main-document",
        class="container-fluid",
        tags$div(class="tab-content", id="doc-tabs-panes")
      )
    )
  )
  ui
}

# vivid_panel <- function(vid, ...){
#   tags$div(
#     id=vid("vivid-panel"),
#     class="panel panel-default",
#     ...
#   )
# }

panel_card <- function(card_id, heading, ..., show=TRUE){
  show <- if(show) "in" else ""
  tags$div(
    class="card",
    tags$div(
      id=paste0(card_id, "_heading"),
      class="card-header",
      tags$h5(
        class="mb-0",
        tags$a(
          `data-toggle`="collapse",
          #role="button",
          href=paste0("#", card_id, "collapse_one"),
          `data-target`=paste0("#", card_id, "_collapse_one"),
          `aria-expanded`="true",
          `aria-controls`=paste0("#", card_id, "_collapse_one"),
          heading
        )
      )
    ),
    tags$div(
      id=paste0(card_id, "_collapse_one"),
      class=paste0("collapse multi-collapse ", show),
      `aria-labelledby`=paste0(card_id, "_heading"),
      `aria-expanded`="true",
      tags$div(
        class="card card-body",
        ...
      )
    )
  )
}


showError <- function(...){
  showModal(modalDialog(..., title="Error", easyClose=TRUE))
}

showWarning <- function(...){
  showModal(modalDialog(..., title="Warning", easyClose=TRUE))
}

showNotify <- function(...){
  showModal(modalDialog(..., title="Notification", easyClose=TRUE))
}
