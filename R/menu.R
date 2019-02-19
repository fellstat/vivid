add_menu_item <-function(input_id, label, menu_id="vivid-navbar-ul", active=FALSE, ...){
  active <- if(active) "active" else NULL
  insertUI(
    paste0("#", menu_id),
    "beforeEnd",
    ui = tags$li(
      class=active,
      action_link(input_id, label, ...)
    )
  )
}

add_menu <- function(input_id, label, ...){
  insertUI(
    "#vivid-navbar-ul",
    "beforeEnd",
    ui = tags$li(
      class="dropdown",
      tags$a(
        href="#",
        class="dropdown-toggle",
        `data-toggle`="dropdown",
        role="button",
        `aria-haspopup`="true",
        `aria-expanded`="false",
        label,
        tags$span(class="caret")
      ),
      tags$ul(
        id=input_id,
        class="dropdown-menu"
      )
    )
  )
}

make_menu <- function(){
  add_menu("vivid_menu_session","Session")
  add_menu("vivid_menu_code","Code")
  add_menu("vivid_menu_data","Data")
  add_menu("vivid_menu_analysis","Analysis")
  add_menu("vivid_menu_vis","Visualize")

  add_menu_item("menu_insert_markdown_block",
                "Insert Markdown Block",
                "vivid_menu_code")

  add_menu_item("gizmo_test",
                "Gizmo Test",
                "vivid_menu_analysis")

  add_menu_item("gizdata",
                "Load Data From Package",
                "vivid_menu_data")

  add_menu_item("scatter_3d",
                "3D Scatter Plot",
                "vivid_menu_vis")

  add_menu_item("new_doc",
                "New Document",
                "vivid_menu_session")

  add_menu_item("save_doc",
                "Save Document",
                "vivid_menu_session")

  add_menu_item("load_doc",
                "Load Document",
                "vivid_menu_session")

  add_menu_item("doc_to_markdown",
                "Convert to markdown",
                "vivid_menu_session")

  add_menu_item("doc_to_r_script",
                "Convert to R script",
                "vivid_menu_session")
}
