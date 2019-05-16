test_gizmo_dynamic_ui <- function(ns) {
  fluidPage(
    shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
    shinyjs::inlineCSS(".fa-tag-green { color: green }"),
    shinyjs::inlineCSS(".fa-tag-orange { color: orange }"),
    shinyjs::inlineCSS(".fa-tag-brown { color: brown }"),
    shinyjs::inlineCSS(".jstree-anchor>.fa-tag-black { color: black }"),
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns("datatreex"),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreex"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreex"),
      tags$i(
        tags$i(class = "fa fa-tag fa-tag-brown", "int"),
        tags$i(class = "fa fa-tag fa-tag-orange", "float"),
        tags$i(class = "fa fa-tag fa-tag-green", "char")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreex').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreex').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreex').style.overflow='auto'")),
      tags$script(paste0("var madatatreex = document.createElement('i');
	    madatatreex.classList.add('fa');madatatreex.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreex-search-input').parentNode.insertBefore(
		madatatreex,document.getElementById('",ns(""),"datatreex-search-input').nextSibling);")),
	  tags$script(paste0("$('#",ns(""),"datatreex').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    ),	
    tags$br(),
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns("datatreey"),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreey"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreey"),
      tags$i(
        tags$i(class = "fa fa-tag fa-tag-brown", "int"),
        tags$i(class = "fa fa-tag fa-tag-orange", "float"),
        tags$i(class = "fa fa-tag fa-tag-green", "char")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.overflow='auto'")),
      tags$script(paste0("var madatatreey = document.createElement('i');
	    madatatreey.classList.add('fa');madatatreey.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreey-search-input').parentNode.insertBefore(
		madatatreey,document.getElementById('",ns(""),"datatreey-search-input').nextSibling);")),
	  tags$script(paste0("$('#",ns(""),"datatreey').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    ),	
    tags$br()
  )
}

test_gizmo_dynamic_server <- function(input, output, session, state = NULL) {

    # Restore UI state
    if (!is.null(state)) {
      session$onFlushed(function() {
      })
    }
	
    datasets <- reactiveVal(list("NA" = structure("NA", sticon = 'fa fa-warning')))
	
    remote_eval(vivid:::texasCi(), function(obj) {
      datasets(obj)
    })
	

   	#input X
	output$datatreex <- shinyTree::renderTree({
      datasets()
    })

    output$lbdatatreex <- renderText(paste("X: ", {
      resu <- extract_local(input$datatreex)
      if (length(resu) == 0) {
        "None"
      } else{
        toString(resu)
      }
    }))
	
	#input Y
	output$datatreey <- shinyTree::renderTree({
      datasets()
    })
	
    output$lbdatatreey <- renderText(paste("Y: ", {
      resu <- extract_local(input$datatreey)
      if (length(resu) == 0) {
        "None"
      } else{
        toString(resu)
      }
    }))

    # RMarkdown Code
    txt_react <- reactive({
      txt <- paste0("## knitr::kable( )")
      txt
    })

    # Get UI state
    get_state <- function() {
      list(`__version__` = "1.0")
    }
    list(code = txt_react,
         get_state = get_state)
  }


.globals$gizmos$dynamicui <- list(
  ui = test_gizmo_dynamic_ui,
  server = test_gizmo_dynamic_server,
  library = "vivid",
  opts = list()
)


run_dynamic_ui <- function()
  run_standalone("dynamicui")


extract_local <- function(datatreex) {
  library(shinyTree)
  resu <- list()
  try(for (pkg in names(datatreex)) {
    for (dd in names(datatreex[[pkg]])) {
      for (slc in names(datatreex[[pkg]][[dd]])) {
        try(if (attr(datatreex[[pkg]][[dd]][[slc]], "stselected")) {
          resu <- c(resu, list(
            package = pkg,
            data = dd,
            col = slc
          ))
        }, silent = TRUE)
      }
    }
  },
  silent = TRUE)
  resu
}
