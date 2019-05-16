test_gizmo_dynamic_ui <- function(ns) {
  fluidPage(
    shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
    shinyjs::inlineCSS(".fa-tag-green { color: green }"),
    shinyjs::inlineCSS(".fa-tag-orange { color: orange }"),
    shinyjs::inlineCSS(".fa-tag-brown { color: brown }"),
    shinyjs::inlineCSS(".jstree-anchor>.fa-tag-black { color: black }"),	
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns("datatreept"),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreept"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreept"),
      # tags$i(
        # tags$i(class = "fa fa-tag fa-tag-brown", "int"),
        # tags$i(class = "fa fa-tag fa-tag-orange", "float"),
        # tags$i(class = "fa fa-tag fa-tag-green", "char")
      # ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.overflow='auto'")),
      tags$script(paste0("var madatatreept = document.createElement('i');
	    madatatreept.classList.add('fa');madatatreept.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreept-search-input').parentNode.insertBefore(
		madatatreept,document.getElementById('",ns(""),"datatreept-search-input').nextSibling);")),
	  tags$script(paste0("$('#",ns(""),"datatreept').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    ),	
    tags$br(),
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
		madatatreex,document.getElementById('",ns(""),"datatreex-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreex').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
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
    tags$br(),
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns("datatreecolor"),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreecolor"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreecolor"),
      tags$i(
        tags$i(class = "fa fa-tag fa-tag-brown", "int"),
        tags$i(class = "fa fa-tag fa-tag-orange", "float"),
        tags$i(class = "fa fa-tag fa-tag-green", "char")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.overflow='auto'")),
      tags$script(paste0("var madatatreecolor = document.createElement('i');
	    madatatreecolor.classList.add('fa');madatatreecolor.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreecolor-search-input').parentNode.insertBefore(
		madatatreecolor,document.getElementById('",ns(""),"datatreecolor-search-input').nextSibling);")),
	  tags$script(paste0("$('#",ns(""),"datatreecolor').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
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
	
   	#input PLOT TYPE
	output$datatreept <- shinyTree::renderTree(plottypes())
    plottype <- reactive(toString(get_selected(input$datatreept, format = c("names"))))
    output$lbdatatreept <- renderText(paste("PLOT TYPE: ", {
      plottype()
    }))

   	#input X
	output$datatreex <- shinyTree::renderTree(datasets())
	plotx <- reactive(format_local(extract_local(input$datatreex)))
    output$lbdatatreex <- renderText(paste("X: ", {
      show_local(extract_local(input$datatreex))
    }))
	
	#input Y
	output$datatreey <- shinyTree::renderTree(datasets())	
	ploty <- reactive(format_local(extract_local(input$datatreey)))
    output$lbdatatreey <- renderText(paste("Y: ", {
      show_local(extract_local(input$datatreey))
    }))
	
	#input COLOR
	output$datatreecolor <- shinyTree::renderTree(datasets())	
    output$lbdatatreecolor <- renderText(paste("COLOR: ", {
      show_local(extract_local(input$datatreecolor))
    }))

    # RMarkdown Code
    txt_react <- reactive({
      txt <- paste0(
	    "* PLOT: ",toString(plottype())," \n",
	    "*    X: ",toString(plotx())," \n",
		"*    Y: ",toString(ploty())," \n",
		" \n"
	  )
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
          resu <- append(resu, list(c(
            package = pkg,
            data = dd,
            col = slc
          )))
        }, silent = TRUE)
      }
    }
  },
  silent = TRUE)
  resu
}

format_local <- function(resu) {
  library(shinyTree)
  result <- NULL
  for (res in resu) {
	try(result <- append(result, list(c(
		paste0(
			ifelse(res[["package"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["package"]],"::") ),
			res[["data"]],"$",res[["col"]]
		)
	))))
  }
  result
}

show_local <- function(resu) {
  if (length(resu) == 0) {
    "None"
  } else{
    toString(resu)
  }
}

plottypes <- function( ) {
	structure(list(
	  'Line'=structure('Line',sticon=' fa fa-line-chart ',stselected=FALSE), 
	  'Bar'=structure('Bar',sticon=' fa fa-bar-chart ',stselected=FALSE),
	  'Pie'=structure('Pie',sticon=' fa fa-pie-chart ',stselected=FALSE),
	  'Area'=structure('Area',sticon=' fa fa-area-chart ',stselected=FALSE)	  
	),stopened=TRUE)
}