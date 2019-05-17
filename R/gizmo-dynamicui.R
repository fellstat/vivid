test_gizmo_dynamic_ui <- function(ns) {
  fluidPage(
    shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
    shinyjs::inlineCSS(".fa-tag-integer { color: brown }"),
    shinyjs::inlineCSS(".fa-tag-numeric { color: orange }"),
    shinyjs::inlineCSS(".fa-tag-character { color: green }"),
	shinyjs::inlineCSS(".fa-tag-Date { color: red }"),
	shinyjs::inlineCSS(".fa-tag-ts { color: red }"),
	shinyjs::inlineCSS(".fa-tag-orderedfactor { color: purple }"),
	shinyjs::inlineCSS(".fa-tag-factor { color: purple }"),
    shinyjs::inlineCSS(".jstree-anchor>.fa-tag-black { color: black }"),	
	"Select ONLY one each. Jstree will be implemented as JS level!",	
    tags$br(),
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
      tags$i(
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.overflow='auto'")),
      tags$script(paste0("var madatatreept = document.createElement('i');
	    madatatreept.classList.add('fa');madatatreept.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreept-search-input').parentNode.insertBefore(
		madatatreept,document.getElementById('",ns(""),"datatreept-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreept').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    ),	
    tags$br(),
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns("datatreedf"),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreedf"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreedf"),
      tags$i(
        tags$i(class = "fa fa-box", "environment"),
        tags$i(class = "fa fa-tags", "data.frame")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreedf').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreedf').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreedf').style.overflow='auto'")),
      tags$script(paste0("var madatatreedf = document.createElement('i');
	    madatatreedf.classList.add('fa');madatatreedf.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreedf-search-input').parentNode.insertBefore(
		madatatreedf,document.getElementById('",ns(""),"datatreedf-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreedf').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
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
        tags$i(class = "fa fa-tag fa-tag-integer", "integer"),
        tags$i(class = "fa fa-tag fa-tag-numeric", "numeric"),
        tags$i(class = "fa fa-tag fa-tag-character", "character"),
		tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
	    tags$i(class = "fa fa-tag fa-tag-factor", "factor")
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
        tags$i(class = "fa fa-tag fa-tag-integer", "integer"),
        tags$i(class = "fa fa-tag fa-tag-numeric", "numeric"),
        tags$i(class = "fa fa-tag fa-tag-character", "character"),
		tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
	    tags$i(class = "fa fa-tag fa-tag-factor", "factor")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.overflow='auto'")),
      tags$script(paste0("var madatatreey = document.createElement('i');
	    madatatreey.classList.add('fa');madatatreey.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreey-search-input').parentNode.insertBefore(
		madatatreey,document.getElementById('",ns(""),"datatreey-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreey').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
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
        tags$i(class = "fa fa-tag fa-tag-integer", "integer"),
        tags$i(class = "fa fa-tag fa-tag-numeric", "numeric"),
        tags$i(class = "fa fa-tag fa-tag-character", "character"),
		tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
	    tags$i(class = "fa fa-tag fa-tag-factor", "factor")
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxHeight='400px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.overflow='auto'")),
      tags$script(paste0("var madatatreecolor = document.createElement('i');
	    madatatreecolor.classList.add('fa');madatatreecolor.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreecolor-search-input').parentNode.insertBefore(
		madatatreecolor,document.getElementById('",ns(""),"datatreecolor-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreecolor').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
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
	
    datasets <- reactiveVal(NAlist())
	datadfs  <- reactiveVal(NAlist())
	
    remote_eval(vivid:::texasCi(), function(obj) {
      datasets(obj$Tree0s)
	  datadfs (obj$Traa0s)
    })
	
   	#input PLOT TYPE
	output$datatreept <- shinyTree::renderTree(plottypes())
    plottype <- reactive(toString(get_selected(input$datatreept, format = c("names"))))
    output$lbdatatreept <- renderText(paste("PLOT TYPE: ", {
      show_local(plottype())
    }))
	
	#input DATA FRAME
	output$datatreedf <- shinyTree::renderTree(datadfs())	
    plotdf <- reactive(format_local2(extract_local2(input$datatreedf)))
    output$lbdatatreedf <- renderText(paste("DATA FRAME: ", {
      show_local(extract_local2(input$datatreedf))
    }))
	
   	#input X
	output$datatreex <- shinyTree::renderTree(filter_df(datasets(), extract_local2(input$datatreedf)))
	inputdatatreex <- reactiveVal(NAlist())
	observeEvent(input$datatreex,{inputdatatreex(input$datatreex) } ,ignoreNULL = FALSE)
	observeEvent(input$datatreedf,{inputdatatreex(NAlist())} ,ignoreNULL = FALSE)
	plotx <- reactive(format_local(extract_local(inputdatatreex())))
    output$lbdatatreex <- renderText(paste("X: ", {
      show_local(extract_local(inputdatatreex()))
    }))
	
   	#input Y
	output$datatreey <- shinyTree::renderTree(filter_df(datasets(), extract_local2(input$datatreedf)))
	inputdatatreey <- reactiveVal(NAlist())
	observeEvent(input$datatreey,{inputdatatreey(input$datatreey) } ,ignoreNULL = FALSE)
	observeEvent(input$datatreedf,{inputdatatreey(NAlist())} ,ignoreNULL = FALSE)
	ploty <- reactive(format_local(extract_local(inputdatatreey())))
    output$lbdatatreey <- renderText(paste("Y: ", {
      show_local(extract_local(inputdatatreey()))
    }))
	
	#input COLOR
	output$datatreecolor <- shinyTree::renderTree(NAlist())	
	plotcolor <- reactive(format_local(extract_local(input$datatreecolor)))
    output$lbdatatreecolor <- renderText(paste("COLOR: ", {
      show_local(extract_local(input$datatreecolor))
    }))

    # RMarkdown Code
    txt_react <- reactive({
      txt <- paste0(
	    "* PLOT: ",toString(plottype())," \n",
		"* DATA: ",toString(plotdf())," \n",
	    "* X: ",toString(plotx())," \n",
		"* Y: ",toString(ploty())," \n",
		"* COLOR: ",toString(plotcolor())," \n",
		
	    "```{r}\n",
	    "library(ggplot2)\n",
		
		if (toString(plottype())=="histogram"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(",toString(plotx()),")) +  "," \n",
		"  geom_histogram(bins=20) +   "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		if (toString(plottype())=="bar"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(",toString(plotx()),")) +  "," \n",
		"  geom_bar() +                "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},	

		if (toString(plottype())=="box"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(",toString(plotx()),")) +  "," \n",
		"  geom_boxplot() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},			
		
		if (toString(plottype())=="line"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(x=seq_along(",toString(plotx()),"),y=",toString(plotx()),")) +  "," \n",
		"  geom_line() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},	
		
		if (toString(plottype())=="area"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(x=seq_along(",toString(plotx()),"),y=",toString(plotx()),")) +  "," \n",
		"  geom_area() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},	
		
		if (toString(plottype())=="pie"){paste0(
		"(                             "," \n",
		"  ggplot(",toString(plotdf()),", aes(x=factor(1),fill=",toString(plotx()),")) +  "," \n",
		"  geom_bar(width = 1) +            "," \n",
		"  coord_polar('y') #+            "," \n",
		"  #theme_bw()                  "," \n",
		") #%>% plotly::ggplotly()      "," \n"
		)}else{""},	
		

	    "```\n",
		
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
            col = slc,
            dt = get_dt(attr(datatreex[[pkg]][[dd]][[slc]], "sticon"))
          )))
        }, silent = TRUE)
      }
    }
  },
  silent = TRUE)
  resu
}

get_dt<- function(resu) {
	kk=strsplit(toString(resu), " ")[[1]]
	substr(kk[[length(kk)]],8,1000)
}

extract_local2 <- function(datatreex) {
  library(shinyTree)
  resu <- list()
  try(for (pkg in names(datatreex)) {
    for (dd in names(datatreex[[pkg]])) {
        try(if (attr(datatreex[[pkg]][[dd]], "stselected")) {
          resu <- append(resu, list(c(
            package = pkg,
            data = dd
          )))
        }, silent = TRUE)
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

format_local2 <- function(resu) {
  library(shinyTree)
  result <- NULL
  for (res in resu) {
	try(result <- append(result, list(c(
		paste0(
			ifelse(res[["package"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["package"]],"::") ),
			res[["data"]]
		)
	))))
  }
  result
}

show_local <- function(resu) {
  if (length(resu) == 0) {
    "None"
  } else if (toString(resu) == "") {
    "None"
  } else {
    toString(resu)
  }
}

plottypes <- function( ) {
	structure(list(
	  'histogram'=structure('histogram',sticon=' fa fa-bar-chart ',stselected=FALSE),
	  'line'=structure('line',sticon=' fa fa-line-chart ',stselected=FALSE), 
	  'bar'=structure('bar',sticon=' fa fa-bar-chart ',stselected=FALSE),
	  'box'=structure('box',sticon=' fa fa-tag ',stselected=FALSE),
	  'pie'=structure('pie',sticon=' fa fa-pie-chart ',stselected=FALSE),
	  'area'=structure('area',sticon=' fa fa-area-chart ',stselected=FALSE)	  
	),stopened=TRUE)
}



filter_df <- function (original, criterias){
  Tree0s=NULL
  for (pkg in names(original)) {
    for (dd in names(original[[pkg]])) {
		for (criteria in criterias){
			      if(pkg==criteria[["package"]] & dd==criteria[["data"]] ){
	        for (slc in names(original[[pkg]][[dd]])) {				
				Tree0s[[pkg]][[dd]]=original[[pkg]][[dd]]
				attr(Tree0s[[pkg]][[dd]], "stopened")=TRUE
				attr(Tree0s[[pkg]], "sttype")="pkg-node"
				attr(Tree0s[[pkg]], "sticon")="fas fa-box"
				attr(Tree0s[[pkg]], "stopened")=TRUE
			}
			      }
			
		}
    }
  }
  if(is.null(Tree0s)){
     NAlist()
  } else {
    Tree0s
  }
}

NAlist <- function ( ){
list("NA" = structure("NA",sttype='pkg-node',sticon = ' fa fa-warning'))
}