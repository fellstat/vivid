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
				   'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns("lbdatatreept"), inline = TRUE),
      #width = "300px",
      inputId = ns("iidatatreept"),
      tags$i(
      ),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.maxHeight='400px'")),
	  tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreept').style.minWidth='300px'")),
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
				   'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
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
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreedf').style.minWidth='300px'")),
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
				   'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
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
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreex').style.minWidth='300px'")),
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
				   'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
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
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.minWidth='300px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.maxWidth='350px'")),
      tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreey').style.overflow='auto'")),
      tags$script(paste0("var madatatreey = document.createElement('i');
	    madatatreey.classList.add('fa');madatatreey.classList.add('fa-search');
	    document.getElementById('",ns(""),"datatreey-search-input').parentNode.insertBefore(
		madatatreey,document.getElementById('",ns(""),"datatreey-search-input').nextSibling);"))#,
	  #tags$script(paste0("$('#",ns(""),"datatreey').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    ),
    tags$br(),
    # shinyWidgets::dropdownButton(
      # shinyTree::shinyTree(
        # ns("datatreecolor"),
        # checkbox = TRUE,
        # search = TRUE,
        # types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				   # 'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
      # ),
      # circle = FALSE,
      # icon = icon("gear"),
      # label = textOutput(ns("lbdatatreecolor"), inline = TRUE),
      # #width = "300px",
      # inputId = ns("iidatatreecolor"),
      # tags$i(
        # tags$i(class = "fa fa-tag fa-tag-integer", "integer"),
        # tags$i(class = "fa fa-tag fa-tag-numeric", "numeric"),
        # tags$i(class = "fa fa-tag fa-tag-character", "character"),
		# tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
	    # tags$i(class = "fa fa-tag fa-tag-factor", "factor")
      # ),
      # tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxHeight='400px'")),
      # tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.minWidth='300px'")),
      # tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.maxWidth='350px'")),
      # tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"iidatatreecolor').style.overflow='auto'")),
      # tags$script(paste0("var madatatreecolor = document.createElement('i');
	    # madatatreecolor.classList.add('fa');madatatreecolor.classList.add('fa-search');
	    # document.getElementById('",ns(""),"datatreecolor-search-input').parentNode.insertBefore(
		# madatatreecolor,document.getElementById('",ns(""),"datatreecolor-search-input').nextSibling);"))#,
	  # #tags$script(paste0("$('#",ns(""),"datatreecolor').bind('activate_node.jstree', function (event, data) { if (data.instance.get_checked().length > 1) { data.instance.uncheck_all(); } });"))
    # ),
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

	ptdisablex <- reactiveVal(FALSE)
	ptdisabley <- reactiveVal(FALSE)

	#input DATA FRAME
	output$datatreedf <- shinyTree::renderTree(datadfs())
	outputdatatreedf_old <- reactiveVal(datadfs())  #single selection ctrl
	observeEvent(input$datatreedf,{   #single selection
		T <- extract_local2(input$datatreedf)
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreedf",  outputdatatreedf_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreedf_old()[[T[[1]][['package']]]][[T[[1]][['data']]]],'stselected'))){
				outputdatatreedf_temp <- datadfs()
				attr(outputdatatreedf_temp[[T[[2]][['package']]]][[T[[2]][['data']]]],'stselected')=TRUE
				outputdatatreedf_old(outputdatatreedf_temp)
			}else{
				outputdatatreedf_temp <- datadfs()
				attr(outputdatatreedf_temp[[T[[1]][['package']]]][[T[[1]][['data']]]],'stselected')=TRUE
				outputdatatreedf_old(outputdatatreedf_temp)
			}			
			shinyTree::updateTree(session, "datatreedf",  outputdatatreedf_old() )
		}else if(length(T)==1){
			outputdatatreedf_temp <- datadfs()
			attr(outputdatatreedf_temp[[T[[1]][['package']]]][[T[[1]][['data']]]],'stselected')=TRUE
			outputdatatreedf_old(outputdatatreedf_temp)
		}else{
			outputdatatreedf_old(datadfs())
		}		
	})	
    plotdf <- reactive( format_local2(extract_local2(outputdatatreedf_old())) )
    output$lbdatatreedf <- renderText(paste("DATA FRAME: ", {
      toStringB(extract_local2(outputdatatreedf_old()))
    }))

   	#input X
	output$datatreex <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),ptdisablex()) )
	inputdatatreexx <- reactiveVal(NAlist())
	inputdatatreex <- reactiveVal(NAlist())
	observeEvent(ptdisablex(),{inputdatatreex(filter_dis(inputdatatreexx(),ptdisablex())) } ,ignoreNULL = FALSE)
	observeEvent(inputdatatreexx(),{message("I got input B");browser();inputdatatreex(filter_dis(inputdatatreexx(),ptdisablex())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreex,{message("I got input A");inputdatatreexx(input$datatreex) } ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{inputdatatreexx(NAlist())} ,ignoreNULL = FALSE)
	plotx <- reactive(format_local(extract_local(inputdatatreex())))
    output$lbdatatreex <- renderText(paste("X: ", {
      toStringB(extract_local(inputdatatreex()))
    }))

   	#input Y
	output$datatreey <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),ptdisabley()) )
	inputdatatreeyy <- reactiveVal(NAlist())
	inputdatatreey <- reactiveVal(NAlist())
	observeEvent(ptdisabley(),{inputdatatreey(filter_dis(inputdatatreeyy(),ptdisabley())) } ,ignoreNULL = FALSE)	
	observeEvent(inputdatatreeyy(),{inputdatatreey(filter_dis(inputdatatreeyy(),ptdisabley())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreey,{inputdatatreeyy(input$datatreey) } ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{inputdatatreeyy(NAlist())} ,ignoreNULL = FALSE)
	ploty <- reactive(format_local(extract_local(inputdatatreey())))
    output$lbdatatreey <- renderText(paste("Y: ", {
      toStringB(extract_local(inputdatatreey()))
    }))

	# #input COLOR
	# output$datatreecolor <- shinyTree::renderTree(NAlist())
	# plotcolor <- reactive(format_local(extract_local(input$datatreecolor)))
    # output$lbdatatreecolor <- renderText(paste("COLOR: ", {
      # toStringB(extract_local(input$datatreecolor))
    # }))
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
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
				dt = get_dt(attr(datasets()[[pkg]][[dd]][[slc]], "sticon"))
			  )))
			}, silent = TRUE)
		  }
		}
	  },
	  silent = TRUE)
	  resu
	}
		

	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

   	#input PLOT TYPE
	output$datatreept <- shinyTree::renderTree(NApt())
	inputdatatreept <- reactiveVal(NApt())
	observeEvent(input$datatreept,{inputdatatreept(input$datatreept) } ,ignoreNULL = FALSE)
	#observeEvent(outputdatatreedf_old(),{inputdatatreept(NApt())} ,ignoreNULL = FALSE)
    plottype <- reactive(  filter_pt(get_selected(inputdatatreept(), format = c("names")),extract_local(inputdatatreex()),extract_local(inputdatatreey())))
    output$lbdatatreept <- renderText(paste("PLOT TYPE: ", {
      toStringB(plottype(),'Auto')
    }))
	observeEvent(input$datatreept,{
	    temp <- get_selected(inputdatatreept(), format = c("names"))
		if (length(temp)>0){
			if(!is.element('auto', temp)){
				disablex=TRUE;disabley=TRUE
				if(is.element("histogram", temp)) {disablex=FALSE}
				if(is.element("bar", temp)) {disablex=FALSE}
				if(is.element("box", temp)) {disabley=FALSE}
				if(is.element("bar2", temp)) {disabley=FALSE}
				if(is.element("scatter", temp)) {disablex=FALSE;disabley=FALSE}
				if(is.element("box2", temp)) {disablex=FALSE;disabley=FALSE}
				if(is.element("histogram2", temp)) {disablex=FALSE;disabley=FALSE}
				if(is.element("grid", temp)) {disablex=FALSE;disabley=FALSE}
				
				if(is.element("line", temp)) {disablex=FALSE;disabley=FALSE}
				if(is.element("area", temp)) {disablex=FALSE;disabley=FALSE}
				ptdisablex(disablex)
				ptdisabley(disabley)
			}else{
				disablex=FALSE;disabley=FALSE				
				ptdisablex(disablex)
				ptdisabley(disabley)
			}
		}else{
				disablex=FALSE;disabley=FALSE				
				ptdisablex(disablex)
				ptdisabley(disabley)
		}
	
	} ,ignoreNULL = FALSE)
	

	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

    # RMarkdown Code
    txt_react <- reactive({
      txt <- paste0(
	    "* PLOT: ",toString(pt_autofree(plottype()))," \n",
		"* DATA: ",toString(plotdf())," \n",
	    "* X: ",toString(plotx())," \n",
		"* Y: ",toString(ploty())," \n",
		#"* COLOR: ",toString(plotcolor())," \n",
		" \n",
		" \n",
		
	    "```{r}\n",
	    "library(ggplot2)\n",

		if (toString(pt_autofree(plottype()))=="histogram" & length(get_col(plotx()))>0 ){paste0(
		"#(numeric x)                  "," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(plotx())),")) +  "," \n",
		"  geom_histogram(bins=20) +   "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (toString(pt_autofree(plottype()))=="bar" & length(get_col(plotx()))>0 ){paste0(
		"#(categorical x)              "," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(plotx())),")) +  "," \n",
		"  geom_bar() +                "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (toString(pt_autofree(plottype()))=="box" & length(get_col(ploty()))>0 ){paste0(
		"#(numeric y)                  "," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(ploty())),", x = \"\")) +  "," \n",
		"  geom_boxplot() +            "," \n",
		"  xlab(\"\") +                  "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (toString(pt_autofree(plottype()))=="bar2" & length(get_col(ploty()))>0 ){paste0(
		"#(categorical y)              "," \n",
		"(                             "," \n",
		" ",toString(plotdf())," %>%                                  "," \n",
		"  dplyr::group_by(",toString(get_col(ploty())),") %>%        "," \n",
		"  dplyr::summarise( count= dplyr::n()) %>%                   "," \n",
		"  ggplot(aes(x=count, y=",toString(get_col(ploty())),")) +   "," \n",
		"  geom_point(size=2) +                                       "," \n",
		"  geom_errorbarh(aes(xmax=count), xmin=0, height=0) +        "," \n",
		"  theme_bw()                                                 "," \n",
		") %>% plotly::ggplotly()                                     "," \n"
		)}else{""},
		
		
		if (toString(pt_autofree(plottype()))=="scatter" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"#(numeric x and y)            "," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(plotx())),",",toString(get_col(ploty())),")) +  "," \n",
		"  geom_point() +              "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (toString(pt_autofree(plottype()))=="box2" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"#(categorical x numeric y)    "," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(plotx())),",",toString(get_col(ploty())),")) +  "," \n",
		"  geom_boxplot() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (toString(pt_autofree(plottype()))=="histogram2" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"#(numeric x categorical y)    "," \n",
		"#(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(",toString(get_col(plotx())),",",toString(get_col(ploty())),")) +  "," \n",
		"  ggridges::stat_binline(bins = 50, scale = .7, draw_baseline = FALSE) +     "," \n",
		"  ggridges::theme_ridges()               "," \n",
		"#) %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (toString(pt_autofree(plottype()))=="grid" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"#(categorical x categorical y)"," \n",
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(x=seq_along(",toString(get_col(plotx())),"),y=",toString(get_col(ploty())),", fill=stat(count))) +  "," \n",
		"  stat_bin2d() +              "," \n",
		"  scale_fill_gradient2() +     "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},		
		
		
		######################################################@@@@@@@@@@@@@@@@@@

		if (toString(pt_autofree(plottype()))=="line" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(x=seq_along(",toString(get_col(plotx())),"),y=",toString(get_col(ploty())),")) +  "," \n",
		"  geom_line() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},

		if (toString(pt_autofree(plottype()))=="area" & length(get_col(plotx()))>0 & length(get_col(ploty()))>0 ){paste0(
		"(                             "," \n",
		" ggplot(",toString(plotdf()),", aes(x=seq_along(",toString(get_col(plotx())),"),y=",toString(get_col(ploty())),")) +  "," \n",
		"  geom_area() +            "," \n",
		"  theme_bw()                  "," \n",
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},

		# if (toString(pt_autofree(plottype()))=="pie"){paste0(
		# "(                             "," \n",
		# "  ggplot(",toString(plotdf()),", aes(x=factor(1),fill=",toString(plotx()),")) +  "," \n",
		# "  geom_bar(width = 1) +            "," \n",
		# "  coord_polar('y') #+            "," \n",
		# "  #theme_bw()                  "," \n",
		# ") #%>% plotly::ggplotly()      "," \n"
		# )}else{""},

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

toStringB <- function(resu,zeromessage="None") {
  if (length(resu) == 0) {
    zeromessage
  } else if (toString(resu) == "") {
    zeromessage
  } else {
    toString(resu)
  }
}

NApt <- function( ) {
	structure(list(
	  'auto'=structure('auto',sticon=' fa fa-oil-can ',stselected=TRUE),
	  'area'=structure('area',sticon=' fa fa-area-chart ',stselected=FALSE),
	  'bar'=structure('bar',sticon=' fas fa-tachometer-alt ',stselected=FALSE),
	  'bar2'=structure('bar2',sticon=' fas fa-tachometer-alt fa-tag-numeric ',stselected=FALSE),
	  'box'=structure('box',sticon=' fas fa-inbox ',stselected=FALSE),
	  'box2'=structure('box',sticon=' fas fa-inbox  fa-tag-numeric ',stselected=FALSE),
	  'grid'=structure('grid',sticon=' fas fa-car-battery ',stselected=FALSE),
	  'histogram'=structure('histogram',sticon=' fa fa-bar-chart ',stselected=FALSE),
	  'histogram2'=structure('histogram2',sticon=' fa fa-bar-chart  fa-tag-numeric ',stselected=FALSE),
	  'line'=structure('line',sticon=' fa fa-line-chart ',stselected=FALSE),
	  #'pie'=structure('pie',sticon=' fa fa-pie-chart ',stselected=FALSE), #Not supported
	  'scatter'=structure('scatter',sticon=' fas fa-braille ',stselected=FALSE)

	),stopened=TRUE)
}



filter_df <- function (original, criterias){
  Tree0s=NULL
  for (pkg in names(original)) {
    for (dd in names(original[[pkg]])) {
		for (criteria in criterias){
			      if(pkg==criteria[["package"]] & dd==criteria[["data"]] ){
	        for (slc in names(original[[pkg]][[dd]])) {
				Tree0s[[pkg]][[dd]] <- original[[pkg]][[dd]]
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

filter_pt <- function (original,x,y){
	if(!is.element('auto', original) & length(original) > 0) {
		original
	}else{
		decide_pt(x,y)
	}
}
decide_pt <- function (xx,yy){
    result <- NULL
	if(!length(xx)&!length(yy)){
		result <- list( structure("auto",implied='unknown'))
	}else if( length(xx)&!length(yy)){
		for (xxx in xx){
			result<-c(result, if( judge_numeric(xxx) ){
									structure("auto histogram",implied='histogram')
							  }else if( judge_categorical(xxx) ){
							        structure("auto bar",implied='bar')
							  }else{
							        structure("auto",implied='unknown')
							  }

			)
		}
	}else if(!length(xx)& length(yy)){
		for (yyy in yy){
			result<-c(result, if( judge_numeric(yyy) ){
									structure("auto box",implied='box')
							  }else if( judge_categorical(yyy) ){
							        structure("auto bar2",implied='bar2')
							  }else{
							        structure("auto",implied='unknown')
							  }

			)
		}
	}else{
		for (xxx in xx){
			for (yyy in yy){
				result<-c(result, if( judge_numeric(xxx) & judge_numeric(yyy) ){
										structure("auto scatter",implied='scatter')
									}else if( judge_categorical(xxx) & judge_numeric(yyy) ){
										structure("auto box2",implied='box2')
									}else if( judge_numeric(xxx) & judge_categorical(yyy) ){
										structure("auto histogram2",implied='histogram2')
									}else if( judge_categorical(xxx) & judge_categorical(yyy) ){
										structure("auto grid",implied='grid')
									}else{
										structure("auto",implied='unknown')
									}
				)
			}
		}
	}
	result

}    #original <- c(original,   list('auto numeric'=structure("auto numeric",implied='numeric')))


judge_numeric <- function (res){
   #is.element(res['dt'],c('numeric','integer','Date','ts'))
   !judge_categorical(res)
}
judge_categorical <- function (res){
   is.element(res['dt'],c('orderedfactor','factor','character'))
}
#Date ts are unknown type

pt_autofree <- function(resu) {
	result <- NULL
	for (res in resu){
	    if (substr(res,1,5)=="auto "){
		    result <- c(result, substr(res,6,1000) )
		}else{
			result <- c(result, res )
		}
	}
	result
}

get_col <- function(resu) {
	result <- NULL
	for (res in resu){
		    result <- c(result, substr(res,stringr::str_locate(res, "\\$")[[1]]+1,1000) )
	}
	result
}

filter_dis <- function(resu,disabled=FALSE,fill=NAlist()) {
	if(disabled){
		fill
	}else{
		resu
	}
}