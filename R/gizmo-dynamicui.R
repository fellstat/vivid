ctrlJS <- function (...){
  tags$i(
    shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
    shinyjs::inlineCSS(".fa-tag-integer { color: brown }"),
    shinyjs::inlineCSS(".fa-tag-numeric { color: orange }"),
    shinyjs::inlineCSS(".fa-tag-character { color: green }"),
	shinyjs::inlineCSS(".fa-tag-Date { color: red }"),
	shinyjs::inlineCSS(".fa-tag-ts { color: red }"),
	shinyjs::inlineCSS(".fa-tag-orderedfactor { color: purple }"),
	shinyjs::inlineCSS(".fa-tag-factor { color: purple }"),
    shinyjs::inlineCSS(".jstree-anchor>.fa-tag-black { color: black }")
  )
}

ctrlA <- function (ns, ctrlname, ...){
    shinyWidgets::dropdownButton(
      shinyTree::shinyTree(
        ns(ctrlname),
        checkbox = TRUE,
        search = TRUE,
        types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}},
				   'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}  }"
      ),
      circle = FALSE,
      icon = icon("gear"),
      label = textOutput(ns(paste0("lb",ctrlname)), inline = TRUE),
      inputId = ns(paste0("ii",ctrlname)),	  
      tags$i(
	  tags$br(),
        tags$i(class = "fa fa-box", "environment"),
        tags$i(class = "fa fa-tags", "data.frame"),
		tags$br(),
        tags$i(class = "fa fa-tag fa-tag-integer", "integer"),
        tags$i(class = "fa fa-tag fa-tag-numeric", "numeric"),
        tags$i(class = "fa fa-tag fa-tag-character", "character"),		
		tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
	    tags$i(class = "fa fa-tag fa-tag-factor", "factor")
      ),
      tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.maxHeight='400px'")),
	  tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.minWidth='300px'")),
      tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.maxWidth='500px'")),
      tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.overflow='auto'")),
      tags$script(paste0("var madatatreept = document.createElement('i');
	    madatatreept.classList.add('fa');madatatreept.classList.add('fa-search');
	    document.getElementById('",ns(paste0(ctrlname,"-search-input")),"').parentNode.insertBefore(
		madatatreept,document.getElementById('",ns(paste0(ctrlname,"-search-input")),"').nextSibling);"))
    )
}


ctrlB <- function (ns, ctrlname, ...){
  shiny::column(3,shinyWidgets::dropdown(
	    ...,
        circle = FALSE,
        icon = icon("gear"),
        label = toupper(stringr::str_remove(ctrlname, "-panel")),
        inputId = ns(paste0("ii",ctrlname)),
		up = TRUE, 
		tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.width='100%'")),
        tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.maxHeight='400px'")),
        tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.minWidth='300px'")),
        tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.maxWidth='350px'")),
        tags$script(paste0("document.getElementById('",paste0("dropdown-menu-",ns(paste0("ii",ctrlname))),"').style.overflow='auto'"))
      ))
}


test_gizmo_dynamic_ui <- function(ns) {
  fluidPage(
	ctrlJS(),
	ctrlA(ns,"datatreept"),tags$br(),
	ctrlA(ns,"datatreedf"),tags$br(),
	ctrlA(ns,"datatreex"),tags$br(),
	ctrlA(ns,"datatreey"),tags$br(),
    tags$br(),
	fluidRow(
	  column(6,ctrlA(ns,"datatreecolor")),
	  column(6,ctrlA(ns,"datatreefacet"))
	),
	tags$br(),
	fluidRow(
      ctrlB(ns,"general-panel",
	    checkboxInput(ns("ggtitle"), "GGTITLE"),
		textInput(ns("ggtitle_label"), "GGTITLE  LABEL"),
		checkboxInput(ns("stat_summary"), "STAT SUMMARY", FALSE)),
      ctrlB(ns,"x-axis-panel",
	    checkboxInput(ns("xlab"), "X LAB"),
        textInput(ns("xlab_label"), "X LAB LABEL"),
		checkboxInput(ns("scale_x_log10"), "SCALE X LOG 10", FALSE)),
      ctrlB(ns,"y-axis-panel",
	    checkboxInput(ns("ylab"), "Y LAB"),
        textInput(ns("ylab_label"), "Y LAB LABEL"),
		checkboxInput(ns("scale_y_log10"), "SCALE Y LOG 10", FALSE)),
	  ctrlB(ns,"coord_flip-panel",
	    checkboxInput(ns("coord_flip"), "COORD FLIP")),
      ctrlB(ns,"debug-panel",
        textAreaInput(ns("customized_code"), "CUSTOMIZED CODE", width='100%')),
	  ctrlB(ns,"theme-panel",
	    checkboxInput(ns("theme"), "THEME", TRUE),
        pickerInput(ns("theme_fun"), label = "THEME FUN", choices = theme_choices(), selected = "theme_bw" ), 
		numericInput(ns("theme_base_size"), "THEME BASE SIZE", 12, min = 1, max = 100)),
	  ctrlB(ns,"geom_violin-panel",
	    checkboxInput(ns("geom_violin"), "VIOLIN"),
		textInput(ns("geom_violin_color"), "VIOLIN COLOR", "white"),
		textInput(ns("geom_violin_fill"), "VIOLIN FILL", "grey90")),
	  ctrlB(ns,"geom_histogram-panel",
	    checkboxInput(ns("geom_histogram"), "HISTOGRAM"),
		numericInput(ns("geom_histogram_bins"), "BINS", 20, min = 1, max = 100)),
	  ctrlB(ns,"geom_bar-panel",
	    checkboxInput(ns("geom_bar"), "BAR")),
	  ctrlB(ns,"geom_boxplot-panel",
	    checkboxInput(ns("geom_boxplot"), "BOXPLOT")),
	  ctrlB(ns,"geom_point-panel",
	    checkboxInput(ns("geom_point"), "POINT"),
		numericInput(ns("geom_point_size"), "POINT SIZE", 2, min = 1, max = 100)
		)
	),
    tags$br(),
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

	ptdisabletreex <- reactiveVal(FALSE)
	ptdisabletreey <- reactiveVal(FALSE)
	ptdisabletreecolor <- reactiveVal(FALSE)
	ptdisabletreefacet <- reactiveVal(FALSE)

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
	output$datatreex <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old()),outputdatatreex_old()),ptdisabletreex()) )
	outputdatatreex_old <- reactiveVal(NAlist())
	inputdatatreex <- reactiveVal(NAlist())
	observeEvent(ptdisabletreex(),{inputdatatreex(filter_dis(outputdatatreex_old(),ptdisabletreex())) } ,ignoreNULL = FALSE)
	observeEvent(outputdatatreex_old(),{inputdatatreex(filter_dis(outputdatatreex_old(),ptdisabletreex())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreex,{
		T <- extract_local(input$datatreex)		
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreex",  outputdatatreex_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreex_old()[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected'))){
				outputdatatreex_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreex_temp[[T[[2]][['package']]]][[T[[2]][['data']]]][[T[[2]][['col']]]],'stselected')=TRUE
				outputdatatreex_old(outputdatatreex_temp)
			}else{
				outputdatatreex_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreex_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
				outputdatatreex_old(outputdatatreex_temp)
			}			
			shinyTree::updateTree(session, "datatreex",  outputdatatreex_old() )
		}else if(length(T)==1){
			outputdatatreex_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
			attr(outputdatatreex_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
			outputdatatreex_old(outputdatatreex_temp)
		}else{
			outputdatatreex_old(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE))
		}
	} ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{outputdatatreex_old(NAlist())} ,ignoreNULL = FALSE)
	plottreex <- reactive(format_local(extract_local(inputdatatreex())))
    output$lbdatatreex <- renderText(paste("X: ", {
      toStringB(extract_local(inputdatatreex()),ptdisabletreex())
    }))

   	#input Y
	output$datatreey <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old()),outputdatatreey_old()),ptdisabletreey()) )
	outputdatatreey_old <- reactiveVal(NAlist())
	inputdatatreey <- reactiveVal(NAlist())
	observeEvent(ptdisabletreey(),{inputdatatreey(filter_dis(outputdatatreey_old(),ptdisabletreey())) } ,ignoreNULL = FALSE)	
	observeEvent(outputdatatreey_old(),{inputdatatreey(filter_dis(outputdatatreey_old(),ptdisabletreey())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreey,{
		T <- extract_local(input$datatreey)		
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreey",  outputdatatreey_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreey_old()[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected'))){
				outputdatatreey_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreey_temp[[T[[2]][['package']]]][[T[[2]][['data']]]][[T[[2]][['col']]]],'stselected')=TRUE
				outputdatatreey_old(outputdatatreey_temp)
			}else{
				outputdatatreey_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreey_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
				outputdatatreey_old(outputdatatreey_temp)
			}			
			shinyTree::updateTree(session, "datatreey",  outputdatatreey_old() )
		}else if(length(T)==1){
			outputdatatreey_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
			attr(outputdatatreey_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
			outputdatatreey_old(outputdatatreey_temp)
		}else{
			outputdatatreey_old(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE))
		}
	} ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{outputdatatreey_old(NAlist())} ,ignoreNULL = FALSE)
	plottreey <- reactive(format_local(extract_local(inputdatatreey())))
    output$lbdatatreey <- renderText(paste("Y: ", {
      toStringB(extract_local(inputdatatreey()),ptdisabletreey())
    }))

   	#input COLOR
	output$datatreecolor <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old()),outputdatatreecolor_old()),ptdisabletreecolor()) )
	outputdatatreecolor_old <- reactiveVal(NAlist())
	inputdatatreecolor <- reactiveVal(NAlist())
	observeEvent(ptdisabletreecolor(),{inputdatatreecolor(filter_dis(outputdatatreecolor_old(),ptdisabletreecolor())) } ,ignoreNULL = FALSE)	
	observeEvent(outputdatatreecolor_old(),{inputdatatreecolor(filter_dis(outputdatatreecolor_old(),ptdisabletreecolor())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreecolor,{
		T <- extract_local(input$datatreecolor)		
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreecolor",  outputdatatreecolor_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreecolor_old()[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected'))){
				outputdatatreecolor_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreecolor_temp[[T[[2]][['package']]]][[T[[2]][['data']]]][[T[[2]][['col']]]],'stselected')=TRUE
				outputdatatreecolor_old(outputdatatreecolor_temp)
			}else{
				outputdatatreecolor_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreecolor_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
				outputdatatreecolor_old(outputdatatreecolor_temp)
			}			
			shinyTree::updateTree(session, "datatreecolor",  outputdatatreecolor_old() )
		}else if(length(T)==1){
			outputdatatreecolor_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
			attr(outputdatatreecolor_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
			outputdatatreecolor_old(outputdatatreecolor_temp)
		}else{
			outputdatatreecolor_old(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE))
		}
	} ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{outputdatatreecolor_old(NAlist())} ,ignoreNULL = FALSE)
	plottreecolor <- reactive(format_local(extract_local(inputdatatreecolor())))
    output$lbdatatreecolor <- renderText(paste("COLOR: ", {
      toStringB(extract_local(inputdatatreecolor()),ptdisabletreecolor())
    }))
	
	
   	#input FACET
	output$datatreefacet <- shinyTree::renderTree(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old()),outputdatatreefacet_old()),ptdisabletreefacet()) )
	outputdatatreefacet_old <- reactiveVal(NAlist())
	inputdatatreefacet <- reactiveVal(NAlist())
	observeEvent(ptdisabletreefacet(),{inputdatatreefacet(filter_dis(outputdatatreefacet_old(),ptdisabletreefacet())) } ,ignoreNULL = FALSE)	
	observeEvent(outputdatatreefacet_old(),{inputdatatreefacet(filter_dis(outputdatatreefacet_old(),ptdisabletreefacet())) } ,ignoreNULL = FALSE)	
	observeEvent(input$datatreefacet,{
		T <- extract_local(input$datatreefacet)		
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreefacet",  outputdatatreefacet_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreefacet_old()[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected'))){
				outputdatatreefacet_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreefacet_temp[[T[[2]][['package']]]][[T[[2]][['data']]]][[T[[2]][['col']]]],'stselected')=TRUE
				outputdatatreefacet_old(outputdatatreefacet_temp)
			}else{
				outputdatatreefacet_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(outputdatatreefacet_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
				outputdatatreefacet_old(outputdatatreefacet_temp)
			}			
			shinyTree::updateTree(session, "datatreefacet",  outputdatatreefacet_old() )
		}else if(length(T)==1){
			outputdatatreefacet_temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
			attr(outputdatatreefacet_temp[[T[[1]][['package']]]][[T[[1]][['data']]]][[T[[1]][['col']]]],'stselected')=TRUE
			outputdatatreefacet_old(outputdatatreefacet_temp)
		}else{
			outputdatatreefacet_old(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE))
		}
	} ,ignoreNULL = FALSE)
	observeEvent(plotdf(),{outputdatatreefacet_old(NAlist())} ,ignoreNULL = FALSE)
	plottreefacet <- reactive(format_local(extract_local(inputdatatreefacet())))
    output$lbdatatreefacet <- renderText(paste("FACET: ", {
      toStringB(extract_local(inputdatatreefacet()),ptdisabletreefacet())
    }))
	
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
	extract_local <- function(datatreez) {
	  resu <- list()
	  try(for (pkg in names(datatreez)) {
		for (dd in names(datatreez[[pkg]])) {
		  for (slc in names(datatreez[[pkg]][[dd]])) {
			try(if (attr(datatreez[[pkg]][[dd]][[slc]], "stselected")) {
			  resu <- append(resu, list(c(
				package = pkg,
				data = dd,
				col = slc,
				dt = attr(datasets()[[pkg]][[dd]][[slc]], "dt")
			  )))
			}, silent = TRUE)
		  }
		}
	  },silent = TRUE)
	  resu
	}
		

	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

   	#input PLOT TYPE
	output$datatreept <- shinyTree::renderTree(NApt(TRUE))
	outputdatatreept_old <- reactiveVal(NApt(TRUE))
	observeEvent(input$datatreept,{	
		T <- get_selected(input$datatreept, format = c("names"))				
		if(length(T)>2){
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreept_old()[[T[[1]]]],'stselected'))){
				outputdatatreept_temp <- NApt()
				attr(outputdatatreept_temp[[T[[2]]]],'stselected')=TRUE
				outputdatatreept_old(outputdatatreept_temp)
			}else{
				outputdatatreept_temp <- NApt()
				attr(outputdatatreept_temp[[T[[1]]]],'stselected')=TRUE
				outputdatatreept_old(outputdatatreept_temp)
			}			
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
		}else if(length(T)==1){
			outputdatatreept_temp <- NApt()
			attr(outputdatatreept_temp[[T[[1]]]],'stselected')=TRUE
			outputdatatreept_old(outputdatatreept_temp)
		}else{
			outputdatatreept_temp <- NApt()
			attr(outputdatatreept_temp[['auto']],'stselected')=TRUE
			outputdatatreept_old(outputdatatreept_temp)
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
			#outputdatatreept_old(NApt())
		}
	} ,ignoreNULL = FALSE)
	#observeEvent(outputdatatreedf_old(),{outputdatatreept_old(NApt())} ,ignoreNULL = FALSE)
    plottype <- reactive(  filter_pt(get_selected(outputdatatreept_old(), format = c("names")),extract_local(inputdatatreex()),extract_local(inputdatatreey())))
    output$lbdatatreept <- renderText(paste("PLOT TYPE: ", {
      toStringB(plottype())
    }))
	
	plotdf_ <- reactive(toString(plotdf()))
	plottreex_ <- reactive(toString(get_col(plottreex())))
	plottreey_ <- reactive(toString(get_col(plottreey())))
	plottype_ <- reactive(toString(pt_autofree(plottype())))
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

	
	ctrl7 <- function (matchtypes, tocheckbox){
		observeEvent(plottype_(),{
			if( is.element(plottype_(),matchtypes) ){
				updateCheckboxInput(session, tocheckbox, value = TRUE)
			}else{
				updateCheckboxInput(session, tocheckbox, value = FALSE)
			}
		})
	}
	
	ctrl7(c('histogram'),"geom_histogram")
	ctrl7(c('bar'),"geom_bar")
	ctrl7(c('box','box2'),"geom_boxplot")
	ctrl7(c('bar2','scatter'),"geom_point")
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
	observeEvent(input$datatreept,{
	    temp <- get_selected(outputdatatreept_old(), format = c("names"))
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
				ptdisabletreex(disablex)
				ptdisabletreey(disabley)
			}else{
				disablex=FALSE;disabley=FALSE				
				ptdisabletreex(disablex)
				ptdisabletreey(disabley)
			}
		}else{
				disablex=FALSE;disabley=FALSE				
				ptdisabletreex(disablex)
				ptdisabletreey(disabley)
		}
	
	} ,ignoreNULL = FALSE)
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
	parameters_list=list(
		"coord_flip"=list(),
		"scale_x_log10"=list(),
		"scale_y_log10"=list(),
		"theme"=list( "theme_fun"=structure("theme_fun",nme='theme_fun',tp="character",deflt="",fun=TRUE),
					  "theme_base_size"=structure("theme_base_size",nme='base_size',tp="numeric",deflt=12)
						  ),
		"ggtitle"=list( "ggtitle_label"=structure("ggtitle_label",nme='label',tp="character",deflt="")
						  ),
		"xlab"=list( "xlab_label"=structure("xlab_label",nme='label',tp="character",deflt="")
						  ),
		"ylab"=list( "ylab_label"=structure("ylab_label",nme='label',tp="character",deflt="")
						  ),					  
		"geom_violin"=list( "geom_violin_color"=structure("geom_violin_color",nme='size',tp="character",deflt=""),
							"geom_violin_fill"=structure("geom_violin_fill",nme='size',tp="character",deflt="")
						  ),					  
		"geom_point"=list( "geom_point_size"=structure("geom_point_size",nme='size',tp="numeric",deflt=2)
						  ),
		"geom_histogram"=list( "geom_histogram_bins"=structure("geom_histogram_bins",nme='bins',tp="numeric",deflt=20)
						  )
	
	)
	
	get_panel <- function (region_property, plus = TRUE){
		result <- "";
	    if(isTRUE(input[[region_property]]) ){
		  result <- paste0("  ",region_property,"(")
		  {
			      first_parameter <- TRUE	  
				  for (parameter in names(parameters_list[[region_property]]) ){
					user_input <- input[[parameter]]
					deflt_input <- attr(parameters_list[[region_property]][[parameter]], 'deflt')
					nme <- attr(parameters_list[[region_property]][[parameter]], 'nme')
					tp <- attr(parameters_list[[region_property]][[parameter]], 'tp')
					fun <- attr(parameters_list[[region_property]][[parameter]], 'fun')
					if (isTRUE(fun)){
						result <- paste0("  ",user_input,"(")
						first_parameter <- TRUE	
					}else if (isTRUE(user_input!=deflt_input)){
						if (!first_parameter){
							result <- paste0(result, ", ")
						}else{
							first_parameter <- FALSE
						}	
						result <- paste0(result, nme)
						result <- paste0(result, " = ")
						if (tp=='numeric'){
							result <- paste0(result, user_input)
						}else{
							result <- paste0(result, "\"",user_input, "\"")
						}	
					}	
				  }		  
		  
		  }
		  result <- paste0(result, ")")
		  if (isTRUE(plus)){
			result <- paste0(result, " + \n")
		  }else{
			result <- paste0(result, "   \n")
		  }
	    }
		result
	}

	get_color <- function (){
	  color <- toString(       (plottreecolor())) #color <- toString(get_col(plottreecolor()))
	  paste0(
		if(isTRUE(nchar(color)>0)){
			paste0(", color=",color)
		}else{""}
	  )	
	}
	
	get_facet <- function (){
	  facet <- toString(       (plottreefacet())) #facet <- toString(get_col(plottreefacet()))
	  paste0(
		if(isTRUE(nchar(facet)>0)){
			paste0("  facet_wrap(~",facet,") + \n")
		}else{""}
	  )	
	}
	
	get_stat_summary <- function (){
	  paste0(
	    if(isTRUE(input$stat_summary) ){
			paste0("  stat_summary(fun.data=function(x) data.frame( \n",
				   "    y=mean(x, na.rm=TRUE),                      \n",
				   "    ymin=mean(x, na.rm=TRUE)-sd(x,na.rm=TRUE),  \n",
				   "    ymax=mean(x, na.rm=TRUE)+sd(x,na.rm=TRUE)), \n",
				   "    color=\"red\") +                            \n")
		}else{""}
	  )	
	}
	
	get_customized_code <- function (){
	  if(isTRUE(nchar(input$customized_code)>0) ){
		paste0(toString(input$customized_code),'\n')
	  }else{""}
	}
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

    # RMarkdown Code
    txt_react <- reactive({
      txt <- paste0(
	    "```{r echo=FALSE} \n",
		"Sys.time()\n",
		"```\n",
	    "* PLOT: ",plottype_()," \n",
		"* DATA: ",plotdf_()," \n",
		if(!ptdisabletreex()){
			paste0("* X: ",toString(plottreex())," \n")
		}else{""},
		if(!ptdisabletreey()){
			paste0("* Y: ",toString(plottreey())," \n")
		}else{""},  
		if(isTRUE(nchar(plottreecolor())>0) & !ptdisabletreecolor()){
			paste0("* COLOR: ",toString(plottreecolor())," \n")
		}else{""},  
		if(isTRUE(nchar(plottreefacet())>0) & !ptdisabletreefacet()){
			paste0("* FACET: ",toString(plottreefacet())," \n")
		}else{""}, 
		if(isTRUE(nchar(input$ggtitle)>0)){
			paste0("* TITLE: ",toString(input$ggtitle)," \n")
		}else{""},
		if(isTRUE(nchar(input$xlab_label)>0)  ){
			paste0("* X LABEL: ",toString(input$xlab_label)," \n")
		}else{""},
		if(isTRUE(nchar(input$ylab_label)>0)  ){
			paste0("* Y LABEL: ",toString(input$ylab_label)," \n")
		}else{""},
		if(isTRUE(input$scale_x_log10) ){
			paste0("* X AXIS: scale_x_log10() \n")
		}else{""},
		if(isTRUE(input$scale_y_log10) ){
			paste0("* Y AXIS: scale_y_log10() \n")
		}else{""},
		if(isTRUE(input$geom_violin)  ){ # & !ptdisabletreey()
			paste0("*    ALSO: geom_violin( ) \n")
		}else{""},
		if(isTRUE(input$stat_summary)  ){ # & !ptdisabletreey()
			paste0("*    ALSO: stat_summary( ) \n")
		}else{""},
		if(isTRUE(input$coord_flip) ){
			paste0("*    ALSO: coord_flip() \n")
		}else{""},
		" \n",
		" \n",
		
	    "```{r}\n",
	    "library(ggplot2)\n",

		if (plottype_()=="histogram" & plottreex_()!="" ){paste0(
		"#(numeric x)                  "," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreex_(),get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (plottype_()=="bar" & plottreex_()!="" ){paste0(
		"#(categorical x)              "," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreex_(),get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (plottype_()=="box" & plottreey_()!="" ){paste0(
		"#(numeric y)                  "," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreey_(),", x = \"\"",get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (plottype_()=="bar2" & plottreey_()!="" ){paste0(
		"#(categorical y)              "," \n",
		"(                             "," \n",
		" ",plotdf_()," %>%                                  "," \n",
		"  dplyr::group_by(",plottreey_(),") %>%        "," \n",
		"  dplyr::summarise( count= dplyr::n()) %>%                   "," \n",
		"  ggplot(aes(x=count, y=",plottreey_(),get_color(),")) +   "," \n",
		"  geom_errorbarh(aes(xmax=count), xmin=0, height=0) +        "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()                                     "," \n"
		)}else{""},
		
		
		if (plottype_()=="scatter" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"#(numeric x and y)            "," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreex_(),",",plottreey_(),get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (plottype_()=="box2" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"#(categorical x numeric y)    "," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreex_(),",",plottreey_(),get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},
		
		
		if (plottype_()=="histogram2" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"#(numeric x categorical y)    "," \n",
		"#(                             "," \n",
		" ggplot(",plotdf_(),", aes(",plottreex_(),",",plottreey_(),get_color(),")) +  "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		#get_panel("theme",plus=FALSE),
		"  ggridges::stat_binline(bins = 50, scale = .7, draw_baseline = FALSE) +     "," \n",
		"  ggridges::theme_ridges()               "," \n",
		"#) %>% plotly::ggplotly()      "," \n"
		)}else{""},
		

		if (plottype_()=="grid" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"#(categorical x categorical y)"," \n",
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(x=seq_along(",plottreex_(),"),y=",plottreey_(),", fill=stat(count)",get_color(),")) +  "," \n",
		"  stat_bin2d() +              "," \n",
		"  scale_fill_gradient2() +     "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},		
		
		
		######################################################@@@@@@@@@@@@@@@@@@

		if (plottype_()=="line" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(x=seq_along(",plottreex_(),"),y=",plottreey_(),get_color(),")) +  "," \n",
		"  geom_line() +            "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
		)}else{""},

		if (plottype_()=="area" & plottreex_()!="" & plottreey_()!="" ){paste0(
		"(                             "," \n",
		" ggplot(",plotdf_(),", aes(x=seq_along(",plottreex_(),"),y=",plottreey_(),get_color(),",)) +  "," \n",
		"  geom_area() +            "," \n",
		get_panel("geom_point"),
		get_panel("geom_boxplot"),
		get_panel("geom_bar"),
		get_panel("geom_histogram"),
		get_customized_code(),
		get_panel("geom_violin"),
		get_stat_summary(),
		get_panel("coord_flip"),
		get_panel("ggtitle"),get_panel("xlab"),get_panel("ylab"),
		get_panel("scale_x_log10"),get_panel("scale_y_log10"),
		get_facet(),
		get_panel("theme",plus=FALSE),
		") %>% plotly::ggplotly()      "," \n"
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
  result <- NULL
  for (res in resu) {
	result <- append(result, list(
	  structure(
		paste0(
			ifelse(res[["package"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["package"]],"::") ),
			res[["data"]],"$",res[["col"]]
		)
	  ,package=ifelse(res[["package"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["package"]],"::") )
	  ,data=res[["data"]]
	  ,col=res[["col"]]
	  )
	))
  }
  result
}

format_local2 <- function(resu) {
  result <- NULL
  for (res in resu) {
	result <- append(result, list(c(
		paste0(
			ifelse(res[["package"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["package"]],"::") ),
			res[["data"]]
		)
	)))
  }
  result
}

toStringB <- function(resu,disable=FALSE) {
  zeromessage="Select...     "
  if(disable)zeromessage="Not Required"
  if (length(resu) == 0) {
    zeromessage
  } else if (toString(resu) == "") {
    zeromessage
  } else {
    toString(resu)
  }
}

NApt <- function(auto=FALSE) {
	structure(list(
	  'auto'=structure('auto',sticon=' fa fa-oil-can ',stselected=auto),
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

filter_df <- function (original, criterias, reference=NULL){
  Tree0s <- NULL
  for (pkg in names(original)) {
    for (dd in names(original[[pkg]])) {
      for (criteria in criterias){
        if(pkg==criteria[["package"]] & dd==criteria[["data"]] ){
          Tree0s[[pkg]][[dd]] <- original[[pkg]][[dd]]
          attr(Tree0s[[pkg]][[dd]], "stopened") <- TRUE
          attr(Tree0s[[pkg]], "sttype") <- "pkg-node"
          attr(Tree0s[[pkg]], "sticon") <- "fas fa-box"
          attr(Tree0s[[pkg]], "stopened") <- TRUE
          for (slc in names(original[[pkg]][[dd]])) {
            try({attr(Tree0s[[pkg]][[dd]][[slc]], "stselected") <- attr(reference[[pkg]][[dd]][[slc]], "stselected")}, silent=TRUE)
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
    result <- c(result, attr(res,"col") )
  }
  result
}

filter_dis <- function(resu, disabled=FALSE, fill=NAlist()) {
  if(disabled){
    fill
  }else{
    resu
  }
}

theme_choices <- function(){
  list("theme_grey", "theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void", "theme_test")
}