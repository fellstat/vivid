parameters_list=list(
  "ggplot"=structure(list(
    "ggplot_data"=structure("ggplot_data",nme='data',deflt="")
  ),ctrl8=c('auto','area','bar','bar2','box','box2','grid','histogram','histogram2','line','scatter')),
  "aes"=structure(list(
    "aes_x"=structure("aes_x",nme='x',deflt=""),
    "aes_y"=structure("aes_y",nme='y',deflt=""),
    "aes_color"=structure("aes_color",nme='color',deflt=""),
    "aes_fill"=structure("aes_fill",nme='fill',deflt=""),
	"aes_size"=structure("aes_size",nme='size',deflt=""),
	"aes_frame"=structure("aes_frame",nme='frame',deflt=""),
	"aes_ids"=structure("aes_ids",nme='ids',deflt=""),
	"aes_group"=structure("aes_group",nme='group',deflt=""),
	"aes_xmax"=structure("aes_xmax",nme='xmax',deflt="")
  ),ctrl8=c('auto','area','bar','bar2','box','box2','grid','histogram','histogram2','line','scatter')),
  "geom_errorbarh"=structure(list(
    "geom_errorbarh_mapping"=structure("geom_errorbarh_mapping",nme='mapping',deflt="aes(xmax=count)",alwyshow='show'),
    "geom_errorbarh_xmin"=structure("geom_errorbarh_xmin",nme='xmin',deflt="0",alwyshow='show'),
    "geom_errorbarh_height"=structure("geom_errorbarh_height",nme='height',deflt="0",alwyshow='show')
  ),ctrl8=c('bar2')),
  "stat_bin2d"=structure(list(),ctrl8=c('grid')),
  "geom_line"=structure(list(),ctrl8=c('line')),
  "geom_area"=structure(list(),ctrl8=c('area')),
  "scale_fill_gradient2"=structure(list(),ctrl8=c('grid')),	
  "geom_point"=structure(list("geom_point_size"=structure("geom_point_size",nme='size',deflt=2)
  ),ctrl8=c('bar2','scatter')),
  "geom_boxplot"=structure(list(),ctrl8=c('box','box2')),
  "geom_bar"=structure(list(),ctrl8=c('bar')),
  "geom_histogram"=structure(list("geom_histogram_bins"=structure("geom_histogram_bins",nme='bins',deflt=20,alwyshow='show')
  ),ctrl8=c('histogram')),
  "geom_violin"=structure(list("geom_violin_color"=structure("geom_violin_color",nme='color',tp="quote",deflt="white",alwyshow='show'),
                     "geom_violin_fill"=structure("geom_violin_fill",nme='color',tp="quote",deflt="grey90",alwyshow='show')
  )),	
  "stat_summary"=structure(list(
    "stat_summary_fun_data"=structure("stat_summary_fun_data",nme='fun.data',deflt="function(x)\n    data.frame( y=mean(x, na.rm=TRUE),\n    ymin=mean(x, na.rm=TRUE)-sd(x,na.rm=TRUE),\n    ymax=mean(x, na.rm=TRUE)+sd(x,na.rm=TRUE))\n    "),
    "stat_summary_color"=structure("stat_summary_color",nme='color',tp="quote",deflt="red")
  )),
  "coord_flip"=structure(list()),
  "ggtitle"=structure(list("ggtitle_label"=structure("ggtitle_label",nme='label',tp="quote",deflt="",alwyshow='show')
  )),
  "xlab"=structure(list("xlab_label"=structure("xlab_label",nme='label',tp="quote",deflt="",alwyshow='show')
  )),  
  "ylab"=structure(list("ylab_label"=structure("ylab_label",nme='label',tp="quote",deflt="",alwyshow='show')
  )),
  "scale_x_log10"=structure(list()),
  "scale_y_log10"=structure(list()),
  "facet_wrap"=structure(list(
    "facet_wrap_facets"=structure("facet_wrap_facets",nme='facets',deflt="")
  )),
  "stat_binline"=structure(list(
    "stat_binline_bins"=structure("ggridges::stat_binline_bins",nme='bins',deflt=50,alwyshow='show'),
    "stat_binline_scale"=structure("ggridges::stat_binline_scale",nme='scale',deflt=0.7,alwyshow='show'),
    "stat_binline_draw_baseline"=structure("ggridges::stat_binline_draw_baseline",nme='draw_baseline',deflt=FALSE,alwyshow='show')
  ), alt="ggridges::stat_binline",ctrl8=c('histogram2')),
  "theme"=structure(list( "theme_fun"=structure("theme_fun",nme='theme_fun',tp="quote",deflt="theme_bw",fun=TRUE),
                "theme_base_size"=structure("theme_base_size",nme='base_size',deflt=12)
  ),ctrl8=c('auto','area','bar','bar2','box','box2','grid','histogram','line','scatter')),
  "theme_ridges"=structure(list(),alt="ggridges::theme_ridges",ctrl8=c('histogram2'))
)


ctrlJS <- function (...){
  tags$i(
    shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
    shinyjs::inlineCSS(".fa-tag-integer { color: gold }"),
    shinyjs::inlineCSS(".fa-tag-numeric { color: darkorange }"),
    shinyjs::inlineCSS(".fa-tag-character { color: green }"),
	shinyjs::inlineCSS(".fa-tag-Date { color: red }"),
	shinyjs::inlineCSS(".fa-tag-ts { color: darkred }"),
	shinyjs::inlineCSS(".fa-tag-orderedfactor { color: darkorchid }"),
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
		tags$i(class = "fa fa-tag fa-tag-Date", "Date"),
		tags$i(class = "fa fa-tag fa-tag-ts", "ts"),
		tags$br(),
        tags$i(class = "fa fa-tag fa-tag-character", "character"),
	    tags$i(class = "fa fa-tag fa-tag-factor", "factor"),
		tags$i(class = "fa fa-tag fa-tag-orderedfactor", "orderedfactor")
      ),
	   { #if(is.element(ctrlname, c("datatreedf","datatreept","treex","treey"))){  
		 #   tags$i(tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.width='70%'")),
		 #   tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.position='relative'")),
		 #   tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.left='15%'")) )
	     #}else{
		   tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.width='100%'"))
	     #}
	   },       
	   tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.textAlign='left'")),
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

ctrlKD <- function (ns, ctrlname, ...){
  shiny::column(3,shinyWidgets::dropdown(
    ...,
    circle = FALSE,
    icon = uiOutput(ns(paste0("ic",ctrlname)), inline =TRUE), #icon("gear"),
    label = tags$i(toupper(stringr::str_remove(ctrlname, "-panel")), style='font-size: 9pt;'),
    inputId = ns(paste0("ii",ctrlname)),
    up = TRUE, 
    tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.width='100%'")),
	tags$script(paste0("document.getElementById('",ns(paste0("ii",ctrlname)),"').style.textAlign='left'")),
    tags$script(paste0("document.getElementById('",paste0("sw-content-",ns(paste0("ii",ctrlname))),"').style.maxHeight='300px'")),
    tags$script(paste0("document.getElementById('",paste0("sw-content-",ns(paste0("ii",ctrlname))),"').style.minWidth='300px'")),
    tags$script(paste0("document.getElementById('",paste0("sw-content-",ns(paste0("ii",ctrlname))),"').style.maxWidth='350px'")),
    tags$script(paste0("document.getElementById('",paste0("sw-content-",ns(paste0("ii",ctrlname))),"').style.overflow='auto'"))
  ))
}

CtrlK <- function(ns){
  AA=list();
  for (region_property in names(parameters_list)){
    BB=tags$div(ctrlKD(ns,paste0(region_property,"-panel"), {
      CC=list(tags$div(checkboxInput(ns(region_property), toupper(region_property))));
      for (parameter in names(parameters_list[[region_property]]) ){      
        deflt <- attr(parameters_list[[region_property]][[parameter]], 'deflt')
        nme <- attr(parameters_list[[region_property]][[parameter]], 'nme')
        tp <- attr(parameters_list[[region_property]][[parameter]], 'tp')
        fun <- attr(parameters_list[[region_property]][[parameter]], 'fun')
		alwyshow <- attr(parameters_list[[region_property]][[parameter]], 'alwyshow')
        DD=tags$div(paste0(nme,' ',tp,' ',deflt,' ',alwyshow), 
		            textInput(ns(parameter), toupper(parameter), value = deflt))
        CC=c(CC,list(tags$div(DD)))
      }
      tags$div(CC)
    }))
    AA=c(AA,list(BB))
  }
  AA
}


extract_local2 <- function(datatreex) {
  library(shinyTree)
  resu <- list()
  try(for (pkg in names(datatreex)) {
    for (dd in names(datatreex[[pkg]])) {
        try(if (attr(datatreex[[pkg]][[dd]], "stselected")) {
          resu <- append(resu, list(c(
            pkg = pkg,
            dat = dd
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
			ifelse(res[["pkg"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["pkg"]],"::") ),
			res[["dat"]],"$",res[["col"]]
		)
	  ,pkg=ifelse(res[["pkg"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["pkg"]],"::") )
	  ,dat=res[["dat"]]
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
			ifelse(res[["pkg"]]=='.GlobalEnv','.GlobalEnv$',paste0(res[["pkg"]],"::") ),
			res[["dat"]]
		)
	)))
  }
  result
}

toStringB <- function(resu, disable=FALSE){
  zeromessage <- "Select...     "
  if(disable)zeromessage <- "Not Required"
  if (length(resu) == 0) {
    zeromessage
  } else if (toString(resu) == "") {
    zeromessage
  } else {
    toString(resu)
  }
}

NApt <- function(auto=FALSE){
	structure(list(
	  'auto'=structure('auto',sticon=' fa fa-oil-can ',stselected=auto),
	  'area'=structure('area',sticon=' fa fa-area-chart ',stselected=FALSE),
	  'bar'=structure('bar',sticon=' fas fa-tachometer-alt ',stselected=FALSE),
	  'bar2'=structure('bar2',sticon=' fas fa-tachometer-alt fa-tag-numeric ',stselected=FALSE),
	  'box'=structure('box',sticon=' fas fa-inbox ',stselected=FALSE),
	  'box2'=structure('box2',sticon=' fas fa-inbox  fa-tag-numeric ',stselected=FALSE),
	  'grid'=structure('grid',sticon=' fas fa-car-battery ',stselected=FALSE),
	  'histogram'=structure('histogram',sticon=' fa fa-bar-chart ',stselected=FALSE),
	  'histogram2'=structure('histogram2',sticon=' fa fa-bar-chart  fa-tag-numeric ',stselected=FALSE),
	  'line'=structure('line',sticon=' fa fa-line-chart ',stselected=FALSE),
	  'scatter'=structure('scatter',sticon=' fas fa-braille ',stselected=FALSE)
	),stopened=TRUE)
}

filter_df <- function (original, criterias, reference=NULL){
  Tree0s <- NULL
  for (pkg in names(original)) {
    for (dd in names(original[[pkg]])) {
      for (criteria in criterias){
        if(pkg==criteria[["pkg"]] & dd==criteria[["dat"]] ){
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
      result <- c(result, 
		  if( judge_numeric(xxx) ){
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
      result <- c(result, 
		  if( judge_numeric(yyy) ){
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
        result<-c(result, 
			if( judge_numeric(xxx) & judge_numeric(yyy) ){
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
  #is.element(res["dt"],c('numeric','integer','Date','ts'))
  !judge_categorical(res)
}

judge_categorical <- function (res){
  is.element(res["dt"],c('character','factor','orderedfactor'))
}

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
} #result=sapply(c(resu), FUN=function(res){attr(res,"col")})

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

test_gizmo_dynamic_ui <- function(ns) {
  fluidPage(
    ctrlJS(),
	fluidRow(
		shiny::column(6,shiny::column(10,ctrlA(ns,"datatreedf"),offset = 1)),
		shiny::column(6,shiny::column(10,ctrlA(ns,"datatreept"),offset = 1))
	),tags$br(),
	fluidRow(
		shiny::column(6,shiny::column(10,ctrlA(ns,"treex"),offset = 1)),
		shiny::column(6,shiny::column(10,ctrlA(ns,"treey"),offset = 1))
	),tags$br(),
	fluidRow(
		shiny::column(3,ctrlA(ns,"treecolor")),
		shiny::column(3,ctrlA(ns,"treefill")),
		shiny::column(3,ctrlA(ns,"treesize"))		
	),tags$br(),
	fluidRow(  
		shiny::column(3,ctrlA(ns,"treefacet")),
		shiny::column(3,ctrlA(ns,"treeframe")),
		shiny::column(3,ctrlA(ns,"treeids"))
	),tags$br(),
	actionLink(ns("advancedmenu"), 'Advanced', icon = icon('caret-right'),	
		onclick=paste0("
							  var dm = document.getElementById('", ns(paste0("advancedmenu", 'div')) ,"');
							  if (dm.style.display === 'none') {
							    dm.style.display = 'block';
							  } else {
							    dm.style.display = 'none';
							  }
		")
	),
	tags$div(
	  fluidRow(
		CtrlK(ns)
	  ), 
	  id=ns(paste0("advancedmenu", 'div')), 
	  style="display: none;"	  
	),
    tags$br(),
	actionLink(ns("miscmenu"), 'Misc', icon = icon('caret-right'),	
		onclick=paste0("
							  var dm = document.getElementById('", ns(paste0("miscmenu", 'div')) ,"');
							  if (dm.style.display === 'none') {
							    dm.style.display = 'block';
							  } else {
							    dm.style.display = 'none';
							  }
		")
	),
	tags$div(
	  fluidRow(
		  ctrlKD(ns,"debug-panel",	        
				radioButtons(ns("plotlyoverlay"), label=NULL, choices = c("plotly", "ggplot"), selected = "plotly", inline=TRUE),
				textAreaInput(ns("customized_code"), "CUSTOMIZED CODE", width='100%')
				),
		  ctrlKD(ns,"reload-panel",	        
				actionButton(ns("reloaddatasets"), "Reload datasets"),
				uiOutput(ns(paste0("ic","reloaddatasets")), inline =TRUE) #icon("gear"),
				)				
	  ), 
	  id=ns(paste0("miscmenu", 'div')), 
	  style="display: none;"	  
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
	
	iccreloaddatasets  <- reactiveVal('time')

    shinyjs::click("reloaddatasets")
	
	observeEvent(input$reloaddatasets, {
	    iccreloaddatasets('time')
		remote_eval(vivid:::texasCi(), function(obj) {
		  message("data retrived")
		  datasets(obj$Tree0s)
		  datadfs (obj$Traa0s)
		  iccreloaddatasets('check')
		})
	})
	
	output[[paste0('ic','reloaddatasets')]]<-renderPrint(tags$i(icon(iccreloaddatasets()), style = "color:black"))

	#input DATA FRAME
	output$datatreedf <- shinyTree::renderTree(datadfs())
	outputOptions(output, "datatreedf", suspendWhenHidden = FALSE)
	outputdatatreedf_old <- reactiveVal(datadfs())  #single selection ctrl
	observeEvent(input$datatreedf,{   #single selection
		T <- extract_local2(input$datatreedf)
		T2 <- extract_local2(outputdatatreedf_old())
		if(length(T2)>1){
			outputdatatreedf_temp <- datadfs()
			outputdatatreedf_old(outputdatatreedf_temp)
			shinyTree::updateTree(session, "datatreedf",  outputdatatreedf_old() )
		}else if(length(T)>2){
			shinyTree::updateTree(session, "datatreedf",  outputdatatreedf_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreedf_old()[[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]],"stselected"))){
				outputdatatreedf_temp <- datadfs()
				attr(outputdatatreedf_temp[[T[[2]][["pkg"]]]][[T[[2]][["dat"]]]],"stselected")=TRUE
			}else{
				outputdatatreedf_temp <- datadfs()
				attr(outputdatatreedf_temp[[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]],"stselected")=TRUE				
			}
			outputdatatreedf_old(outputdatatreedf_temp)			
			shinyTree::updateTree(session, "datatreedf",  outputdatatreedf_old() )
		}else if(length(T)==1){
			outputdatatreedf_temp <- datadfs()
			attr(outputdatatreedf_temp[[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]],"stselected")=TRUE
			outputdatatreedf_old(outputdatatreedf_temp)
		}else{
			outputdatatreedf_old(datadfs())
		}		
	})
	observeEvent(datadfs(),{outputdatatreedf_old(NAlist())} ,ignoreNULL = FALSE)
	output$lbdatatreedf <- renderText(paste("DATA FRAME: ", {
      toStringB(extract_local2(outputdatatreedf_old()))
    }))
	plotdf <- reactive( format_local2(extract_local2(outputdatatreedf_old())) )
	plotdf_ <- reactive(toString(plotdf()))
	
	
   	#input PLOT TYPE
	output$datatreept <- shinyTree::renderTree(NApt(TRUE))
	outputOptions(output, "datatreept", suspendWhenHidden = FALSE)
	outputdatatreept_old <- reactiveVal(NApt(TRUE))
	observeEvent(input$datatreept,{	
		T <- shinyTree::get_selected(input$datatreept, format = c("names"))	
        T2 <- shinyTree::get_selected(outputdatatreept_old(), format = c("names"))		
		if(length(T2)>1){
			outputdatatreept_temp <- NApt()
			attr(outputdatatreept_temp[['auto']],"stselected")=TRUE
			outputdatatreept_old(outputdatatreept_temp)
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
		}else if(length(T)>2){
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
		}else if(length(T)==2){
		    if(isTRUE(attr(outputdatatreept_old()[[T[[1]]]],"stselected"))){
				outputdatatreept_temp <- NApt()
				attr(outputdatatreept_temp[[T[[2]]]],"stselected")=TRUE				
			}else{
				outputdatatreept_temp <- NApt()
				attr(outputdatatreept_temp[[T[[1]]]],"stselected")=TRUE
			}	
			outputdatatreept_old(outputdatatreept_temp)
			shinyTree::updateTree(session, "datatreept",  outputdatatreept_old() )
		}else if(length(T)==1){
			outputdatatreept_temp <- NApt()
			attr(outputdatatreept_temp[[T[[1]]]],"stselected")=TRUE
			outputdatatreept_old(outputdatatreept_temp)
		}else{
			outputdatatreept_temp <- NApt()
			attr(outputdatatreept_temp[['auto']],"stselected")=TRUE
			outputdatatreept_old(outputdatatreept_temp)
		}
	} ,ignoreNULL = FALSE)   
    output$lbdatatreept <- renderText(paste("PLOT TYPE: ", {
      toStringB(plottype())
    })) 
	plottype <- reactive(  filter_pt(shinyTree::get_selected(outputdatatreept_old(), format = c("names")),extract_local(inputdata[['treex']]),extract_local(inputdata[['treey']])))
	plottype_ <- reactive(toString(pt_autofree(plottype())))
	

	#input STANDARD SELECTION
	ptdisable <- reactiveValues()
	output_old <- reactiveValues()
	inputdata <- reactiveValues()
	plot <- reactiveValues()   #raw format
	plot_ <- reactiveValues()  #complete string
	plot__ <- reactiveValues() #col name only
	
	CtrlN <- function (MEXICO){
		ptdisable[[MEXICO]] <- FALSE
		output_old[[MEXICO]] <- NAlist()
		inputdata[[MEXICO]] <- NAlist()
		output[[MEXICO]] <- shinyTree::renderTree({output_old[[MEXICO]]<-filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old()),output_old[[MEXICO]]),ptdisable[[MEXICO]]);output_old[[MEXICO]]} )
		observeEvent(ptdisable[[MEXICO]],{inputdata[[MEXICO]]<-(filter_dis(output_old[[MEXICO]],ptdisable[[MEXICO]])) } ,ignoreNULL = FALSE)	
		observeEvent(output_old[[MEXICO]],{inputdata[[MEXICO]]<-(filter_dis(output_old[[MEXICO]],ptdisable[[MEXICO]])) } ,ignoreNULL = FALSE)	
		observeEvent(input[[MEXICO]],{
			T <- extract_local(input[[MEXICO]])
            T2 <- extract_local(output_old[[MEXICO]])
			if(length(T2)>1){
				temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				output_old[[MEXICO]]<-(temp)
				shinyTree::updateTree(session, MEXICO, output_old[[MEXICO]] )
			}else if(length(T)>2){
				shinyTree::updateTree(session, MEXICO, output_old[[MEXICO]] )
			}else if(length(T)==2){
				if(isTRUE(attr(output_old[[MEXICO]][[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]][[T[[1]][["col"]]]],"stselected"))){
					temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
					attr(temp[[T[[2]][["pkg"]]]][[T[[2]][["dat"]]]][[T[[2]][["col"]]]],"stselected")=TRUE					
				}else{
					temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
					attr(temp[[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]][[T[[1]][["col"]]]],"stselected")=TRUE
				}	
				output_old[[MEXICO]]<-(temp)
				shinyTree::updateTree(session, MEXICO, output_old[[MEXICO]] )
			}else if(length(T)==1){
				temp <- filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE)
				attr(temp[[T[[1]][["pkg"]]]][[T[[1]][["dat"]]]][[T[[1]][["col"]]]],"stselected")=TRUE
				output_old[[MEXICO]]<-(temp)
			}else{
				output_old[[MEXICO]]<-(filter_dis(filter_df(datasets(), extract_local2(outputdatatreedf_old())),FALSE))
			}
		} ,ignoreNULL = FALSE)
		observeEvent(plotdf(),{output_old[[MEXICO]]<-(NAlist())} ,ignoreNULL = FALSE)		
		output[[paste0('lb',MEXICO)]] <- renderText(paste0(toupper(if(substr(MEXICO,1,4)=="tree"){substr(MEXICO,5,1000)}else{MEXICO}), ": ", {
		  toStringB(extract_local(inputdata[[MEXICO]]),ptdisable[[MEXICO]])
		}))
		plot[[MEXICO]] <- reactive(format_local(extract_local(inputdata[[MEXICO]])))
		plot_[[MEXICO]] <- reactive(toString(format_local(extract_local(inputdata[[MEXICO]]))))
		plot__[[MEXICO]] <- reactive(toString(get_col(format_local(extract_local(inputdata[[MEXICO]])))))
	}
	for ( MEXICO in c( "treex", "treey", "treecolor", "treefacet", "treefill", "treesize", "treeframe", "treeids") ){
		CtrlN(MEXICO)
	}
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
	extract_local <- function(datatreez) {
	  resu <- list()
	  try(for (pkg in names(datatreez)) {
		for (dd in names(datatreez[[pkg]])) {
		  for (slc in names(datatreez[[pkg]][[dd]])) {
			try(if (attr(datatreez[[pkg]][[dd]][[slc]], "stselected")) {
			  resu <- append(resu, list(c(
				pkg = pkg,
				dat = dd,
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
	
	observeEvent(c(plotdf_(),plottype_(),plot__[['treey']]()), { 
	    if(plottype_()=="bar2"){
		    updateTextInput(session, "ggplot_data", value = paste0(plotdf_(), " %>% dplyr::group_by(",plot__[['treey']](),") %>% dplyr::summarise(count= dplyr::n())" ) )
		}else{
			updateTextInput(session, "ggplot_data", value = plotdf_() ) 
		}	
	} ,ignoreNULL = FALSE)
	
	observeEvent(c(plottype_(),plot__[['treefill']]()), { 
		if(plottype_()=="grid"){
			updateTextInput(session, "aes_fill", value = "stat(count)" )
		}else{
			updateTextInput(session, "aes_fill", value = plot__[['treefill']]() )
		}
	} ,ignoreNULL = FALSE)
	
	observeEvent(plot__[['treesize']](), { 
		updateTextInput(session, "aes_size", value = plot__[['treesize']]() )
	} ,ignoreNULL = FALSE)
	
	observeEvent(plot__[['treeframe']](), { 
		updateTextInput(session, "aes_frame", value = plot__[['treeframe']]() )
	} ,ignoreNULL = FALSE)
	
	observeEvent(plot__[['treeids']](), { 
		updateTextInput(session, "aes_ids", value = plot__[['treeids']]() )
	} ,ignoreNULL = FALSE)
	
    observeEvent(c(plot__[['treex']](),plottype_()), { 
		if(plottype_()=="box"){
			updateTextInput(session, "aes_x", value = "\"\"" )
		}else if(plottype_()=="grid"){
			updateTextInput(session, "aes_x", value = paste0("seq_along(",plot__[['treex']](),")") )
		}else if(plottype_()=="line"){
			updateTextInput(session, "aes_x", value = paste0("seq_along(",plot__[['treex']](),")") )
		}else if(plottype_()=="area"){
			updateTextInput(session, "aes_x", value = paste0("seq_along(",plot__[['treex']](),")") )
		}else if(plottype_()=="bar2"){
			updateTextInput(session, "aes_x", value = "count" )
		}else{
			updateTextInput(session, "aes_x", value = plot__[['treex']]() )
		}	
	} ,ignoreNULL = FALSE)
	
	observeEvent(plot__[['treey']](),{ updateTextInput(session, "aes_y", value = plot__[['treey']]() ) } ,ignoreNULL = FALSE)
	
	observeEvent(plot_[["treecolor"]](),{ updateTextInput(session, "aes_color", value = plot_[["treecolor"]]() );} ,ignoreNULL = FALSE)	
	
	observeEvent(plot_[['treefacet']](),{ 	
		updateTextInput(session, "facet_wrap_facets", value = plot_[['treefacet']]() ) 
		if(nchar(plot_[['treefacet']]())>0){
		  updateCheckboxInput(session,"facet_wrap", value=TRUE)
		}else{
		  updateCheckboxInput(session,"facet_wrap", value=FALSE)
		}
	
	} ,ignoreNULL = FALSE)
	
	observeEvent(plottype_(), {
	  if(plottype_()=="histogram2"){
		  updatePickerInput(session, "plotlyoverlay", selected = "ggplot")
	  }else{
		  updatePickerInput(session, "plotlyoverlay", selected = "plotly")
	  }	
	},ignoreNULL = FALSE)
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

	ctrl3 <- function (tocheckbox){
		output[[paste0('ic',tocheckbox, '-panel')]]<-renderPrint({	
			apick <- FALSE
			ctrl8=attr(parameters_list[[tocheckbox]], 'ctrl8')
			if(!is.null(ctrl8)){
			  if(isTRUE(nchar(plottype_())>0)){
				  if( is.element(plottype_(),c(ctrl8)) ){
					apick<-TRUE
				  }
			  }
			}
			if (isTRUE(input[[tocheckbox]]) & apick){
				tags$i(icon('check'), style = "color:red")
			}else if (isTRUE(input[[tocheckbox]])){
				tags$i(icon('check'), style = "color:orange")
			}else{
				tags$i(icon('gear'), style = "color:lightgray")
			}	
		})
		outputOptions(output, paste0('ic',tocheckbox, '-panel'), suspendWhenHidden = FALSE)
	}
	
	lapply(
	  X = names(parameters_list),
	  FUN = function(region_property){
		  ctrl3(region_property)
	  }
	)
	
	ctrl7 <- function (matchtypes, tocheckbox){
	  observeEvent(plottype_(),{
		local({
		if( is.element(plottype_(),c(matchtypes)) ){
		  updateCheckboxInput(session, tocheckbox, value = TRUE)
		}else{
		  updateCheckboxInput(session, tocheckbox, value = FALSE)
		}
	  })
	  })
	}
	
	lapply(
	  X = names(parameters_list),
	  FUN = function(region_property){
		ctrl8=attr(parameters_list[[region_property]], 'ctrl8')
		if(!is.null(ctrl8)){
		  ctrl7(ctrl8,region_property)
		}
	  }
	)

	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#
	
	observeEvent(input$datatreept,{
	    temp <- shinyTree::get_selected(outputdatatreept_old(), format = c("names"))
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
				ptdisable[['treex']]=(disablex)
				ptdisable[['treey']]=(disabley)
			}else{
				disablex=FALSE;disabley=FALSE				
				ptdisable[['treex']]=(disablex)
				ptdisable[['treey']]=(disabley)
			}
		}else{
				disablex=FALSE;disabley=FALSE				
				ptdisable[['treex']]=(disablex)
				ptdisable[['treey']]=(disabley)
		}
	} ,ignoreNULL = FALSE)
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

	get_panels <- function (){
	  result <- ""
	  first_parameter <- TRUE
	  for (region_property in names(parameters_list) ){
		panel <- get_panel(region_property)
		if(isTRUE(nchar(panel)>0) ){
		    if(!first_parameter){
				result <- paste0(result, " + \n")
			}else{
				first_parameter=FALSE
			}
			result <- paste0(result, panel)
		}
	  }	
	  result <- paste0(result, "  \n")
	} 

	get_panel <- function (region_property){
	  result <- "";
	  if(isTRUE(input[[region_property]]) ){
		alt <- attr(parameters_list[[region_property]],"alt")
		if(is.null( alt )){
		  result <- paste0(region_property,"(")
		}else{
		  result <- paste0(alt,"(")
		}		
		{
		  first_parameter <- TRUE	  
		  for (parameter in names(parameters_list[[region_property]]) ){
			user_input <- input[[parameter]]
			deflt <- attr(parameters_list[[region_property]][[parameter]], 'deflt')
			nme <- attr(parameters_list[[region_property]][[parameter]], 'nme')
			tp <- attr(parameters_list[[region_property]][[parameter]], 'tp')
			fun <- attr(parameters_list[[region_property]][[parameter]], 'fun')
			alwyshow <- attr(parameters_list[[region_property]][[parameter]], 'alwyshow')
			if (isTRUE(fun)){
			  result <- paste0(user_input,"(")
			  first_parameter <- TRUE	
			}else if (isTRUE(user_input!=deflt)|isTRUE(alwyshow=='show')){
			  if (!first_parameter){
				result <- paste0(result, ", ")
			  }else{
				first_parameter <- FALSE
			  }	
			  result <- paste0(result, nme)
			  result <- paste0(result, " = ")
			  if (isTRUE(tp=='quote')){				
				result <- paste0(result, "\"",user_input, "\"")
			  }else{
				result <- paste0(result, user_input)
			  }	
			}	
		  }		  
		}
		result <- paste0("  ",result, ")")
	  }
	  result
	}	
	
	get_time <- function(){
	  paste0(		
		"```{r echo=FALSE} \n",
		"Sys.time()\n",
		"```\n"
	  )	
	}
	
	get_customized_code <- function (){
	  if(isTRUE(nchar(input$customized_code)>0) ){
		paste0(toString(input$customized_code),'\n')
	  }else{""}
	}
	
	what_auto_means <- function(){
	  paste0(
		if (plottype_()=="histogram" & plot__[['treex']]()!="" ){paste0(
		 "# hint: (numeric x) \n"
		)}else{""},		
		if (plottype_()=="bar" & plot__[['treex']]()!="" ){paste0(
		 "# hint: (categorical x) \n"
		)}else{""},		
		if (plottype_()=="box" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (numeric y) \n"
		)}else{""},		
		if (plottype_()=="bar2" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (categorical y) \n"
		)}else{""},		
		if (plottype_()=="scatter" & plot__[['treex']]()!="" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (numeric x and y) \n"
		)}else{""},		
		if (plottype_()=="box2" & plot__[['treex']]()!="" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (categorical x numeric y) \n"
		)}else{""},			
		if (plottype_()=="histogram2" & plot__[['treex']]()!="" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (numeric x categorical y) \n"
		)}else{""},			
		if (plottype_()=="grid" & plot__[['treex']]()!="" & plot__[['treey']]()!="" ){paste0(
		 "# hint: (categorical x categorical y) \n"
		)}else{""}		
	  )
	}
	
	#-------LOGICAL SEPERATION-------------------------------------------------------------------#

    # RMarkdown Code
	txt_react <- reactive({
	  txt <- paste0(
		get_time(),
		"* PLOT: ",plottype_()," \n",
		"* DATA: ",plotdf_()," \n",
		if(!ptdisable[['treex']]){
		  paste0("* X: ",toString(plot[['treex']]())," \n")
		}else{""},
		if(!ptdisable[['treey']]){
		  paste0("* Y: ",toString(plot[['treey']]())," \n")
		}else{""},  		
		if(isTRUE(nchar(plot[["treecolor"]]())>0) & !ptdisable[["treecolor"]]){
		  paste0("* COLOR: ",toString(plot[["treecolor"]]())," \n")
		}else{""},  
		if(isTRUE(nchar(plot[['treefacet']]())>0) & !ptdisable[['treefacet']]){
		  paste0("* FACET: ",toString(plot[['treefacet']]())," \n")
		}else{""}, 
		if(isTRUE(nchar(input$ggtitle_label)>0)){
		  paste0("* TITLE: ",toString(input$ggtitle_label)," \n")
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
		if(isTRUE(input$coord_flip) ){
		  paste0("*    ALSO: coord_flip() \n")
		}else{""},
		" \n",			
		if({nchar(plottype_())>0} & isTRUE(plottype_()!='auto')){paste0(
			"```{r} \n",
			what_auto_means(),
			"library(ggplot2) \n",		
			if(toString(input$plotlyoverlay)=="plotly"){"( \n"}else{""},
			get_panels(),
			get_customized_code(),
			if(toString(input$plotlyoverlay)=="plotly"){") %>% plotly::ggplotly()\n"}else{""},
			"```\n"
		)}else{""},		
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
