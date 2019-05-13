library(shinyTree)
test_gizmo_esquisse_ui <- function(ns){
library(shinyTree)
	fluidPage(
	    shinyjs::useShinyjs(),
	    shinyjs::inlineCSS("html, body  { overflow: auto;  } "),
		shinyjs::inlineCSS(".sw-dropdown { position: relative; display: inline-block;  } "),
		fluidRow(
		 #shiny::tags$script(src = 'shinyTree/jsTree-3.3.7/jstree.min.js'),
         #shiny::tags$script(src = 'shinyTree/shinyTree.js'),
		
		column(6,textInput(ns("esquisse0"), "input", value="Ready.") ),
		column(6,textInput(ns("esquisse1"), "Debug Only", value=ns("")) ),
		column(12,verbatimTextOutput(ns("esquisse_DS")) )
		),
		 
		
		
		tags$h1("Esquisse: ggplot2 Builder"),
		  #shinyTree::shinyTree(ns("tree2"), checkbox = TRUE),
		  fluidRow(
		      #column(12, uiOutput(ns("shinyTreeTest"))),
			  #column(12, dropdown( tags$h3("List of Input"),
			#	  br(),
			#	  tags$h3("List of Input2"),
			#	  style = "default", label = "Location", width = "300px", circle=FALSE
			 # )),
		       column(12,
			  #pickerInput(inputId = ns("input_data"),
                         # label = "Input Dataset:",
                         # choices = c("a"),
						 # #choicesOpt = c("a"),
                         # multiple=FALSE,
                         # options = list(`actions-box` = TRUE,
                                        # `live-search`=TRUE,
                                        # `none-selected-text`="Choose Data"))	
				column(4,
				 dropdownButton(
					#textInput(ns("datfilter"),NULL, value="", placeholder = "Filter"),
					shinyTree::shinyTree(ns("dattree"), checkbox = TRUE, search=TRUE),			
					#, 
					circle = FALSE, 
					status = "default", 
					icon = icon("gear"),  
					label = "Data Selection", 
					#width = "300px",
					inputId=ns("ii"),
					tooltip = tooltipOptions(title = "Click to see inputs !") 
				)											
										
										
			  ),
			  column(4,radioButtons(
				inputId = ns("data"),
				label = "Sample data to use:",
				choices = c("iris", "mtcars"),
				inline = TRUE
			  )),
			  column(3,radioButtons(
				inputId = ns("ggplot2orplotly"),
				label = "Output format:",
				choices = c("ggplot2", "plotly"),
				inline = TRUE
			  ))
		  ),		  
		  fluidPage(tags$div(
			style = "height: 700px; ",
			esquisse::esquisserUI(
			  id = ns("esquisse"),
			  header = FALSE,
			  choose_data = TRUE
			))
		  ),
		  shinyjs::inlineCSS(".sw-dropdown { position: relative; display: inline-block;  } "),
		  shinyjs::inlineCSS("html, body  { overflow: auto;   } ")
		  
		),tags$br(),tags$br(),tags$br(),tags$br(),
		  shinyjs::delay(500,shinyjs::runjs(paste0("shinyTree.initSearch('",ns(""),"dattree','",ns(""),"dattree-search-input', 250);"))),
		  #message(paste0("$(\"#",ns(""),"dattree\").jstree('open_all')")),
		  #shinyjs::delay(500,shinyjs::runjs(paste0("$(\"#",ns(""),"dattree\").jstree('open_all')"))),
		  shinyjs::delay(500,shinyjs::runjs(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.maxHeight='250px'"))),
		  shinyjs::delay(500,shinyjs::runjs(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.minWidth='250px'"))),
		  shinyjs::delay(500,shinyjs::runjs(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.overflow='auto'")))
	)
}

test_gizmo_esquisse_server <- function(input, output, session, state=NULL){
  affix <- gen_uuid()
  #data_r <- reactiveValues(data = ToothGrowth, name = "ToothGrowth")
  data_r <- reactiveValues(data = iris, name = "iris")
  tempcode=isolate(state$esquisse_code)
###test region

#message(paste0("shinyTree.initSearch('",input$esquisse1,"dattree','",input$esquisse1,"dattree-search-input', 250);"))
#shinyjs::delay(1000,shinyjs::runjs(paste0("shinyTree.initSearch('",input$esquisse1,"dattree','",input$esquisse1,"dattree-search-input', 250);")))
	
  #output$shinyTreeTest <- renderUI({ 
  #  
  #})
  
  
  output$esquisse_DS <- renderText({
    if (is.null(input$dattree)){
      "None"
    }else{
      paste0("dattree: ",toString(get_selected(input$dattree, format="names")))
    }
  }) 
  
  observeEvent(input$dattree, {
    library(ggplot2)
    resultt=c()
	typee=c()
    if (is.null(input$dattree)){
      "None"
    }else{
      message(paste0("dattree: ",toString(get_selected(input$dattree, format="names"))))
	  selection=get_selected(input$dattree, format="names")
	  for (cc in 1:length(selection)) {
	     message(selection[cc])		
	     datsm=eval(parse(text=toString(selection[cc])))
		 #browser()
		 
         if(is.vector(datsm)){
		    typee=c(typee,is.numeric(datsm))
			#browser()
			resultt=cbind(resultt,datsm)
			
			colnames(resultt)[ncol(resultt)] <- toString(selection[cc])
		 }
	  }
	  View(data.frame(resultt))
	  resulttA=data.frame(resultt)
	  
	  for(cc in 1:ncol(resulttA)){
		if (typee[cc]){  resulttA[cc]=as.numeric(unlist(resulttA[cc]) ) }
	  }
	  
	   #browser()
	  data_r$name <- 'dataSelection'
	  data_r$data <- resulttA
	  resultt
    }
  })
  
  
  output$dattree <- shinyTree::renderTree({ 
#message(paste0("shinyTree.initSearch('",input$esquisse1,"dattree','",input$esquisse1,"dattree-search-input', 250);"))
#shinyjs::delay(1000,shinyjs::runjs(paste0("shinyTree.initSearch('",input$esquisse1,"dattree','",input$esquisse1,"dattree-search-input', 250);")))
  library(shinyTree)
		  #shinyjs::delay(100,shinyjs::runjs(paste0("$(\"#",input$esquisse1,"dattree\").jstree('open_all')")))
      #dfs <- search_obj(what = "data.frame")
	  #if (is.null(dfs)) { }
		dfs <- data(package = "ggplot2", envir = environment())$results[, "Item"]	  

    list(  '.GlobalEnv'= list( 
      'iris' =  structure(list('Sepal.Length'='1', 'Sepal.Width'='2','Petal.Length'='3'),stselected=TRUE),  
      'mtcars' =  structure(list('mpg'='4','cyl'='5'), stselected=TRUE))) 
	  
		dfs <- data(package = "ggplot2", envir = environment())$results[, "Item"]	  
		temp=list()
		for (cc in 1:length(dfs) ) {
		  temp1=names(get_df(dfs[cc]))
		  temp2=list()
		  for (dd in 1:length(temp1) ) {
			ee=temp1[dd]#c(paste0(dfs[cc],"$",temp1[dd]))
			names(ee)=paste0(dfs[cc],"$",temp1[dd])
			temp2=append(temp2, ee)			
		  }
		  if(dfs[cc]=='msleep'){attr(temp2,"stselected")=TRUE;}
		  temp3=list(temp2)
		  names(temp3)=dfs[cc]
		  attr(temp3,"stopened")=FALSE
		  temp=c(temp,temp3)
		}
		temp 
  })
  output$tree2 <- shinyTree::renderTree({ 
    library(shinyTree)
    list(  'I lorem impsum'= list( 
      'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
      'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE)))

  })
  #browser()



  datasets <- reactiveVal(c())
  remote_eval(vivid:::.get_data()$objects, function(obj){
    names(obj) <- obj
    print(obj)
    datasets(obj)
    session$onFlushed(function(){
    updatePickerInput(session, inputId = "input_data", 
	choices = c("iris","mtcars",obj)#,
	#choicesOpt = c("iris","mtcars",obj)
	)
    })
  })
  observe({
    updatePickerInput(session, inputId = "input_data", 
	choices = c("iris","mtcars",datasets())#,
	#choicesOpt = c("iris","mtcars",datasets())
	
	)
  })




  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
    updateTextInput(session, "esquisse0", value=isolate(state$esquisse0))

    #browser()
	  
	# restore for esquisserUI
	  #ns("geom")
	##esquisse::updateDragulaInput(session, "esquisse-dragvars", choices= isolate(state$esquisse_input$`esquisse-dragvars`)) 
	# restore for controls_labs
    updateTextInput(session,"esquisse-controls-title", value= isolate(state$esquisse_input$`esquisse-controls-title`)) 
    updateTextInput(session,"esquisse-controls-subtitle", value= isolate(state$esquisse_input$`esquisse-controls-subtitle`))  
    updateTextInput(session,"esquisse-controls-caption", value= isolate(state$esquisse_input$`esquisse-controls-caption`))  
    updateTextInput(session,"esquisse-controls-x", value= isolate(state$esquisse_input$`esquisse-controls-x`))  
    updateTextInput(session,"esquisse-controls-y", value= isolate(state$esquisse_input$`esquisse-controls-y`)) 
	# restore for controls_params
    updateMaterialSwitch(session, "esquisse-controls-smooth_add", value=isolate(state$esquisse_input$`esquisse-controls-smooth_add`))
	updateSliderInput(session,"esquisse-controls-smooth_span", value=isolate(state$esquisse_input$`esquisse-controls-smooth_span`))
    updateSliderInput(session,"esquisse-controls-size", value=isolate(state$esquisse_input$`esquisse-controls-size`)) 	
    updateSliderInput(session,"esquisse-controls-bins", value=isolate(state$esquisse_input$`esquisse-controls-bins`)) 
    updateSliderInput(session,"esquisse-controls-scale", value=isolate(state$esquisse_input$`esquisse-controls-scale`)) 		
    updateSliderInput(session,"esquisse-controls-adjust", value=isolate(state$esquisse_input$`esquisse-controls-adjust`)) 
    updatePrettyRadioButtons(session,"esquisse-controls-position", selected=isolate(state$esquisse_input$`esquisse-controls-position`)) 
	updateMaterialSwitch(session,"esquisse-controls-flip", value=isolate(state$esquisse_input$`esquisse-controls-flip`))
	# restore for controls_appearance
    updateSpectrumInput(session,"esquisse-controls-fill_color", selected=isolate(state$esquisse_input$`esquisse-controls-fill_color`)) 	
	updatePickerInput(session,"esquisse-controls-palette", selected=isolate(state$esquisse_input$`esquisse-controls-palette`)) 
	updatePickerInput(session,"esquisse-controls-theme", selected=isolate(state$esquisse_input$`esquisse-controls-theme`)) 
	updateRadioGroupButtons(session,"esquisse-controls-legend_position", selected=isolate(state$esquisse_input$`esquisse-controls-legend_position`)) 
	#restore for controls_code
	  #"esquisse-controls-export_png"                                   
      #"esquisse-controls-export_ppt" 
 
	# restore main
	# targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet")	
	# for (cc in 1:length(targetsIds)) {
	    # tempvar=isolate(state$esquisse_input$`esquisse-dragvars`$target[cc])
		# compvar=isolate(state$esquisse_input$`esquisse-dragvars`$source)
		# if (tempvar %in% compvar) {
			# message(paste0('document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("xyvar")[',cc,'-1].innerHTML+=("','<span class=\'label-dragula\' id=\'dragvars-target-label-',clean_string(tempvar),'\' data-value=\'',tempvar,'\'> <span class=\'label label-continue badge-dad\'>',tempvar,'</span> </span>','") '))
			# #browser()
			# shinyjs::runjs(paste0('document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("xyvar")[',cc,'-1].innerHTML+=("','<span class=\'label-dragula\' id=\'dragvars-target-label-',clean_string(tempvar),'\' data-value=\'',tempvar,'\'> <span class=\'label label-continue badge-dad\'>',tempvar,'</span> </span>','") '))
			# #browser()
		# }
	# }
	  
	  if(!is.null(isolate(state$esquisse_code))){
	  tempcode<-isolate(state$esquisse_code)
	  tempcode<-substr(tempcode, instr(tempcode, "ggplot\\(data \\=")+14, nchar(tempcode))
	  tempcode<-substr(tempcode, 1, instr(tempcode, "\\)")-1)
	  message(tempcode)
	  }else{
	      tempcode<- "reload"
	  }
	#message("Look1")
	  updateRadioButtons(session, "data",
	    label = "Sample data to use:",
	    choices = c("iris", "mtcars", "reload" ),
	    selected = "reload",
	    inline = TRUE
	  )
	#message("Look2")
	
    })
  }
  
  #observeEvent(esquisse_input, {
#	  message("restoration")
#	  message(esquisse_input$esquisse-controls-title)
 # })


  # fix CSS JS
	dropmenu <- c("labsdrop","paramsdrop","filterdrop","codedrop")
	for ( elemdrop in dropmenu ){
		dropmenu0 <- dropmenu[dropmenu != elemdrop]
		shinyjs::runjs(paste0('document.getElementById("',elemdrop,'").id="',affix,'-',elemdrop,'";'))
		shinyjs::runjs(paste0('document.getElementById("sw-content-',elemdrop,'").id="',affix,'-','sw-content-',elemdrop,'";'))
		shinyjs::runjs(paste0('document.getElementById("',affix,'-',elemdrop,'").addEventListener("click", function() {       document.getElementById("',affix,'-','sw-content-',elemdrop,'").classList.toggle("sw-show"); });'))
		shinyjs::runjs(paste0('document.getElementById("',affix,'-','sw-content-',elemdrop,'").childNodes[1].childNodes[1].style.width="300px";'))
		for (elemdrop0 in dropmenu0){
			shinyjs::runjs(paste0('document.getElementById("',affix,'-',elemdrop,'").addEventListener("click", function() {       document.getElementById("',affix,'-','sw-content-',elemdrop0,'").classList.remove("sw-show"); });'))
		}
		
		
	}

  # quick select
  # data_r$data <- reactiveValues({
    # if (input$data == "iris") {
      # iris
    # } else if (input$data == "mtcars") {
      # mtcars
    # } else if (input$data == "reload") {
      # isolate(state$esquisse_data)
	# }
  # })
  
  # data_r$name <- reactiveValues({
    # if (input$data == "iris") {
      # "iris"
    # } else if (input$data == "mtcars") {
      # "mtcars"
    # } else if (input$data == "reload") {
      # "reload"
	# }
  # })
  data_to_restore=reactiveVal(TRUE)
  observeEvent(input[["data"]], {{    
    #message("I got involved!!!")
	message(input$data)
    if (input$data == "iris" && data_r$name != "iris" ) {
      data_r$data <- iris
      data_r$name <- "iris"
    } else if (input$data == "mtcars" && data_r$name != "mtcars" ) {
      data_r$data <- mtcars
      data_r$name <- "mtcars"
    } else if (input$data == "reload") {
	  #data_r$data <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
      data_r$data <- isolate(state$esquisse_data)
	  
	  if(!is.null(isolate(state$esquisse_code))){
	  tempcode<-isolate(state$esquisse_code)
	  tempcode<-substr(tempcode, instr(tempcode, "ggplot\\(data \\=")+14, nchar(tempcode))
	  tempcode<-substr(tempcode, 1, instr(tempcode, "\\)")-1)
	  message(tempcode)
	  }else{
	      tempcode<- "reload"
	  }
      data_r$name <- tempcode
      ##data_r$`esquisse-controls-title` <- isolate(state$esquisse_input$`esquisse-controls-title`)
      ##data_r$`esquisse-controls-x` <- isolate(state$esquisse_input$`esquisse-controls-x`)
      ##data_r$`esquisse-controls-y` <- isolate(state$esquisse_input$`esquisse-controls-y`)
	#restore main
	shinyjs::delay(1000,if(data_to_restore()){
	  targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet")
	  #for (dd in 1:200)		
		for (cc in 1:length(targetsIds)) {
		tempvar=isolate(state$esquisse_input$`esquisse-dragvars`$target[cc])
		compvar=isolate(state$esquisse_input$`esquisse-dragvars`$source)
		if (tempvar %in% compvar) {
			#message(paste0('document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("xyvar")[',cc,'-1].innerHTML+=("','<span class=\'label-dragula\' id=\'dragvars-target-label-',clean_string(tempvar),'\' data-value=\'',tempvar,'\'> <span class=\'label label-continue badge-dad\'>',tempvar,'</span> </span>','") '))
			#browser()
			shinyjs::runjs(paste0('document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("xyvar")[',cc,'-1].innerHTML+=("','<span class=\'label-dragula\' id=\'dragvars-target-label-',clean_string(tempvar),'\' data-value=\'',tempvar,'\'> <span class=\'label label-continue badge-dad\'>',tempvar,'</span> </span>','") '))

			#browser()
			
			}
			
			#cc=document.getElementById("dad-target-label-Petal_Length")
		}
		
		#message(paste0('$(document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("label-dragula")[1]).trigger("change") '))
		#browser()
		shinyjs::runjs(paste0('$(document.getElementById("',affix,'-labsdrop").parentElement.parentElement.parentElement.getElementsByClassName("label-dragula")[1]).trigger("change") '))		
		
		data_to_restore(FALSE)
	  })
	  ####### js css operation to move selected data 
    }
  }}, ignoreInit = FALSE)
  
  
  
  
  

  data00=callModule(module = esquisse::esquisserServer, id = "esquisse", data = data_r)

  # RMarkdown Code
  ggplot2orplotly=reactiveVal("ggplot2")
  observeEvent(input[["ggplot2orplotly"]],{
	#message("Hay!")
    #message(input[["ggplot2orplotly"]])
    ggplot2orplotly(input[["ggplot2orplotly"]])
  })
  txt_react <- reactive({
    #browser()
	#message("Hey!")
	#message(ggplot2orplotly())
    txt <- paste0("## ", input[["esquisse0"]],"\n",
	"```{r}\n",
	"library(ggplot2)\n",
	ifelse(ggplot2orplotly()=="plotly","p <- "," "), toString(data00$code),"\n",
    ifelse(ggplot2orplotly()=="plotly","plotly::ggplotly(p)"," "), "\n",
	"```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      esquisse0=input[["esquisse0"]],
	  esquisse_data=data00$data,
	  esquisse_code=data00$code,
	  esquisse_data_r=data_r,
	  #`esquisse-controls-title`=input$`esquisse-controls-title`,
	  esquisse_input=isolate(input),
      `__version__` = "1.0"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )

}


.globals$gizmos$esquisse <- list(
  ui=test_gizmo_esquisse_ui,
  server=test_gizmo_esquisse_server,
  library="vivid",
  opts=list()
)


run_esquisse <- function() run_standalone("esquisse")


