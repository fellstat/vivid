
test_gizmo_esquisse_ui <- function(ns){
	fluidPage(
	    shinyjs::useShinyjs(),
	    shinyjs::inlineCSS("html, body  { overflow: auto;  } "),
		shinyjs::inlineCSS(".sw-dropdown { position: relative; display: inline-block;  } "),
		textInput(ns("esquisse0"), "input", value="Ready."),
		 fluidPage(
		  tags$h1("Esquisse: ggplot2 Builder"),
		  fluidRow(
			  column(9,radioButtons(
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
		  tags$div(
			style = "height: 700px; ",
			esquisse::esquisserUI(
			  id = ns("esquisse"),
			  header = FALSE,
			  choose_data = TRUE
			)
		  ),
		  shinyjs::inlineCSS("html, body  { overflow: auto;   } "),
		  shinyjs::inlineCSS(".sw-dropdown { position: relative; display: inline-block;  } ")
		),tags$br()
	)
}

test_gizmo_esquisse_server <- function(input, output, session, state=NULL){
  affix <- gen_uuid()
  #data_r <- reactiveValues(data = ToothGrowth, name = "ToothGrowth")
  data_r <- reactiveValues(data = iris, name = "iris")
  tempcode=isolate(state$esquisse_code)
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
	  
	  if(!is.null(tempcode)){
	  #browser()
	  tempcode=substr(tempcode, instr(tempcode, "ggplot\\(data \\=")+14, nchar(tempcode))
	  tempcode=substr(tempcode, 1, instr(tempcode, "\\)")-1)
	  #message(tempcode)
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
		}
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


