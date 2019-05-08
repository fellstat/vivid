
test_gizmo_esquisse_ui <- function(ns){
	fluidPage(
	    shinyjs::useShinyjs(),
	    shinyjs::inlineCSS("html, body  { overflow: auto;  } "),
		shinyjs::inlineCSS(".sw-dropdown { position: relative; display: inline-block;  } "),
		textInput(ns("esquisse0"), "input", value="Ready."),
		 fluidPage(
		  tags$h1("Esquisse: ggplot2 Builder"),
		  radioButtons(
			inputId = ns("data"), 
			label = "Sample data to use:", 
			choices = c("iris", "mtcars"),
			inline = TRUE
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
  data_r <- reactiveValues(data = iris, name = "iris")
  
  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "esquisse0", value=state$esquisse0)
	  data_r=state$esquisse_data
    })
  }

  # fix CSS JS
	affix <- gen_uuid()
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
  observeEvent(input[["data"]], {
    if (input$data == "iris") {
      data_r$data <- iris
      data_r$name <- "iris"
    } else {
      data_r$data <- mtcars
      data_r$name <- "mtcars"
    }
  })
  data00=callModule(module = esquisse::esquisserServer, id = "esquisse", data = data_r)

  
  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("## ", input[["esquisse0"]],"\n",
	"```{r}\n",
	"library(ggplot2)\n",
	toString(data00$code),"\n",
	"```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      esquisse0=input[["esquisse0"]],
	  esquisse_data=data00$data,
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
