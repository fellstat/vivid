
test_gizmo_dynamic_ui <- function(ns){
	fluidPage(	  
	  shinyWidgets::dropdownButton( 
	                shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),
	                shinyjs::inlineCSS(".fa-tag-green { color: green }"),
					shinyjs::inlineCSS(".fa-tag-orange { color: orange }"),
					shinyjs::inlineCSS(".fa-tag-brown { color: brown }"),
					shinyjs::inlineCSS(".jstree-anchor>.fa-tag-black { color: black }"),
					shinyTree::shinyTree(ns("dattree"), checkbox = TRUE, search=TRUE,
                    types = "{ 'pkg-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}, 
				               'df-node': {'a_attr' : { 'style' : 'color:black' , class: 'no_checkbox'}}, 
                               'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }" ),
					circle = FALSE, 
					status = "default", 
					icon = icon("gear"),  
					label = textOutput(ns("dslabels"), inline = TRUE),
					tags$i(
					  tags$i(class="fa fa-tag fa-tag-brown","int"), 
					  tags$i(class="fa fa-tag fa-tag-orange","float"),
					  tags$i(class="fa fa-tag fa-tag-green","char")
					),
					#width = "300px",
					inputId=ns("ii")
    ),
	tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.maxHeight='400px'")),
	tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.maxWidth='250px'")),
	tags$script(paste0("document.getElementById('dropdown-menu-",ns(""),"ii').style.overflow='auto'")),
	tags$script(paste0("var sp1 = document.createElement('i');sp1.classList.add('fa');sp1.classList.add('fa-search');document.getElementById('",ns(""),"dattree-search-input').parentNode.insertBefore(sp1,document.getElementById('",ns(""),"dattree-search-input').nextSibling);")),
	tags$br(),
    verbatimTextOutput(ns("dattree_display")),tags$br()
	)
}

test_gizmo_dynamic_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      
    })
  } 

  variables <- reactiveVal(as.character(c()))
  datasets <- reactiveVal({library(shinyTree);list("NA"=structure("NA",sticon='fa fa-warning'))})
  remote_eval(vivid:::texasCi(), function(obj){
    library(shinyTree)
    
    session$onFlushed(function(){
  		shinyTree::updateTree(session, "dattree", data = obj)
    })
	datasets(obj)
  })

  output$dattree <- shinyTree::renderTree({
    library(shinyTree)
    datasets()
  })
  
  output$dslabels <- renderText(paste("X: ", {
    library(shinyTree)
	resu <- extract_local(input$dattree)
    if (length(resu)==0){
      "None"
    }else{
	  toString(resu)
    }
  }))
  
  output$dattree_display <- renderText({
		 if (is.null(input$dattree)){
      "None"
    }else{
	  paste0("dattree: ",toString(get_selected(input$dattree, format="names")))
	    }
  }) 
  
  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("## knitr::kable( )")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      
      `__version__` = "1.0"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$dynamicui <- list(
  ui=test_gizmo_dynamic_ui,
  server=test_gizmo_dynamic_server,
  library="vivid",
  opts=list()
)


run_dynamic_ui <- function() run_standalone("dynamicui")


extract_local <- function(dattree){
    library(shinyTree)
	resu <- list()
	try(for (pkg in names(dattree)){
		  for (dd in names(dattree[[pkg]])){
			for (slc in names(dattree[[pkg]][[dd]])){
			   try(if ( attr(dattree[[pkg]][[dd]][[slc]],"stselected")  ){
					resu <- c(resu, list(package=pkg,data=dd,col=slc))
			   }, silent=TRUE)
			}
		  }
		},
	silent=TRUE)
	resu
} 
