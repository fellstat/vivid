
test_gizmo_dynamic_ui <- function(ns){
	fluidPage(	  
	  shinyWidgets::dropdownButton(
	                shinyjs::inlineCSS(".no_checkbox>i.jstree-checkbox { display:none }"),				 
					shinyTree::shinyTree(ns("dattree"), checkbox = TRUE, search=TRUE,
                types = "{ 'topp-node': {'a_attr' : { 'style' : 'color:red' , class: 'no_checkbox'}}, 
                'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }" ),
					circle = FALSE, 
					status = "default", 
					icon = icon("gear"),  
					label = "Data Selection", 
					#width = "300px",
					inputId=ns("ii")
    ),tags$br(),
    verbatimTextOutput(ns("dattree_display")),tags$br()
	)
}

test_gizmo_dynamic_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      
    })
  } 

  #datasets <- reactiveVal(c())
  #remote_eval(vivid:::.get_data()$objects, function(obj){
  #  names(obj) <- obj
  #  print(obj)
  #  datasets(obj)
  #  session$onFlushed(function(){
  #
  #  })
  #})


texasCi <- function(){
  library(shinyTree)
  library(ggplot2)
  
  envirs=base::search()
  Tree0s=list()
  
  for (envir in envirs) {
    pkgname=envir
    if(substr(envir,1,8)=="package:"){pkgname=substr(envir,9,1000)}
    if(substr(envir,1,6)=="tools:"){pkgname=substr(envir,7,1000)}
    ccs=sapply(sapply(ls(as.environment(envir)), get), is.data.frame)
    dds=names(ccs)[(ccs==TRUE)]
    Tree1s=list()
    for (dd in dds) {
      Tree2s=list()
      if(substr(envir,1,8)=="package:"){
        TreeA=list()
        
        try(eval(parse(text=paste0("TreeA=(names(",pkgname,"::", dd,"))"))))
        for (Treea in TreeA){
          try(eval(parse(text=paste0("Tree2s=c(Tree2s, '",Treea,"'=list(structure(\"",Treea,"\",sticon=\"tag\")))"))))
        }
      } else if (substr(envir,1,6)=="tools:"){
        
      } else if (envir==".GlobalEnv"){
        TreeA=list()
        #browser()
        try(eval(parse(text=paste0("TreeA=(names(",".GlobalEnv","$", dd,"))"))))
        #TreeA=datasets()
        for (Treea in TreeA){
          try(eval(parse(text=paste0("Tree2s=c(Tree2s, '",Treea,"'=list(structure(\"",Treea,"\",sticon='tag')))"))))
        }
      }			
      if(length(Tree2s))try(eval(parse(text=paste0("Tree1s=c(Tree1s,'",dd,"'=list(structure(Tree2s,sticon='tags')))"))))
    }
    if(length(Tree1s))try(eval(parse(text=paste0("Tree0s=c(Tree0s,'",pkgname,"'=list(structure(Tree1s,sttype='topp-node')))"))))
  }
  Tree0s
}

  output$dattree <- shinyTree::renderTree({
    library(shinyTree)
    list('.GlobalEnv'= structure(list( 
      'iris' =  structure(list('Sepal.Length'=structure('iris$Sepal.Length',sticon="tag"), 'Sepal.Width'=structure('iris$Sepal.Width',sticon="tag"),'Petal.Length'=structure('iris$Petal.Length',sticon="tag")),stselected=TRUE, sticon="tags"),  
      'mtcars' =  structure(list('mpg'='mtcars$mpg','cyl'='mtcars$mpg'), stselected=TRUE, sticon="tags")), stopened=TRUE, sticon="cloud")) 
  
    texasCi()
  })

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
