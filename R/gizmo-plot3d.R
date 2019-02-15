test_gizmo_plot3d_ui <- function(ns){  
  data(USJudgeRatings);
  data(USArrests);
  fluidPage(h4("Plot: plot3d"),
            actionLink(
						ns("reload"),
						"Reload from globalenv()"
                    ),
            fluidRow(
              column(6,
                     selectInput(ns("selectdat"),
                                 "data",
								 ls(globalenv()),
                                 "USArrests"
                     )
				)
            ),
            fluidRow(
              column(4,
                     selectInput(ns("selectx"),
                                 "x:",
								 paste0("USArrests$",ls(USArrests)),
                                 "USArrests$Assault"
                     )
              ),
              column(4,
                     selectInput(ns("selecty"),
                                 "y:",
								 paste0("USArrests$",ls(USArrests)),
                                 "USArrests$Rape"
                     )
              ),
              column(4,
                     selectInput(ns("selectz"),
                                 "z:",
								 paste0("USArrests$",ls(USArrests)),
                                 "USArrests$UrbanPop"
                     )
              )
            )
  )
}

test_gizmo_plot3d_server <- function(input, output, session, set_rmarkdown_reactive){
  
   observeEvent(input[["reload"]],{
     updateSelectInput(session, "selectdat", "data by globalenv():", ls(.GlobalEnv), input[["selectdat"]])
   })

  observeEvent(input[["selectdat"]],{
	tryCatch({
		updateSelectInput(session, "selectx","x:", paste0(input[["selectdat"]],"$",eval(parse(text=paste0("ls(",input[["selectdat"]] ,")"))) ), input[["selectx"]] )
		updateSelectInput(session, "selecty","y:", paste0(input[["selectdat"]],"$",eval(parse(text=paste0("ls(",input[["selectdat"]] ,")"))) ), input[["selecty"]] )
		updateSelectInput(session, "selectz","z:", paste0(input[["selectdat"]],"$",eval(parse(text=paste0("ls(",input[["selectdat"]] ,")"))) ), input[["selectz"]] )
	}, error = function(e) {
		updateSelectInput(session, "selectx","x:", paste0(input[["selectdat"]],"$",c('NA') ) )
		updateSelectInput(session, "selecty","y:", paste0(input[["selectdat"]],"$",c('NA') ) )
		updateSelectInput(session, "selectz","z:", paste0(input[["selectdat"]],"$",c('NA') ) )
	})
  })  
  
  txt_react <- reactive({
    txt <- paste0(
      "<!-- Data: load data from package -->", "\n",
      "```{r}\n",
	  "head(", input[["selectdat"]], ")", "\n",
	  "#x <- ", input[["selectx"]],  "\n",
	  "#y <- ", input[["selecty"]],  "\n",
	  "#z <- ", input[["selectz"]],  "\n",
	  "plot3D::scatter3D(", input[["selectx"]],  ", ", input[["selecty"]],  ", ", input[["selectz"]],  ")", "\n",

      #"knitr::kable(",input[["selectdat"]],") %>%", "\n",
      #"  kableExtra::kable_styling() ", "\n",
      "```\n"
    )
    txt
  })
  set_rmarkdown_reactive(txt_react)
}

test_gizmo_plot3d_get_state <- function(input, output, session){
  list(data=eval(parse(text=input[["selectdat"]])),
  selectdat=input[["selectdat"]],
  selectx=input[["selectx"]],
  selecty=input[["selecty"]],
  selectz=input[["selectz"]]
  )
}

test_gizmo_plot3d_restore_state <- function(input, output, session, state){
    eval(parse(text=paste0( '.GlobalEnv$',  state$selectdat, "=state$data"      )))
	updateSelectInput(session, "selectdat", "data by globalenv():", ls(.GlobalEnv), state$selectdat)
	updateSelectInput(session, "selectx","x:", paste0(state$selectdat,"$",eval(parse(text=paste0("ls(",state$selectdat ,")"))) ), state$selectx )
	updateSelectInput(session, "selecty","y:", paste0(state$selectdat,"$",eval(parse(text=paste0("ls(",state$selectdat ,")"))) ), state$selecty )
	updateSelectInput(session, "selectz","z:", paste0(state$selectdat,"$",eval(parse(text=paste0("ls(",state$selectdat ,")"))) ), state$selectz )
}


.globals$gizmos$gizplot3d <- list(
  ui=test_gizmo_plot3d_ui,
  server=test_gizmo_plot3d_server,
  library="vivid",
  get_state=test_gizmo_plot3d_get_state,
  restore_state=test_gizmo_plot3d_restore_state,
  opts=list()
)

runPackageData <- function() run_standalone("gizplot3d")
