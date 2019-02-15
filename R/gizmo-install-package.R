test_gizmo_install_ui <- function(ns){
  fluidPage(h4("Install: Install package"),
            fluidRow(
              column(4,
                     textInput(ns("selectpkg"),
                                 "package:",
                                 "ggplot2"
                     )
              ),column(4,
			  tags$a("Packages Available", href="https://cran.rstudio.com/src/contrib/PACKAGES")
			  )
            )
  )
}

test_gizmo_install_server <- function(input, output, session, set_rmarkdown_reactive){

  txt_react <- reactive({
    txt <- paste0(
      "<!-- Data: load data from package -->", "\n",
      "```{r}\n",
	  "options(repos='http://cran.rstudio.com/')", "\n",
      "install.packages(\"", input[["selectpkg"]], "\")", "\n",
      #"knitr::kable(",input[["selectdat"]],") %>%", "\n",
      #"  kableExtra::kable_styling() ", "\n",
      "```\n"
    )
    txt
  })
  set_rmarkdown_reactive(txt_react)
}

test_gizmo_install_get_state <- function(input, output, session){
  list(selectpkg=input[["selectpkg"]]
  )
}

test_gizmo_install_restore_state <- function(input, output, session, state){
    updateTextInput(session, "selectpkg", "package:", state$selectpkg)
}


.globals$gizmos$gizinstall <- list(
  ui=test_gizmo_install_ui,
  server=test_gizmo_install_server,
  library="vivid",
  get_state=test_gizmo_install_get_state,
  restore_state=test_gizmo_install_restore_state,
  opts=list()
)

runPackageData <- function() run_standalone("gizinstall")
