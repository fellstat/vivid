test_gizmo_data_ui <- function(ns){
  fluidPage(h4("Data: load data from package"),
            fluidRow(
              column(4,
                     selectInput(ns("selectpkg"),
                                 "package:",
                                 unique(as.data.frame(data(package=.packages(all.available=TRUE))$results)$Package)
                     )
              ),
              column(4,
                     selectInput(ns("selectdat"),
                                 "data:",
                                 c('NA'))
              ),
              column(4,
                     textInput(ns("renameas"),
                               "rename as:",
                               'NA')
              )
            )
  )
}

test_gizmo_data_server <- function(input, output, session, set_rmarkdown_reactive){
  observeEvent(input[["selectpkg"]], {
    updateSelectInput(session,"selectdat","data:",
                      choices = as.vector(data(package = input[["selectpkg"]])$results[, "Item"]),
                      selected = input[["selectdat"]]
    )
  })
  observeEvent(input[["selectdat"]],{
    updateTextInput(session,"renameas","rename as:",
                    input[["selectdat"]]
    )
  })
  txt_react <- reactive({
    txt <- paste0(
      "<!-- Data: load data from package -->", "\n",
      "```{r}\n",
      "library(", input[["selectpkg"]], ")", "\n",
      "data(", input[["selectdat"]], ")", "\n",
      if(input[["renameas"]]!=input[["selectdat"]]) paste0(input[["renameas"]]," <-",input[["selectdat"]],"\n"),
      "head(", input[["renameas"]], ")", "\n",
      #"knitr::kable(",input[["selectdat"]],") %>%", "\n",
      #"  kableExtra::kable_styling() ", "\n",
      "```\n"
    )
    txt
  })
  set_rmarkdown_reactive(txt_react)
}

test_gizmo_data_get_state <- function(input, output, session){
  list(selectpkg=input[["selectpkg"]],
       selectdat=input[["selectdat"]],
       renameas=input[["renameas"]] )
}

test_gizmo_data_restore_state <- function(input, output, session, state){
  c(
    updateSelectInput(session,"selectpkg","package:",choices = unique(as.data.frame(data(package=.packages(all.available=TRUE))$results)$Package),selected = state$selectpkg),

    updateSelectInput(session,"selectdat","data1:",choices = as.vector(data(package = state$selectpkg)$results[, "Item"]),selected = state$selectdat),
    updateTextInput(session, "renameas", value=state$renameas)
  )
}


.globals$gizmos$gizdata <- list(
  ui=test_gizmo_data_ui,
  server=test_gizmo_data_server,
  library="vivid",
  get_state=test_gizmo_data_get_state,
  restore_state=test_gizmo_data_restore_state,
  opts=list()
)
