test_gizmo_data_ui <- function(ns) {
  fluidPage(h4("Data: load data from package"),
            fluidRow(
              column(
                4,
                selectInput(
                  ns("selectpkg"),
                  "Package",
                  unique(as.data.frame(data(
                    package = .packages(all.available = TRUE)
                  )$results)$Package),
                  selected = "datasets"
                )
              ),
              column(4,
                     selectInput(
                       ns("selectdat"),
                       "Data",
                       as.character(as.vector(data(package = "datasets")$results[, "Item"]))
                     )),
              column(4,
                     textInput(ns("renameas"),
                               "Rename:",
                               ''))
            ))
}


test_gizmo_data_server <- function(input, output, session, state = NULL) {

    observeEvent(input[["selectpkg"]], {
      updateSelectInput(session, "selectdat",
                        choices = as.vector(data(package = input[["selectpkg"]])$results[, "Item"]),
                        selected=input[["selectdat"]])
    }, ignoreInit=TRUE)
    observeEvent(input[["selectdat"]], {
      updateTextInput(session, "renameas", "rename as:",
                      input[["selectdat"]])
    })
    txt_react <- reactive({
      txt <- paste0(
        "<!-- Data: load data from package -->",
        "\n",
        "```{r}\n",
        "library(",
        input[["selectpkg"]],
        ")",
        "\n",
        "data(",
        input[["selectdat"]],
        ")",
        "\n",
        if (input[["renameas"]] != input[["selectdat"]])
          paste0(input[["renameas"]], " <-", input[["selectdat"]], "\n"),
        "head(",
        input[["renameas"]],
        ")",
        "\n",
        #"knitr::kable(",input[["selectdat"]],") %>%", "\n",
        #"  kableExtra::kable_styling() ", "\n",
        "```\n"
      )
      txt
    })

    get_state <- function(){
      list(selectpkg = input[["selectpkg"]],
           selectdat = input[["selectdat"]],
           renameas = input[["renameas"]],
           `__version__` = "0.1")
    }

    if (!is.null(state)) {
      session$onFlushed(function() {
        updateSelectInput(session, "selectpkg", selected = state$selectpkg)
        updateSelectInput(
          session,
          "selectdat",
          choices = as.vector(data(package = state$selectpkg)$results[, "Item"]),
          selected = state$selectdat
        )
        updateTextInput(session, "renameas", state$renameas)
      })
    }

    list(
      code = txt_react,
      get_state = get_state
    )
  }


.globals$gizmos$gizdata <- list(
  ui = test_gizmo_data_ui,
  server = test_gizmo_data_server,
  library = "vivid",
  opts = list()
)

run_package_data <- function()
  run_standalone("gizdata")
