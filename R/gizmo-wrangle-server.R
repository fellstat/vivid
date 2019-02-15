


wrangle_server <- function(input, output, session, set_rmarkdown_reactive){
  ns <- session$ns
  variables <- reactiveVal(as.character(c()))
  ops <- new.env(parent = emptyenv())
  ops$list <- list()
  session$userData[[session$ns("state")]] <- ops

  append_operation <- function(ui, server, title){
    uuid <- vivid::gen_uuid()
    appendVerticalTab(
      ns("dplyr_tabset"),
      verticalTabPanel(
        title = title,
        ui(ns(uuid))
      )
    )
    v <- variables
    if(length(ops$list) > 0)
      v <- ops$list[[length(ops$list)]]$output_variables
    op <- callModule(server, uuid, last_data, v)
    ops$list[[length(ops$list) + 1]] <- op
  }

  remote_eval(vivid:::.get_data()$objects, function(obj){
    names(obj) <- obj
    updatePickerInput(session, inputId = "input_data", choices = obj)
  })

  last_data <- reactiveVal("")
  observe({
    dat <- input$input_data
    expr <- parse(text=paste0("vivid:::.get_objects(envir=", dat, ")"))
    remote_eval(
      expr,
      callback = function(x){
        last_data(dat)
        variables(x)
        #variables[[1]](x)["objects"]
      },
      substitute = FALSE
    )
  })


  observeEvent(input$distinct_button, {
    uuid <- vivid::gen_uuid()
    append_operation(drop_dups_ui, drop_dups_server, "Drop Duplicates")
  })

  observeEvent(input$filter_button, {
    uuid <- vivid::gen_uuid()
    append_operation(filter_ui, filter_server, "Filter")
  })

  txt <- reactive({
    df <- input$input_data
    od <- input$output_name
    if(is.null(od) || nchar(od) == 0){
      od <- paste0(df,"_tr")
    }
    code <- paste0(
      "```{r}\n",
      od, " <- ", df
    )

    for(op in ops$list){
      code <- paste0(code," %>%\n  ", op$code())
    }
    code <- paste0(code,"\nhead(", od, ")\n```")
    code
  })
  set_rmarkdown_reactive(txt)
  txt
}







run_wrangle_data <- function() run_standalone("wrangle_data")

