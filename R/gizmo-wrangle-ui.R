
wrangle_ui <- function(ns){
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("input_data"),
                         label = "Input Dataset:",
                         choices = c(""),
                         multiple=FALSE,
                         options = list(`actions-box` = TRUE,
                                        `live-search`=TRUE,
                                        `none-selected-text`="Choose Data"))
      ),
      column(4, offset = 4,
             textInput(ns("output_name"), "Output Name","")
      )
    ),
    fluidRow(
      column(3,
             br(),
             bs_accordion(id = ns("dply_accord")) %>%
               bs_append(title = "Rows", content = tagList(
                 actionButton(ns("distinct_button"), "Drop Duplicates", width="100%"),
                 actionButton(ns("filter_button"), "Filter", width="100%")
               )
               ) %>%
               bs_append(title = "Variables", content = tagList(
               )
               ) %>%
               bs_append(title = "Compute", content = tagList(
               )
               )
      ),
      column(9,
             verticalTabsetPanel(id=ns("dplyr_tabset"))
      )
    )#,
    #tagsinput::tagsTextInput("kdkd","dddd", width="200px")
  )
}

