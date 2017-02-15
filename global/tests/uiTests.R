
ui <- navbarPage("test", theme = shinytheme("flatly"),
                 tabPanel("test",
                          fluidRow(
                            column(width = 4,
                                   tags$div(style="overflow-y:scroll; height:90vh;",
                                            "hello"),
                                   fileInput("import", "Import", accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                                   tags$label("Save / Export"),br(),
                                     actionButton("save", "Save all"),
                                     downloadButton("export", "Export"),
                                   br(),br()

                            ),
                            column(width=8, tags$p("data"))
                          )
                          
                          ))

server <- function (input, output) {
  
}

shinyApp(ui, server)