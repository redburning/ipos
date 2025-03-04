source("global.R", encoding = "UTF-8")

helpButton <- function(id, label, class) {
  ns <- NS(id)
  actionButton(inputId = ns("help"),
               label = "帮助文档",
               icon = icon("book"),
               class = "help-button")
}


helpServer <- function(input, output, session, title, size, file) {
  observeEvent(input$help, {
    showModal(modalDialog(
      title = title, 
      size = size,
      fluidPage(includeMarkdown(file.path(getwd(), file))),
      easyClose = TRUE, 
      fade = FALSE,
      footer = tagList(
        modalButton(label = "确认")
      ),
      style="max-height:500px; overflow:auto;"
    ))
  })
}