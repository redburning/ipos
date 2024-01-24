library(shiny)
library(shinydashboard)
library(shinyjs)

jscode <- "
shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("collapse")),
    actionButton("bt1", "Collapse box1"),
    actionButton("bt2", "Collapse box2"),
    br(), br(),
    box(id = "box1", title = "box-1", collapsible = TRUE, p("Box 1"), collapsed = TRUE, width = 2),
    box(id = "box2", title = "box-2", collapsible = TRUE, p("Box 2"))
  )
)

server <- function(input, output) {
  js$collapse("box1")
  
  observeEvent(input$bt1, {
    js$collapse("box1")
  })
  observeEvent(input$bt2, {
    js$collapse("box2")
  })
}

shinyApp(ui, server)