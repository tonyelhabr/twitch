library(shiny)
library(bs4Dash)
library(waiter)
shinyApp(
  ui = dashboardPage(
    preloader = list(
      waiter = list(html = tagList(spin_1(), "Loading ...")), 
      color = "#3c8dbc"
    ),
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      actionButton("reload", "Reload")
    ),
    title = "Preloader"
  ),
  server = function(input, output, session) {
    # fake 
    observeEvent(input$reload, {
      Sys.sleep(1)
      session$reload()
    })
  }
)
