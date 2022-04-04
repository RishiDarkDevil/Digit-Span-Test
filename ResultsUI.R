library(shiny)
library(shinydashboard)

# ------------------------ Results UI
ResultUI <- tabPanelBody(
  value = "perfPanel",
  dashboardPage(
    dashboardHeader(title = "Your Performance"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 60px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
      fluidRow(
        valueBox(9, "Digit Span Score", icon = icon("trophy"), width = 3),
        valueBox(25.5, "Average Time", icon = icon("clock"), width = 3),
        # bsTooltip(""),
        valueBox(5, "Mistakes", icon = icon("times"), width = 3),
        valueBox(3, "Restarts", icon = icon("sync"), width = 3)
      ),
      fluidRow(
        column(
          width = 6,
          tabBox(
            title = "Rounds",
            id = "Rounds", height = "250px", width = 12,
            tabPanel(
              "Round 1",
              value = "round1digseq",
              infoBox("Sequence", 123456789, icon = icon("list"), width = 8),
              infoBox("Mistakes", 1, icon = icon("times"), width = 4),
              infoBox("Time", 123, icon = icon("clock"), width = 8),
              infoBox("Restarts", 1, icon = icon("redo"), width = 4)
            )
          ),
          box(
            title = "Average Click Time Each Round", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("time_this_round")
          )
        ),
        column(
          width = 6,
          box(
            title = "Click Times in this Round", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("time_this_round")
          ),
          box(
            title = "Leaderboard", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
            infoBox("Digit Span", "Top 10%", icon = icon("list"), fill = TRUE, width = 6),
            infoBox("Fill Something", "1", icon = icon("times"), fill = TRUE, width = 6),
            infoBox("Time Taken", "Top 20%", icon = icon("clock"), fill = TRUE, width = 6),
            infoBox("Fill Something", "1", icon = icon("redo"), fill = TRUE, width = 6)
          )
        )
      ),
      
    )
  )
)

ui <- fluidPage(
  tabsetPanel(
    ResultUI
  )
)

server <- function(input, output, session) {
  output$dspscore <- renderText("9")
}

shinyApp(ui, server)
