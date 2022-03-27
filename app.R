library(shiny)
library(shinyBS)
library(shinyFeedback)
library(tidyverse)

#-------------------------------------------------- User Data/Info UI
UserDataUI <- fluidPage(
  titlePanel("YOUR INFO", "Digit Span Test"),
  fluidRow(
    useShinyFeedback(),
    numericInput("age", "Enter your Age", value = 19, min = 5, max = 100)
  ),
  fluidRow(
    radioButtons("sex", "Gender", choiceNames = c("Male", "Female", "Prefer not to say"), choiceValues = c(0, 1, 2)),
  ),
  fluidRow(
    selectInput("educat", "Education Qualification", choices = educat_choices),
    bsTooltip("educat", "Select the last completed one")
  ),
  fluidRow(
    selectInput("job", "Current Profession", choices = job_choices, selected = NULL),
    bsTooltip("job", "Select Academia if you are Student/Professor/Teacher")
  ),
  fluidRow(
    radioButtons("maths", "Are you constantly in touch with Mathematics?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
    bsTooltip("maths", "Select yes, if your work or study heavily uses Mathematics")
  ),
  fluidRow(
    radioButtons("music", "Do you regularly play any Musical Intrument?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0))
  ),
  fluidRow(
    selectInput("env", "Current Environment", choices = env_choices),
    bsTooltip("env", "Right now, what environment are you in?")
  ),
  fluidRow(
    column(1, actionButton("start", "START")),
    column(1, textOutput("start_ok"))
  )
  
)

#-------------------------------------------------- Main UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      UserDataUI
    ),
    mainPanel(
      
    ),
  ),
  tableOutput("user_info_table")
)

#-------------------------------------------------- Main Server
server <- function(input, output, session) {
  user_info <- eventReactive(input$start, {
    
    if (((!is.integer(input$age)) | (input$age < 0)) | (input$age > 100)) {
      
      # Age Feedback
      feedbackDanger("age",
                     ((!is.integer(input$age)) | (input$age < 0)) | (input$age > 100),
                     ifelse(
                       !is.integer(input$age), "Age cannot be fraction!",
                       ifelse(
                         (input$age < 0), "Age cannot be negative!", "May you live for 100 more years! But I don't allow age more than 100"
                       )
                     )
      )
      output$start_ok <- renderText("")
      validate("Check for issues!")
    }
    output$start_ok <- renderText("Successfully Recorded.")
    
    tibble(age = input$age, sex = input$sex, educat = input$educat, job = input$job, maths = input$maths, music = input$music, env = input$env)
  })
  
  output$user_info_table <- renderTable(user_info())
}

shinyApp(ui, server)
