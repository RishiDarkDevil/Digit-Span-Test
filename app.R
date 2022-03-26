library(shiny)
library(shinyFeedback)
library(tidyverse)

# User Data/Info Module
UserDataUI <- function(id) {
  tagList(
    useShinyFeedback(),
    numericInput(NS(id, "age"), "Enter your Age", value = 19, min = 5, max = 100),
    
    radioButtons(NS(id, "sex"), "Gender", choiceNames = c("Male", "Female", "Prefer not to say"), choiceValues = c(0, 1, 2)),
    
    useShinyFeedback(),
    selectInput(NS(id, "educat"), "Education Qualification", choices = educat_choices),
    
    useShinyFeedback(),
    selectInput(NS(id, "job"), "Current Profession", choices = job_choices),
    
    useShinyFeedback(),
    radioButtons(NS(id, "maths"), "Are you constantly in touch with Mathematics?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
    
    useShinyFeedback(),
    radioButtons(NS(id, "music"), "Do you play Musical Intrument?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
    
    useShinyFeedback(),
    selectInput(NS(id, "env"), "Current Environment", choices = env_choices)
  )
}

UserDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      
      feedbackDanger("age", !is.integer(input$age), "Age cannot be fraction!")
      #feedbackDanger("age", (input$age < 0), "Age cannot be negative!")
      #feedbackWarning("age", (input$age > 100), "May you live for 100 more years!")
      
      #feedback()
      
      tibble(age = input$age, sex = input$sex, educat = input$educat, job = input$job, maths = input$maths, music = input$music, env = input$env)
      
    })
  })
}

# Main
ui <- fluidPage(
  UserDataUI("userdata"),
  tableOutput("user_inps_table")
)

server <- function(input, output, session) {
  user_inp_data <- UserDataServer("userdata")
  output$user_inps_table <- renderTable(head(user_inp_data()))
}

shinyApp(ui, server)
