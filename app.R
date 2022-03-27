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
    
    selectInput(NS(id, "job"), "Current Profession", choices = job_choices),
    
    useShinyFeedback(),
    radioButtons(NS(id, "maths"), "Are you constantly in touch with Mathematics?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
    
    useShinyFeedback(),
    radioButtons(NS(id, "music"), "Do you regularly play any Musical Intrument?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
    
    useShinyFeedback(),
    selectInput(NS(id, "env"), "Current Environment", choices = env_choices)
  )
}

UserDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      
      # Age Feedback
      feedbackDanger("age",
        ((!is.integer(input$age)) | (input$age < 0)) | (input$age > 100),
        ifelse(
          !is.integer(input$age), "Age cannot be fraction!",
          ifelse(
            (input$age < 0), "Age cannot be negative!", "May you live for 100 more years! But we don't allow age more than 100"
          )
        )
      )
      
      # Education Feedback
      feedback("educat",
        TRUE,
        "Select your last completed one"
      )
      
      # Maths Feedback
      feedback("maths",
               TRUE,
               "Select yes if your work or study involves Mathematics heavily"
      )
      
      # Music Feedback
      feedback("music",
               TRUE,
               "Select yes if you regularly play a musical instrument"
      )
      
      # Env Feedback
      feedback("env",
               TRUE,
               "Select the one relevant to the situation you are at right now"
      )
      
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
