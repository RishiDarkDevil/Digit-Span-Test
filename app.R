library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyFeedback)
library(tidyverse)

#-------------------------------------------------- User Data/Info UI
UserDataUI <- sidebarPanel(
  titlePanel("YOUR INFO", "Digit Span Test"),
  
  useShinyFeedback(),
  numericInput("age", "Enter your Age", value = 19, min = 5, max = 100),
  
  radioButtons("sex", "Gender", choiceNames = c("Male", "Female", "Prefer not to say"), choiceValues = c(0, 1, 2)),
  
  selectInput("educat", "Education Qualification", choices = educat_choices),
  bsTooltip("educat", "Select the last completed one"),
  
  selectInput("job", "Current Profession", choices = job_choices, selected = NULL),
  bsTooltip("job", "Select Academia if you are Student/Professor/Teacher"),
  
  radioButtons("maths", "Are you constantly in touch with Mathematics?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
  bsTooltip("maths", "Select yes, if your work or study heavily uses Mathematics"),
  
  radioButtons("music", "Do you regularly play any Musical Intrument?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0)),
  
  selectInput("env", "Current Environment", choices = env_choices),
  bsTooltip("env", "Right now, what environment are you in?"),
  
  fluidRow(
    column(1, actionButton("start", "TAKE TEST")),
    column(1, span(textOutput("start_ok"), style = "color:#2383cc"), offset = 4)
  )
  
)


#------------------------------ Digit Pad UI
DigitPadUI <- fluidPage(
  fluidRow(
    column(12, align = "center",
      actionButton("1", "1", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("2", "2", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("3", "3", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
      actionButton("4", "4", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("5", "5", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("6", "6", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
      actionButton("7", "7", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("8", "8", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("9", "9", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
           actionButton("backspace", "", icon = icon("backspace"), style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
           actionButton("0", "0", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
           actionButton("next_correct", "", icon = icon("arrow-right"), style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  )
)

#--------------------------- Digit Pad Disable
disable_DigitPad <- function() {
  disable("1");disable("2");disable("3");disable("4");disable("5");disable("6");disable("7");disable("8");disable("9");disable("0");disable("backspace");disable("next_correct")
  updateActionButton(inputId = "1", label = "?");updateActionButton(inputId = "2", label = "?");updateActionButton(inputId = "3", label = "?");
  updateActionButton(inputId = "4", label = "?");updateActionButton(inputId = "5", label = "?");updateActionButton(inputId = "6", label = "?");
  updateActionButton(inputId = "7", label = "?");updateActionButton(inputId = "8", label = "?");updateActionButton(inputId = "9", label = "?");
  updateActionButton(inputId = "0", label = "?");updateActionButton(inputId = "backspace", icon = icon("question"));updateActionButton(inputId = "next_correct", icon = icon("question"))
}

#--------------------------- Digit Pad Enable
enable_DigitPad <- function() {
  enable("1");enable("2");enable("3");enable("4");enable("5");enable("6");enable("7");enable("8");enable("9");enable("0");enable("backspace");enable("next_correct")
  updateActionButton(inputId = "1", label = "1");updateActionButton(inputId = "2", label = "2");updateActionButton(inputId = "3", label = "3");
  updateActionButton(inputId = "4", label = "4");updateActionButton(inputId = "5", label = "5");updateActionButton(inputId = "6", label = "6");
  updateActionButton(inputId = "7", label = "7");updateActionButton(inputId = "8", label = "8");updateActionButton(inputId = "9", label = "9");
  updateActionButton(inputId = "0", label = "0");updateActionButton(inputId = "backspace", label = "", icon = icon("backspace"));updateActionButton(inputId = "next_correct", label = "", icon = icon("arrow-right"));
}

# ------------------------- Test UI
TestUI <- tabPanel(
  "DIGIT SPAN TEST",
  splitLayout(
    span(textOutput("display_digit"), style = "font-size:2000%; text-align: center; vertical-align: middle"),
    DigitPadUI
  )
)

#-------------------------------------------------- Main UI
ui <- fluidPage(
  sidebarLayout(
    UserDataUI,
    mainPanel(
      tabsetPanel(
        TestUI
      ),
    ),
  ),
  tableOutput("user_info_table")
)

#-------------------------------------------------- Main Server
server <- function(input, output, session) {
  
  #------------- Deals with User Info
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
  
  
  #------------- Deals with Conducting Test
  observeEvent(input$start, {
    active(1)
    dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
  })
  
  disp_dig <- reactiveVal(-1)
  active <- reactiveVal(11)
  dig_seq <- reactiveVal(sample(0:9, 2, replace = FALSE))
  no_of_digs <- reactiveVal(2)
  
  output$display_digit <- renderText({
    disp_dig()
  })
  
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active() <= no_of_digs()) {
        disp_dig(dig_seq()[active()])
        active(active() + 1)
        disable_DigitPad()
      } else {
        enable_DigitPad()
      }
    })
  })
  
  observeEvent(input$next_correct, {
    no_of_digs(no_of_digs() + 1)
    disp_dig(-1)
    active(1)
    dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
  })
}

shinyApp(ui, server)

