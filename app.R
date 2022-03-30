library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyFeedback)
library(waiter)
library(tidyverse)

#-------------------------------------------------- User Data/Info UI
educat_choices <- c(0, 1, 2, 3, 4)
educat_choices <- setNames(educat_choices, c("Still in school, below 12th Grade", "12th Grade", "Bachelors", "Masters", "PhD"))
educat_choices

job_choices <- c(0, 1, 2, 3)
job_choices <- setNames(job_choices, c("Academia", "Industry", "Business", "Not working yet"))
job_choices

env_choices <- c(0, 1, 2, 3)
env_choices <- setNames(env_choices, c("Silent", "Normal", "Little Noisy", "Very Noisy"))
env_choices

UserDataUI <- sidebarPanel(
  titlePanel("YOUR INFO", "Digit Span Test"),
  
  useShinyFeedback(),
  numericInput("age", "Enter your Age", value = 19, min = 5, max = 100),
  
  radioButtons("sex", "Gender", choiceNames = c("Male", "Female"), choiceValues = c(0, 1)),
  
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
      actionButton("one", "1", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("two", "2", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("three", "3", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
      actionButton("four", "4", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("five", "5", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("six", "6", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
      actionButton("seven", "7", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("eight", "8", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
      actionButton("nine", "9", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  ),
  fluidRow(
    column(12, align = "center",
           actionButton("restart", "", icon = icon("sync"), style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
           actionButton("zero", "0", style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px'),
           actionButton("next_correct", "", icon = icon("arrow-right"), style='padding-left:25px; padding-right:25px; font-size:700%; margin: 25px')
    )
  )
)

#--------------------------- Digit Pad Disable
disable_DigitPad <- function() {
  disable("one");disable("two");disable("three");disable("four");disable("five");disable("six");disable("seven");disable("eight");disable("nine");disable("zero");disable("backspace");disable("next_correct")
  updateActionButton(inputId = "one", label = "?");updateActionButton(inputId = "two", label = "?");updateActionButton(inputId = "three", label = "?");
  updateActionButton(inputId = "four", label = "?");updateActionButton(inputId = "five", label = "?");updateActionButton(inputId = "six", label = "?");
  updateActionButton(inputId = "seven", label = "?");updateActionButton(inputId = "eight", label = "?");updateActionButton(inputId = "nine", label = "?");
  updateActionButton(inputId = "zero", label = "?");updateActionButton(inputId = "restart", icon = icon("question"));updateActionButton(inputId = "next_correct", icon = icon("question"))
}

#--------------------------- Digit Pad Enable
enable_DigitPad <- function() {
  enable("one");enable("two");enable("three");enable("four");enable("five");enable("six");enable("seven");enable("eight");enable("nine");enable("zero");enable("backspace");enable("next_correct")
  updateActionButton(inputId = "one", label = "1");updateActionButton(inputId = "two", label = "2");updateActionButton(inputId = "three", label = "3");
  updateActionButton(inputId = "four", label = "4");updateActionButton(inputId = "five", label = "5");updateActionButton(inputId = "six", label = "6");
  updateActionButton(inputId = "seven", label = "7");updateActionButton(inputId = "eight", label = "8");updateActionButton(inputId = "nine", label = "9");
  updateActionButton(inputId = "zero", label = "0");updateActionButton(inputId = "restart", label = "", icon = icon("sync"));updateActionButton(inputId = "next_correct", label = "", icon = icon("arrow-right"));
}

wrong_input <- function(id, retry = TRUE) {
  updateActionButton(inputId = id, label = "X")
  if (retry) {
    updateActionButton(inputId = "next_correct", icon = icon("redo"))
  } else {
    updateActionButton(inputId = "next_correct", icon = icon("flag-checkered"))
  }
}

correct_input <- function(id) {
  updateActionButton(inputId = id, label = "O")
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
  use_waitress(),
  sidebarLayout(
    UserDataUI,
    mainPanel(
      use_waiter(),
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
    
    Sys.sleep(0.5)
    
    waiter <- Waiter$new(
      html = tagList(
        spin_fading_circles(),
        "First One is a Trial..."
      )
    )
    waiter$show()
    on.exit(waiter$hide())
    
    Sys.sleep(3)
    
    active(1)
    dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
    message(dig_seq())
    
    tibble(age = input$age, sex = input$sex, educat = input$educat, job = input$job, maths = input$maths, music = input$music, env = input$env)
  })
  
  output$user_info_table <- renderTable(user_info())
  
  
  #------------- Deals with Conducting Test
  
  disp_dig <- reactiveVal("GO") # digit to be displayed
  active <- reactiveVal(4) # Random Number displayer iterator
  dig_seq <- reactiveVal(sample(0:9, 2, replace = FALSE)) # the number to be guessed
  no_of_digs <- reactiveVal(2) # no of digits in the deg seq
  traverse <- reactiveVal(1) # traverse the deg seq to match with the user input if correct or not
  wrong_times <- reactiveVal(0) # No of wrong button clicks -- We will allow upto 3 mistakes
  restart_times <- reactiveVal(0) # No of times restart button is clicked
  last_try <- reactiveVal(TRUE) # Last try correct or wrong
  
  #last_hit_dig <- reactiveVal(-1) # what was the last clicked button label
  #last_hit_index <- reactiveVal(-1) # Stores the index in the deg seq where wrong button was clicked
  
  output$display_digit <- renderText({
    disp_dig()
  })
  
  #---------- Displaying Random Numbers and Diabling and enabling DigitPad
  observe({
    invalidateLater(1000, session)
    isolate({
      waitress <- Waitress$new(max = no_of_digs(), theme = "line")
      if (active() <= no_of_digs()) {
        disp_dig(dig_seq()[active()])
        active(active() + 1)
        disable_DigitPad()
        waitress$set(active()-1)
      } else {
        waitress$close()
        if ((traverse() == 1) & ((active()-1) == no_of_digs())) {
          disp_dig("GUESS")
        }
      }
    })
  })
  
  observe({
      if (disp_dig() == "GUESS") {
        enable_DigitPad()
      }
  })
  
  #--- Heading towards next test
  next_round <- function(restart = FALSE) {
    if (wrong_times() <= 2) {
      if (last_try() & (!restart)) {
        no_of_digs(no_of_digs() + 1)
      }
      last_try(TRUE)
      disp_dig("GO")
      active(1)
      traverse(1)
      if (no_of_digs() <= 10) {
        dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
      } else {
        dig_seq(sample(0:9, no_of_digs(), replace = TRUE))
      }
      
      message(dig_seq())
    }
  }
  
  observeEvent(input$next_correct, {
    if ((traverse()-1) == no_of_digs()) {
      next_round()
    }
  })
  
  #--- Restarting 
  observeEvent(input$restart, {
    next_round(TRUE)
  })
  
  #--- Handling User DigitPad Input after showing a Number
  check_dig_inp <- function(id, val) {
    message("----")
    message(dig_seq())
    observe({
      isolate({
        if (last_try()) {
          if (traverse() <= no_of_digs()) {
            if((dig_seq()[traverse()] == val)){
              traverse(traverse()+1)
              last_try(TRUE)
              correct_input(id)
              message("correct")
            } else {
              message("wrong")
              traverse(traverse()+1)
              last_try(FALSE)
              wrong_times(wrong_times() + 1)
              if (wrong_times() <= 2) {
                wrong_input(id)
              } else {
                wrong_input(id, FALSE)
              }
            }
          }
        }
      })
    })
  }
  
  observeEvent(input$one, check_dig_inp("one", 1))
  observeEvent(input$two, check_dig_inp("two", 2))
  observeEvent(input$three, check_dig_inp("three", 3))
  observeEvent(input$four, check_dig_inp("four", 4))
  observeEvent(input$five, check_dig_inp("five", 5))
  observeEvent(input$six, check_dig_inp("six", 6))
  observeEvent(input$seven, check_dig_inp("seven", 7))
  observeEvent(input$eight, check_dig_inp("eight", 8))
  observeEvent(input$nine, check_dig_inp("nine", 9))
  observeEvent(input$zero, check_dig_inp("zero", 0))
}

shinyApp(ui, server)

