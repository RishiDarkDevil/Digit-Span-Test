library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyFeedback)
library(waiter)
library(tidyverse)
library(lubridate)

#----------------------------------------------------- Helpers

user_dig_seq <- tibble( # To store digit sequence
  try = c(1, 2, 3)
)
user_dig_seq[,paste0("r",1:100)] = ""

user_restart_wrong_data <- tibble(
  variable = c("n_restarts", "n_wrongs")
)
user_restart_wrong_data[,paste0("r",1:100)] <- 0
index_wrong <- rep(-1, 3*100) # index of each sequence starts from 1 assume


user_digit_click_time_data <- user_dig_seq %>%
  gather(colnames(user_dig_seq)[2:ncol(user_dig_seq)], key = "rounds", value = "dig_seq") %>%
  select(try, rounds)

user_digit_click_time_data[,paste0("c",1:102)] <- -1 # Here the c1 indicates the time difference(in secs) between guessing time start and a digit input(wrong/correct)

curr_user_data <- tibble(ID = 0, age = 0, sex = 0, educat = 0, job = 0, academic = 0, maths = 0, music = 0, env = 0)

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
  
  sliderInput("academic", "Your Academic Performance", value = 3, min = 1, max = 5),
  bsTooltip("academic", "1 indicating bad and 5 indicating excellent"),
  
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
  # updateActionButton(inputId = id, label = "X")
  updateActionButton(inputId = "restart", icon = icon("times"))
  if (retry) {
    updateActionButton(inputId = "next_correct", icon = icon("redo"))
  } else {
    updateActionButton(inputId = "next_correct", icon = icon("flag-checkered"))
  }
}

all_correct <- function() {
  # updateActionButton(inputId = id, label = "O")
  updateActionButton(inputId = "restart", icon = icon("check"))
}

# ------------------------- Intro UI
IntroUI <- tabPanelBody(
  "introPanel",
  h1("Digit Span Test", align = "center"),
  h2("Psychological Test for testing short term memory capacity", align = "center")
  
)




# ------------------------- Test UI
TestUI <- tabPanel(
  "DIGIT SPAN TEST",
  value = "testPanel",
  splitLayout(
    span(textOutput("display_digit"), style = "font-size:2000%; text-align: center; vertical-align: middle"),
    DigitPadUI
  )
)

# ------------------------ Results UI
ResultUI <- tabPanel(
  "PERFORMANCE",
  value = "perfPanel",
  
)

#-------------------------------------------------- Main UI
ui <- fluidPage(
  use_waitress(),
  sidebarLayout(
    UserDataUI,
    mainPanel(
      use_waiter(),
      tabsetPanel(
        id = "main",
        type = "hidden",
        selected = "introPanel",
        IntroUI,
        TestUI
      ),
    ),
  )
)

#-------------------------------------------------- Main Server
server <- function(input, output, session) {
  
  # Last ID
  last_ID <- 0
  append_to_prev <- FALSE
  index_wrong <- rep(0, 3*100)
  
  #------------- Deals with User Info
  filled_once <- reactiveVal(FALSE)
  curr_user <- reactiveVal(curr_user_data)
  
  observeEvent(input$start, {
    
    if (filled_once()) { return() }
    
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
    
    updateTabsetPanel(inputId = "main", selected = "testPanel")
    
    active(1)
    dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
    #all_dig_seq()[round_num()] <- dig_seq()
    message(dig_seq())
    
    curr_user(tibble(ID = last_ID+1, age = input$age, sex = input$sex, educat = input$educat, job = input$job, academic = input$academic, maths = input$maths, music = input$music, env = input$env))
    filled_once(TRUE)
  })

  
  #------------- Deals with Conducting Test
  
  disp_dig <- reactiveVal("GO") # digit to be displayed
  active <- reactiveVal(4) # Random Number displayer iterator
  dig_seq <- reactiveVal(sample(0:9, 2, replace = FALSE)) # the number to be guessed
  no_of_digs <- reactiveVal(2) # no of digits in the deg seq
  traverse <- reactiveVal(1) # traverse the deg seq to match with the user input if correct or not
  wrong_times <- reactiveVal(0) # No of wrong button clicks -- We will allow upto 3 mistakes
  restart_times <- reactiveVal(0) # No of times restart button is clicked - Each round will allow 2 restarts
  last_try <- reactiveVal(TRUE) # Last try correct or wrong
  user_dig_seq <- reactiveVal(user_dig_seq) # Store digit sequence displayed
  user_restart_wrong <- reactiveVal(user_restart_wrong_data) # Store the number of times reset and wrong was clicked in each round
  user_digit_click_time <- reactiveVal(user_digit_click_time_data) # Stores the time between clicks of the digits
  prev_hit_time <- reactiveVal(Sys.time()) # Stores absolute time of when the prev button was clicked
  #round_num <- reactiveVal(1) # Round Number
  
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
        prev_hit_time(Sys.time())
      }
  })
  
  
  # ---------------------- Capture Data 
  
  # ---- Captures digit sequence
  write_dig_seq <- function(seq_dig) { # Write a digit sequence passed as arg to the current position in the user_dig_seq table
    user_dig_seq_temp <- user_dig_seq()
    user_dig_seq_temp[(wrong_times()+1), (no_of_digs()-1)] <- paste(seq_dig, collapse = "")
    user_dig_seq(user_dig_seq_temp)
    if (!last_try()) {
      index_wrong[(((no_of_digs()-3)*3) + (wrong_times()+1))] <<- traverse()
    }
  }
  
  # ---- Captures restart times
  write_restart <- function(times_restart) {
    temp <- user_restart_wrong()
    temp[1, (no_of_digs()-1)] <- times_restart
    user_restart_wrong(temp)
  }
  
  write_wrong <- function(times_wrong) {
    temp <- user_restart_wrong()
    temp[2, 2:ncol(temp)] <- as.list(colSums(user_dig_seq()[,2:ncol(temp)] > 0))
    for (i in 2:(no_of_digs()-2)) {
      if ((temp[2,i] == 1) & (user_dig_seq()[2, i] == "") & (user_dig_seq()[2, i] == "")){ temp[2, i] = 0 }
      if((temp[2,i] == 2) & (user_dig_seq()[3, i] == "")){ temp[2,i] = 1 }
      if((temp[2,i] == 3) & (user_dig_seq()[1, i+1] != "")){ temp[2,i] = 2}  
    }
    user_restart_wrong(temp)
  }
  
  #--- Heading towards next test
  next_round <- function(restart = FALSE) {
    if (wrong_times() <= 2) {
      if (!restart & (no_of_digs() > 2) & last_try()) {
        write_dig_seq(dig_seq())
        #write_restart(restart_times())
      }
      if (last_try() & (!restart)) {
        no_of_digs(no_of_digs() + 1)
        wrong_times(0)
      }
      last_try(TRUE)
      # disp_dig("GO")
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
    if ((((traverse()-1) == no_of_digs()) | (!last_try())) & (wrong_times() <= 2)) {
      next_round()
      restart_times(0)
    }
  })
  
  #--- Restarting 
  observeEvent(input$restart, {
    # Restart Rules - Can restart 2 times in a round, Can restart only when all the numbers are displayed for a particular round
    # Can restart only if the user hasn't tried i.e. one cannot restart if he made a wrong guess
    # Restart is available only when the user hasn't tried anything
    if (restart_times() <= 1 & disp_dig() == "GUESS" & last_try() & (traverse() == 1)) {
      next_round(TRUE)
      restart_times(restart_times()+1)
      write_restart(restart_times())
    }
  })
  
  #--- Handling User DigitPad Input after showing a Number
  check_dig_inp <- function(id, val) {
    message("----")
    message(dig_seq())
    observe({
      isolate({
        waitress <- Waitress$new(selector = "#display_digit", theme = "overlay-opacity",min = 0, max = 100)
        
        if (last_try() & (disp_dig() == "GUESS")) {
          if (traverse() <= no_of_digs()) {
            if((dig_seq()[traverse()] == val)){
              curr_time <- Sys.time()
              if (no_of_digs() > 2) {
                temp <- user_digit_click_time()
                temp[(((no_of_digs()-3)*3) + (wrong_times()+1)), (traverse()+2)] <- as.numeric(difftime(curr_time, prev_hit_time(), units = "secs"))
                user_digit_click_time(temp)
                prev_hit_time(curr_time)
              }
              traverse(traverse()+1)
              last_try(TRUE)
              waitress$inc((100 / (no_of_digs())))
              if ((traverse()-1) == no_of_digs()) {
                waitress$close()
                all_correct()
              }
              message("correct")
            } else {
              curr_time <- Sys.time()
              if (no_of_digs() > 2) {
                temp <- user_digit_click_time()
                temp[(((no_of_digs()-3)*3) + (wrong_times()+1)), (traverse()+2)] <- as.numeric(difftime(curr_time, prev_hit_time(), units = "secs"))
                user_digit_click_time(temp)
                prev_hit_time(curr_time)
              }
              message("wrong")
              last_try(FALSE)
              if (no_of_digs() > 2) {
                write_dig_seq(dig_seq())
                write_wrong()
              }
              waitress$set(0)
              waitress$close()
              traverse(traverse()+1)
              wrong_times(wrong_times() + 1)
              if (wrong_times() <= 2) {
                wrong_input(id)
              } else {
                wrong_input(id, FALSE)
                
                # formatting the data lil bit before storing for less space usage and add ID to recognize particular user across all files
                last_ID <<- 0
                append_to_prev <<- FALSE
                if (file.exists("user_data.csv")) {
                  temp <- read_csv("user_data.csv")
                  last_ID <<- temp$ID[length(temp$ID)]
                  append_to_prev <<- TRUE
                }
                
                write_csv(curr_user(), "user_data.csv", append = append_to_prev)
                
                temp <- user_dig_seq()
                temp <- temp %>%
                  add_column(ID = c(last_ID+1, last_ID+1, last_ID+1)) %>%
                  select(ID, everything()) %>%
                  gather(paste0("r", 1:100), key = "rounds", value = "dig_seq") %>%
                  add_column(mis_ind = index_wrong) %>%
                  filter(dig_seq != "") %>%
                  arrange(parse_number(rounds), try)
                
                write_csv(temp, "user_dig_seq.csv", append = append_to_prev)
                
                temp <- user_restart_wrong()
                temp <- temp %>%
                  add_column(ID = c(last_ID+1, last_ID+1)) %>%
                  select(ID, everything()) %>%
                  gather(paste0("r", 1:100), key = "rounds", value = "n_times")
                
                write_csv(temp[1:(2*(no_of_digs()-2)),], "user_restart_wrong.csv", append = append_to_prev)
                
                temp <- user_digit_click_time()
                temp <- temp[1:(3*(no_of_digs()-2)), 1:(3*(no_of_digs()-2) + 2)] %>%
                  add_column(ID = (last_ID+1)) %>%
                  select(ID, rounds, everything()) %>%
                  gather(paste0("c", 1:(3*(no_of_digs()-2))), key = "clicks", value = "time_diff") %>%
                  arrange(parse_number(rounds), try) %>%
                  filter(time_diff != -1)
                
                write_csv(temp, "user_digit_click_time.csv", append = append_to_prev)
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

