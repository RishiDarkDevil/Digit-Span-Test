library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyFeedback)
library(shinydashboard)
library(shinyWidgets)
library(waiter)
library(tidyverse)
library(lubridate)
library(viridis)
library(googledrive)
library(googlesheets4)

#----------------------------------------------------- Google Sheets 
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "DIGIT SPAN/.secrets"
)

#drive_auth()
#gs4_auth()

# -------------- Creating GoogleSheets
#gs4_create(name = "user_data", sheets = "main")
#gs4_create(name = "user_dig_seq", sheets = "main")
#gs4_create(name = "user_digit_click_time", sheets = "main")
#gs4_create(name = "user_restart_wrong", sheets = "main")

# -------------- Get IDs
user_data_id <- drive_get("user_data")$id
user_dig_seq_id <- drive_get("user_dig_seq")$id
user_digit_click_time_id <- drive_get("user_digit_click_time")$id
user_restart_wrong_id <- drive_get("user_restart_wrong")$id 

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

job_choices <- c(0, 1, 2, 3)
job_choices <- setNames(job_choices, c("Academia", "Industry", "Business", "Not working yet"))

env_choices <- c(0, 1, 2, 3)
env_choices <- setNames(env_choices, c("Silent", "Normal", "Little Noisy", "Very Noisy"))



UserDataUI <- fluidPage(
  titlePanel("YOUR INFO", "Digit Span Test"),
  
  useShinyFeedback(),
  numericInput("age", "Enter your Age", value = 19, min = 5, max = 100),
  
  radioButtons("sex", "Gender", choiceNames = c("Male", "Female"), choiceValues = c(0, 1)),
  
  selectInput("educat", "Education Qualification", choices = educat_choices, selected = 2),
  bsTooltip("educat", "Select the one you are currently pursuing. If not in Academia, select the one last completed."),
  
  selectInput("job", "Current Profession", choices = job_choices, selected = 0),
  bsTooltip("job", "Select Academia if you are Student/Professor/Teacher/Researcher"),
  
  sliderInput("academic", "Your Academic Performance", value = 3, min = 1, max = 5),
  bsTooltip("academic", "1 indicating bad and 5 indicating excellent"),
  
  radioButtons("maths", "Are you constantly in touch with Mathematics?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0), selected = 1),
  bsTooltip("maths", "Select yes, if your work or study heavily uses Mathematics"),
  
  radioButtons("music", "Do you regularly play any Musical Intrument?", choiceNames = c("Yes", "No"), choiceValues = c(1, 0), selected = 0),
  
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

# ------------------------ Concepts and Background UI

# ------------------------- Description & Explanation of the test and theory UI
SensDescTabUI <- tabPanel(
  "What is Sensory Memory?",
  value = "SensDesc",
  h1("What is Sensory Memory?", align = "center", style = "font-weight: bold"),
  p(
    "During every moment of an organism's life, sensory information is being taken in by sensory receptors and processed by the nervous system. Sensory information is stored in sensory memory just long enough to be transferred to short-term memory. Humans have five traditional senses: sight, hearing, taste, smell, touch. Sensory memory (SM) allows individuals to retain impressions of sensory information after the original stimulus has ceased. A common demonstration of SM is a child's ability to write letters and make circles by twirling a sparkler at night. When the sparkler is spun fast enough, it appears to leave a trail which forms a continuous image. This 'light trail' is the image that is represented in the visual sensory store known as iconic memory. The other two types of SM that have been most extensively studied are echoic memory, and haptic memory.",
    style = "font-size: 150%"
  ),
  p(
    "It is the first stage of the Modal Model.The SM do not process the information carried by the stimulus, but rather detect and hold that information for use in STM. For this reason Atkinson and Shiffrin also called the registers 'buffers', as they prevent immense amounts of information from overwhelming higher-level cognitive processes. Information is only transferred to the STM when attention is given to it, otherwise it decays rapidly and is forgotten.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Iconic Memory", style = "font-weight: bold"),
  p(
    "Iconic memory, which is associated with the visual system, is perhaps the most researched of the sensory registers.Iconic memory is only limited to field of vision. That is, as long as a stimulus has entered the field of vision there is no limit to the amount of visual information iconic memory can hold at any one time. As noted above, sensory registers do not allow for further processing of information, and as such iconic memory only holds information for visual stimuli such as shape, size, color and location (but not semantic meaning). As the higher-level processes are limited in their capacities, not all information from sensory memory can be conveyed. It has been argued that the momentary mental freezing of visual input allows for the selection of specific aspects which should be passed on for further memory processing. The biggest limitation of iconic memory is the rapid decay of the information stored there; items in iconic memory decay after only 0.5–1.0 seconds.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Echoic Memory", style = "font-weight: bold"),
  p(
    "Echoic memory refers to information that is registered by the auditory system. As with iconic memory, echoic memory only holds superficial aspects of sound (e.g. pitch, tempo, or rhythm) and it has a nearly limitless capacity. Echoic memory is generally have a a duration of between 1.5 and 5 seconds depending on context but has been shown to last up to 20 seconds in the absence of competing information.",
    style = "font-size: 150%"
  ),
  img(src='Sensory-Memory.jpg', align = "center")
)

STMDescTabUI <- tabPanel(
  "What is Short Term Memory?",
  value = "STMDesc",
  h1("What is Short Term Memory?", align = "center", style = "font-weight: bold"),
  p(
    "Short-Term Memory (STM) is the capacity for holding, but not manipulating, a small amount of information in mind in an active, readily available state for a short period of time. The purpose of the STM was to allow preliminary processing of information. Items held in the short-term memory decay rapidly over time; Atkinson and Shiffrin estimated that all trace of a word placed in STM will normally be lost within 30 seconds. For as long as an item resides in STM, however, there is a tendency to transfer it to the long-term memory(LTM); the longer an item is resident in STM, the greater the likelihood that a copy will be transferred to LTM. And once information is transferred to LTM, it is likely to be held there permanently.",
    style = "font-size: 150%"
  ),
  p(
    "The STM was itself fed by a series of sensory registers from sensory memory. These registers acted as a system for selecting and collating sensory information ormation, which could be viewed as an essential component of perception. It reflect faculties of the human mind that can hold a limited amount of information in a very accessible state temporarily. One might relate short-term memory to a pattern of neural firing that represents a particular idea and one might consider the idea to be in short-term memory only when the firing pattern, or cell assembly, is active.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Duration", style = "font-weight: bold"),
  p(
    "As with sensory memory, the information that enters short-term memory decays and is lost, but the information in the short-term store has a longer duration, approximately 18–20 seconds when the information is not being actively rehearsed, though it is possible that this depends on modality and could be as long as 30 seconds. Fortunately, the information can be held in the short-term store for much longer through what Atkinson and Shiffrin called rehearsal. For auditory information rehearsal can be taken in a literal sense: continually repeating the items. However, the term can be applied for any information that is attended to, such as when a visual image is intentionally held in mind. Finally, information in the short-term store does not have to be of the same modality as its sensory input. For example, written text which enters visually can be held as auditory information, and likewise auditory input can be visualized. On this model, rehearsal of information allows for it to be stored more permanently in the long-term store. Atkinson and Shiffrin discussed this at length for auditory and visual information but did not give much attention to the rehearsal/storage of other modalities due to the experimental difficulties of studying those modalities.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Capacity", style = "font-weight: bold"),
  p(
    "There is a limit to the amount of information that can be held in the STM: 7 ± 2 chunks. These chunks, which were noted by Miller in his seminal paper The Magical Number Seven, Plus or Minus Two, are defined as independent items of information. It is important to note that some chunks are perceived as one unit though they could be broken down into multiple items, for example '1066' can be either the series of four digits '1, 0, 6, 6' or the semantically grouped item '1066' which is the year the Battle of Hastings was fought. Chunking allows for large amounts of information to be held in memory: 149283141066 is twelve individual items, well outside the limit of the STM, but it can be grouped semantically into the 3 chunks [1492][8314][1066]. Because short-term memory is limited in capacity, it severely limits the amount of information that can be attended to at any one time.",
    style = "font-size: 150%"
  )
)

LTMDescTabUI <- tabPanel(
  "What is Long Term Memory?",
  value = "LTMDesc",
  h1("What is Long Term Memory?", align = "center", style = "font-weight: bold"),
  p(
    "Long-Term Memory(LTM) is is the stage of the Atkinson–Shiffrin memory model in which informative knowledge is held indefinitely. It is defined in contrast to short-term and working memory, which persist for only about 18 to 30 seconds. Long-term memory is commonly labelled as explicit memory (declarative), as well as episodic memory, semantic memory, autobiographical memory, and implicit memory.",
    style = "font-size: 150%"
  ),
  p(
    "The LTM is concerned with storing information over extensive periods of time and fed by a STM that acted as a controller, feeding in new information and selecting particular processes for pulling information out of the LTM. It is a vast store of knowledge and a record of prior events, and it exists according to all theoretical views; it would be difficult to deny that each normal person has at his or her command a rich, although not flawless or complete, set of long-term memories.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Transfer from STM", style = "font-weight: bold"),
  p(
    "Information is postulated to enter the LTM store from the STM more or less automatically. As Atkinson and Shiffrin model it, transfer from the STM to the LTM is occurring for as long as the information is being attended to in the STM. In this way, varying amounts of attention result in varying amounts of time in STM. Ostensibly, the longer an item is held in STM, the stronger its memory trace will be in LTM. Repeated rote repetition enhances LTM. Forgetting increases for items which are studied fewer times. There are stronger encoding processes than simple rote rehearsal, namely relating the new information to information which has already made its way into the LTM.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Capacity and Duration", style = "font-weight: bold"),
  p(
    "In this model, as with most models of memory, LTM is assumed to be nearly limitless in its duration and capacity. It is most often the case that brain structures begin to deteriorate and fail before any limit of learning is reached. This is not to assume that any item which is stored in LTM is accessible at any point in the lifetime. Rather, it is noted that the connections, cues, or associations to the memory deteriorate; the memory remains intact but unreachable.",
    style = "font-size: 150%"
  )
)

AtkShifTabUI <- tabPanel(
  "The Atkinson-Shiffrin Model",
  value = "AtkShif",
  h1("The Atkinson-Shiffrin Model", align = "center", style = "font-weight: bold"),
  p(
    "Two American Psychologists- Atkinson and Shiffrin suggested a three store structural model for memory. The first, called the sensory memory, where the inputs from the sensory organs is stored for a very small time in it's preliminary form before it is passed on to the next part, called short-term memory or STM. STM was assumed to be a temporary storage system that holds material just long enough for it to be processed; the capacity of this temporary store is very small. Once processing in this first store is completed, the coded material would be transferred to a more permanent store called long-term memory, or LTM. This model explained many of the memory related data so successfully that is soon became the modal model.",
    style = "font-size: 150%"
  ),
  img(src='Atkinson-and-Shiffrin-memory-model.png', align = "center")
)

DSTDescTabUI <- tabPanel(
  "What is a Digit Span Test?",
  value = "DSTDesc",
  h1("What is a Digit Span Test?", align = "center", style = "font-weight: bold"),
  p(
    "A digit-span task is used to measure STM's number storage capacity. Participants see or hear a sequence of numerical digits and are tasked to recall the sequence correctly, with increasingly longer sequences being tested in each trial. The participant's span is the longest number of sequential digits that can accurately be remembered. Digit-span tasks can be given forwards or backwards, meaning that once the sequence is presented, the participant is asked to either recall the sequence in normal or reverse order. Digit-span tasks are the most commonly used test for memory span, partially because performance on a digit-span task cannot be affected by factors such as semantics, frequency of appearance in daily life, complexity, etc",
    style = "font-size: 150%"
  ),
  p(
    "Verbal working memory is involved in many everyday tasks, such as remembering a friend's telephone number while entering it into a phone and understanding long and difficult sentences. Verbal working memory is also thought to be one of the elements underlying intelligence (often referred to as 'IQ,' meaning 'intelligence quotient'); thus, the digit span task is a common component of many IQ tests, including the widely used Wechsler Adult Intelligence Scale (WAIS). Performance on the digit span task is also closely linked to language learning abilities; improving verbal memory capacities may therefore aid mastery of a new language.",
    style = "font-size: 150%"
  ),
  p(
    "One of the earliest measures of STM was digit span, the longest sequence of numbers that can be immediately repeated back in the correct order. People vary in their span, but it is usually around seven digits or five random letters.",
    style = "font-size: 150%"
  ),
  br(),
  h3("Types of Digit Span Test", style = "font-weight: bold"),
  p("- Forward Digit Span Test:", style = "font-size:150%; font-weight:bold"),
  p("Here the subject is required to repeat the presented digit sequence in the given order", style = "font-size:150%"),
  p("- Backward Digit Span Test:", style = "font-size:150%; font-weight:bold"),
  p("It is an effective method in finding out the Short Term Memory capacity of subjects and the findings are reliable.", style = "font-size:150%")
)

DSTAdvTabUI <- tabPanel(
  "Advantages of Digit Span Test",
  value = "DSTAdv",
  h1("Advantages of Digit Span Test", align = "center", style = "font-weight: bold"),
  tags$div(tags$ul(
    tags$li(p("It is very simple test and doesn't require any special setup apart from a silent environment and test subject.", style = "font-size:150%")),
    tags$li(p("It serves as a part of assessing the IQ of the subjects. Higher values indicate higher remembering capacity.", style = "font-size:150%")),
    tags$li(p("It is an effective method in finding out the Short Term Memory capacity of subjects and the findings are reliable.", style = "font-size:150%")),
    tags$li(p("Computer and mobile versions of the test eliminate examiner differences and increase the inter-rater reliability.", style = "font-size:150%")),
    tags$li(p("The digit sequence shows a superiority effect when compared to any non-digit span, and the Digit Span test is a preferred method to measure one’s cognitive functioning.", style = "font-size:150%")),
  )
  )
)

DSTDisAdvTabUI <- tabPanel(
  "Disadvantages of Digit Span Test",
  value = "DSTDisAdv",
  h1("Disadvantages of Digit Span Test", align = "center", style = "font-weight: bold"),
  tags$div(tags$ul(
    tags$li(p("It is criticised for being artificial in nature.", style = "font-size:150%")),
    tags$li(p("This experiment is not the representative for the kinds of STM we do in everyday life.", style = "font-size:150%")),
    tags$li(p("It is an effective method in finding out the Short Term Memory capacity of subjects and the findings are reliable.", style = "font-size:150%")),
    tags$li(p("It lacks temporal validity i.e. findings may not generalize to modern times as it was devised almost a century ago.", style = "font-size:150%")),
  )
  )
)

DetailsNavListUI <- navlistPanel(
  id = "details",
  widths = c(2,10),
  "Memory Model",
  AtkShifTabUI,
  SensDescTabUI,
  STMDescTabUI,
  LTMDescTabUI,
  "Digit Span Test",
  DSTDescTabUI,
  DSTAdvTabUI,
  DSTDisAdvTabUI
  
)

ConptBgUI <- tabPanelBody(
  "theoryPanel",
  fluidPage(
    useShinydashboard(),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 11,
        DetailsNavListUI
      )
    )
  )
)

# ------------------------- Entire Test UI
EntireTestResultsUI <- tabPanelBody(
  "entireTestPanel",
  fluidPage(
    #theme = bs_theme(bootswatch = "darkly"),
    useShinydashboard(),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 11,
        h1("Test Results", align = "center", style = "font-weight: bold"),
        fluidRow(
          infoBoxOutput("NumOfSubjects"),
          infoBox("Number of Features Recorded", 16, icon = icon("list"), color = "blue", fill = TRUE),
          downloadButton("download", "Download All Data", style = "width: 30%; font-size: 200%")
        ),
        h2("Digit Span Score and Factoring Variables", align = "center", style = "font-weight: bold"),
        fluidRow(
          box(
            title = "Digit Span Score Distribution", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPDist")
          ),
          box(
            title = "Digit Span Score vs Age", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAge")
          ),
          box(
            title = "Digit Span Score vs Education Level", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsEducat")
          ),
          box(
            title = "Digit Span Score vs Gender", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsGender")
          ),
          box(
            title = "Digit Span Score vs Profession", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsJob")
          ),
          box(
            title = "Digit Span Score vs Academic Performance", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAcad")
          ),
          box(
            title = "Digit Span Score vs Maths", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsMaths")
          ),
          box(
            title = "Digit Span Score vs Music", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsMusic")
          ),
          box(
            title = "Digit Span Score vs Environment", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsEnv")
          ),
          box(
            title = "Digit Span Score vs Mean Click Time", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsMeanTime")
          )
        ),
        h2("Mean Click Times and Factoring Variables", align = "center", style = "font-weight: bold"),
        fluidRow(
          box(
            title = "Mean Click Time Difference vs Age", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsAge")
          ),
          box(
            title = "Mean Click Time Difference vs Education Level", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsEducat")
          ),
          box(
            title = "Mean Click Time Difference vs Gender", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsGender")
          ),
          box(
            title = "Mean Click Time Difference vs Profession", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsJob")
          ),
          box(
            title = "Mean Click Time Difference vs Academic Performance", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsAcad")
          ),
          box(
            title = "Mean Click Time Difference vs Maths", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsMaths")
          ),
          box(
            title = "Mean Click Time Difference vs Music", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsMusic")
          ),
          box(
            title = "Mean Click Time Difference vs Environment", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("MeanTimevsEnv")
          )
        ),
        h2("Variation of Digit Span with Age and other Factoring Variables", align = "center", style = "font-weight: bold"),
        fluidRow(
          box(
            title = "Digit Span Score vs Age vs Education Level", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsEducat")
          ),
          box(
            title = "Digit Span Score vs Age vs Gender", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsGender")
          ),
          box(
            title = "Digit Span Score vs Age vs Profession", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsJob")
          ),
          box(
            title = "Digit Span Score vs Age vs Academic Performance", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsAcad")
          ),
          box(
            title = "Digit Span Score vs Age vs Maths", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsMaths")
          ),
          box(
            title = "Digit Span Score vs Age vs Music", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsMusic")
          ),
          box(
            title = "Digit Span Score vs Age vs Environment", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
            plotOutput("DSPvsAgevsEnv")
          )
        ),
        h2("Interpretation", align = "center", style = "font-weight: bold"),
        box(
          title = "Interpretation", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          tags$div(tags$ul(
            tags$li(p("It is observed that with increase in age digit span decreases.", style = "font-size:150%")),
            tags$li(p("It is observed that with increase in education level digit span increases.", style = "font-size:150%")),
            tags$li(p("It is observed that those who play musical instrument or constantly in touch with Maths have higher digit span.", style = "font-size:150%")),
            tags$li(p("It is observed that those who attempted test in a silent environment have better digit span scores.", style = "font-size:150%")),
            tags$li(p("From individual results(available if you attempt the test on your own), it is observed the time taken between clicks is far less than the time taken near the starting and end of the digit. It may be explained in two ways- one is the person starts guessing and eventually speed increases but that does not justify higher end time. So it may be because when a person remembers a digit clearly he is more relaxed to click the button for that digit but when he doesn't then he quickly presses the buttons to make sure he doesn't forget it. So, it is also justified by the primacy and recency effects of the Modal Model.", style = "font-size:150%")),
            tags$li(p("Mean Click Time increases with age which can be interpreted as more time is required to retrieve the digits.", style = "font-size:150%")),
            tags$li(p("Mean Click Time decreases with increase in education level.", style = "font-size:150%")),
            tags$li(p("Mean Click Time is less for subjects who play musical instrument or are in constantly touch with Maths.", style = "font-size:150%")),
            tags$li(p("From individual results(available if you attempt the test on your own), it is observed the time taken between clicks is more for later rounds which maybe because in general larger digit sequence takes more time to be retrieved from memory which also makes it prone to forget some of the digits.", style = "font-size:150%")),
            tags$li(p("Higher Digit Span Subjects have slightly higher Mean Click Time i.e. on an average they can retrieve the digits even after more time has passed compared to the subjects with lower digit span.", style = "font-size:150%")))
          )
        )
      )
    )
  )
)

modal_entiretest_tab <- modalDialog(
  "No Pre-Collected Data Found",
  title = "Error!",
  footer = tagList(
    actionButton("nopredata", "GO BACK TO HOME", class = "btn btn-danger")
  )
)

# ------------------------- Intro UI
IntroUI <- tabPanelBody(
  "introPanel",
  h1("Digit Span Test", align = "center", style = "font-weight: bold"),
  h2("Psychological Test for testing short term memory capacity", align = "center"),
  h3(tags$a(href="https://github.com/RishiDarkDevil", "By Rishi Dey Chowdhury(RishiDarkDevil)", target = "_blank"), align = "center"),
  fluidRow(
    column(width = 1),
    column(
      width = 11,
      fluidRow(
        column(
          width = 10,
          h3("Rules(Read all or atleast the highlighted ones carefully):", style = "font-weight: bold")
        ),
        column(
          width = 2,
          #actionButton("theory", "Concepts & Background")
        )
      ),
      tags$div(tags$ul(
        tags$li(tags$span("Fill in the Details about 'YOUR INFO' on Left Side and Click 'Take Test' to begin.", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Numbers will be displayed one at a time(at equal intervals of 1.5 sec).", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("The Digit Pad will be disabled when number display is in progress.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("A thin progress bar showing how much number is displayed is visible on top of the screen throughout this process.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("The random number sequence to be remembered will increase by 1 after each successful completion of round.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("After all numbers are displayed 'GO' will be visible and Digit Pad is enabled. You can now start guessing the number in the correct order(Since, it is Forward Digit Span).", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("One Restart per round is available, which can be used only if no guess attempt(right or wrong) is made.", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("One chance for Mistake is available per round i.e. if you get a guess wrong then upon clicking the retry button another number sequence of same length will be displayed and you need to guess that.", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Click on the Right Arrow Button after a successful guess to move to the next round. ", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("As you guess a progress bar will indicate how much you guessed correctly.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("On successful guess restart button turns into a correct symbol and upon wrong guess turns into a cross symbol.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Continue till the maximum number of rounds you can go.", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Even if you feel you have exhausted one mistake in the round and you don't remember the retry digit sequence properly, guess as much as you remember", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Only after you reach the maximum round i.e. commit two mistakes in a round, you will be get access to 'Your Performance' assessment tab", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("'Your Performance' tab has comparison info with all the other test takers and visualizations regarding your performance", style = "font-size:18px"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("No Interface element except Digit Pad can be used once the test starts.", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("Ranking is decided first based on Digit Span Score(i.e. (max number of rounds)-1) then ties are broken based on the total time taken and average time difference between clicks", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;"),
        tags$li(tags$span("First Round is Trial", style = "font-size:18px; font-weight: bold"), style = "font-size: 36px; list-style-type: square;")))
    )
  )
)


# ------------------- Detect Mobile
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

modal_mobile <- modalDialog(
  "The webapp is not optimized for running on smaller screens. Please try using larger screen devices and Landscape Orientation.",
  title = "You are on Mobile!",
  footer = tagList(
    actionButton("mobile", "Continue", class = "btn btn-warning")
  )
)

# ------------------------- Test UI
TestUI <- tabPanelBody(
  value = "testPanel",
  fluidRow(
    column(width = 1),
    column(
      width = 11,
      splitLayout(
        cellWidths = c("60%", "40%"),
        span(textOutput("display_digit"), style = "font-size:2000%; text-align: center; vertical-align: middle", align = "center"),
        DigitPadUI
      )
    )
  )
)

# ------------------------ Results UI
ResultUI <- tabPanelBody(
  value = "perfPanel",
  useShinydashboard(),
  tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 60px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
  fluidRow(
    h1(textOutput("ResultHead"), align = "center", style = "font-weight: bold")
  ),
  fluidRow(
    column(width = 1),
    column(
      width = 11,
      fluidRow(
        valueBoxOutput("DSPScore", width = 3),
        valueBoxOutput("TotalTime", width = 3),
        # bsTooltip(""),
        valueBoxOutput("TotalMistakes", width = 3),
        valueBoxOutput("TotalRestarts", width = 3)
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("roundwisedata"),
          box(
            title = "Average Click Time in Each Round", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("time_each_round")
          )
        ),
        column(
          width = 6,
          box(
            title = "Click Times in Selected Round", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("time_this_round")
          ),
          box(
            title = "Your Position Insight", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
            infoBoxOutput("DSPRank", width = 6),
            infoBoxOutput("NumCompetitors", width = 6),
            infoBoxOutput("MeanTimeRankWithSameDSPRank", width = 6),
            infoBoxOutput("TotTimeRankWithSameDSPRank", width = 6)
          )
        )
      )
    )
  )
)

modal_performance_tab <- modalDialog(
  "Your digit span data captured. Click below to view performance assessment",
  title = "Test Complete",
  footer = tagList(
    actionButton("performance", "View Performance Assessment", class = "btn btn-success")
  )
)

#-------------------------------------------------- Main UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "DIGIT SPAN TEST", titleWidth = "15%",
    tags$li(
      class = "dropdown",
      actionButton("theory", "Concepts & Background"),
      bsTooltip("theory", "Gives Detailed information about the Digit Span Test and Memory Model."),
      actionButton("entireTestRes", "Results and Findings"),
      bsTooltip("entireTestRes", "Gives Visualization and Interpretation about already collected data."),
      actionButton("home", "Home"),
      bsTooltip("home", "Goes to Home Screen where all the rules are listed")
    )
  ),
  dashboardSidebar(
    tags$style(HTML(".main-sidebar{width: 15%;}")),
    UserDataUI
  ),
  dashboardBody(
    mobileDetect('isMobile'),
    use_waiter(),
    use_waitress(),
    tabsetPanel(
      id = "main",
      type = "hidden",
      selected = "introPanel",
      #selected = "perfPanel",
      ConptBgUI,
      EntireTestResultsUI,
      IntroUI,
      TestUI,
      ResultUI
    )
  )
)


#-------------------------------------------------- Main Server
server <- function(input, output, session) {
  
  showNotification("Kindly keep an eye on notifications..", duration = NULL, type = "message")
  
  # ------------ Deals with Mobile
  observe({
    if (input$isMobile) {
      showModal(modal_mobile)
    }
  })
  
  observeEvent(input$mobile, {
    removeModal()
  })
  #------------- Deals with Theory
  observeEvent(input$theory, {
    if (input$main != "testPanel") {
      updateTabsetPanel(inputId = "main", selected = "theoryPanel")
    }
  })
  
  #------------- Deals with Test Home Screen
  observeEvent(input$home, {
    if (input$main != "testPanel") {
      updateTabsetPanel(inputId = "main", selected = "introPanel")
    }
  })
  
  #------------- Deals with Entire Test Results Screen
  EntireTestResultsSetup <- function() {
    user_data_temp <- read_sheet(user_data_id, sheet = "main")
    user_dig_seq_temp <- read_sheet(user_dig_seq_id, sheet = "main")
    user_restart_wrong_temp <- read_sheet(user_restart_wrong_id, sheet = "main")
    user_digit_click_time_temp <- read_sheet(user_digit_click_time_id, sheet = "main")
    
    output$NumOfSubjects <- renderInfoBox({
      infoBox("Number of Subjects", nrow(user_data_temp), icon = icon("users"), fill = TRUE, color = "teal")
    })
    
    digit_span_per_ID <- user_dig_seq_temp %>%
      group_by(ID) %>%
      summarise(dig_span = max(parse_number(rounds))+1)
    user_data_dig_span <- left_join(user_data_temp, digit_span_per_ID, by = "ID")
    
    theme_include <- theme_bw() +
      theme(
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        legend.position = "none"
      ) 
    
    output$DSPDist <- renderPlot({
      t1 <- digit_span_per_ID %>%
        summarise('Mean' = mean(dig_span), 'Std' = sd(dig_span)) %>%
        mutate(across(where(is.numeric), ~round(., digits = 2)))
      
      digit_span_per_ID %>%
        mutate(dig_span = as.factor(dig_span)) %>%
        ggplot() +
        geom_bar(aes(dig_span, fill = dig_span)) +
        labs(
          x = "Digit Span Score",
          y = "Count"
        ) +
        annotate("text", x = Inf, y = Inf, label = paste("mean=",round(t1$Mean, 2),",sd=",round(t1$Std, 2)), hjust = 1, vjust = 1) +
        theme_include +
        scale_fill_viridis(discrete = TRUE)
    }, res = 96)
    
    output$DSPvsAge <- renderPlot({
      user_dig_span_age <- user_data_dig_span %>%
        mutate(age = as.factor(ifelse(age < 10, 1, ifelse(age < 20, 2, ifelse(age < 30, 3, ifelse(age < 40, 4, ifelse(age < 50, 5, ifelse(age < 60, 6, ifelse(age < 70, 7, 8)))))))))
      
      user_dig_span_age %>%
        group_by(age) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(age, mean_dig_span, fill = age), stat = "identity") +
        labs(
          x = "Age Groups",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Below 10","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "Above 70"), c(1, 2, 3, 4, 5, 6, 7, 8))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(age, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsEducat <- renderPlot({
      user_data_dig_span %>%
        mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
        group_by(educat) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(educat, mean_dig_span, fill = educat), stat = "identity") +
        labs(
          x = "Education Level",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("School-High School", "Undergraduate", "Graduate-Post Graduate"), c(1, 2, 3))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(educat, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsGender <- renderPlot({
      user_data_dig_span %>%
        mutate(sex = as.factor(sex)) %>%
        group_by(sex) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(sex, mean_dig_span, fill = sex), stat = "identity") +
        labs(
          x = "Gender",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Male", "Female"), c(0, 1))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(sex, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsJob <- renderPlot({
      user_data_dig_span %>%
        mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
        group_by(job) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(job, mean_dig_span, fill = job), stat = "identity") +
        labs(
          x = "Profession",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Academia", "Industry, Bussiness or Other"), c(1, 2))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(job, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsAcad <- renderPlot({
      user_data_dig_span %>%
        mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
        group_by(academic) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(academic, mean_dig_span, fill = academic), stat = "identity") +
        labs(
          x = "Academic Performance",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Below Average", "Average", "Above Average"), c(1, 2, 3))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(academic, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsMaths <- renderPlot({
      user_data_dig_span %>%
        mutate(maths = as.factor(maths)) %>%
        group_by(maths) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(maths, mean_dig_span, fill = maths), stat = "identity") +
        labs(
          x = "Have Maths?",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(maths, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsMusic <- renderPlot({
      user_data_dig_span %>%
        mutate(music = as.factor(music)) %>%
        group_by(music) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(music, mean_dig_span, fill = music), stat = "identity") +
        labs(
          x = "Play Musical Instrument?",
          y = "Average Digit Span Score"
        ) +
        theme_include +    
        scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(music, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsEnv <- renderPlot({
      user_data_dig_span %>%
        mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
        group_by(env) %>%
        summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span)) %>%
        ggplot() +
        geom_bar(aes(env, mean_dig_span, fill = env), stat = "identity") +
        labs(
          x = "Environment",
          y = "Average Digit Span Score"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Silent or Normal", "Noisy or Very Noisy"), c(1, 2))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(env, mean_dig_span, label=paste("mean=",round(mean_dig_span, 2),",sd=",round(sd_dig_span, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsMeanTime <- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(dig_span = dig_span) %>%
        group_by(dig_span) %>%
        summarise(mean_time_diff = mean(time_diff))
      user_data_dig_time
      
      user_data_dig_time %>%
        mutate(dig_span = as.factor(dig_span)) %>%
        ggplot() +
        geom_bar(aes(dig_span, mean_time_diff, fill = dig_span), stat = "identity") +
        labs(
          x = "Digit Span",
          y = "Mean Click Time"
        ) +
        theme_bw() +
        theme_include +
        scale_fill_viridis(discrete = TRUE)
    }, res = 96)
    
    output$MeanTimevsAge <- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(age = as.factor(ifelse(age < 10, 1, ifelse(age < 20, 2, ifelse(age < 30, 3, ifelse(age < 40, 4, ifelse(age < 50, 5, ifelse(age < 60, 6, ifelse(age < 70, 7, 8))))))))) %>%
        group_by(age) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(age, mean_time_diff, fill = age), stat = "identity") +
        labs(
          x = "Age",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Below 10","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "Above 70"), c(1, 2, 3, 4, 5, 6, 7, 8))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(age, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsEducat <- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
        group_by(educat) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(educat, mean_time_diff, fill = educat), stat = "identity") +
        labs(
          x = "Education Level",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("School-High School", "Undergraduate", "Graduate-Post Graduate"), c(1, 2, 3))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(educat, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsGender<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(sex = as.factor(sex)) %>%
        group_by(sex) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(sex, mean_time_diff, fill = sex), stat = "identity") +
        labs(
          x = "Gender",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Male", "Female"), c(0, 1))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(sex, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsJob<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
        group_by(job) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(job, mean_time_diff, fill = job), stat = "identity") +
        labs(
          x = "Profession",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Academia", "Industry, Bussiness or Other"), c(1, 2))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(job, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsAcad<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
        group_by(academic) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(academic, mean_time_diff, fill = academic), stat = "identity") +
        labs(
          x = "Academic Performance",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Below Average", "Average", "Above Average"), c(1, 2, 3))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(academic, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsMaths<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(maths = as.factor(maths)) %>%
        group_by(maths) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(maths, mean_time_diff, fill = maths), stat = "identity") +
        labs(
          x = "Have Maths?",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(maths, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsMusic<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(music = as.factor(music)) %>%
        group_by(music) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(music, mean_time_diff, fill = music), stat = "identity") +
        labs(
          x = "Play Musical Instrument?",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(music, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$MeanTimevsEnv<- renderPlot({
      user_data_dig_time <- full_join(user_digit_click_time_temp, user_data_dig_span)
      
      user_data_dig_time <- user_data_dig_time %>%
        mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
        group_by(env) %>%
        summarise(mean_time_diff = mean(time_diff), std_time_diff = sd(time_diff)) 
      
      user_data_dig_time %>%
        ggplot() +
        geom_bar(aes(env, mean_time_diff, fill = env), stat = "identity") +
        labs(
          x = "Environment",
          y = "Mean Time Diff btw Clicks"
        ) +
        theme_include +
        scale_x_discrete(labels = setNames(c("Silent or Normal", "Noisy or Very Noisy"), c(1, 2))) +
        scale_fill_viridis(discrete = TRUE) +
        geom_text(aes(env, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
    }, res = 96)
    
    output$DSPvsAgevsEducat <- renderPlot({
      user_data_dig_span %>%
        mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
        group_by(age, educat) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = educat), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "right") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("School-High School", "Undergraduate", "Graduate-Post Graduate"), c(1, 2, 3)))
    }, res = 96)
    
    output$DSPvsAgevsGender <- renderPlot({
      user_data_dig_span %>%
        mutate(sex = factor(sex)) %>%
        group_by(age, sex) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = sex), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "right") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Male", "Female"), c(0, 1)))
    }, res = 96)
    
    output$DSPvsAgevsJob <- renderPlot({
      user_data_dig_span %>%
        mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
        group_by(age, job) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = job), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score",
          color = "Profession"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "bottom") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Academia", "Industry, Bussiness or Other"), c(1, 2)))
    }, res = 96)
    
    output$DSPvsAgevsAcad <- renderPlot({
      user_data_dig_span %>%
        mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
        group_by(age, academic) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = academic), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "right") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Below Average", "Average", "Above Average"), c(1, 2, 3)))
    }, res = 96)
    
    output$DSPvsAgevsMaths <- renderPlot({
      user_data_dig_span %>%
        mutate(maths = as.factor(maths)) %>%
        group_by(age, maths) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = maths), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score",
          color = "Have Maths?"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "right") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Yes", "No"), c(1, 0)))
    }, res = 96)
    
    output$DSPvsAgevsMusic <- renderPlot({
      user_data_dig_span %>%
        mutate(music = as.factor(music)) %>%
        group_by(age, music) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = music), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score",
          color = "Play Musical Instrument?"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "right") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Yes", "No"), c(1, 0)))
    }, res = 96)
    
    output$DSPvsAgevsEnv <- renderPlot({
      user_data_dig_span %>%
        mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
        group_by(age, env) %>%
        summarise(mean_dig_span = mean(dig_span)) %>%
        ggplot() +
        geom_line(aes(age, mean_dig_span, color = env), size = 2) +
        labs(
          x = "Age",
          y = "Average Digit Span Score"
        ) +
        theme_bw() +
        theme_include + 
        theme(legend.position = "bottom") +
        scale_color_viridis(discrete = TRUE, labels = setNames(c("Silent or Normal", "Noisy or Very Noisy"), c(1, 2)))
    }, res = 96)
  }
  
  observeEvent(input$entireTestRes, {
    
    waiter <- Waiter$new(
      html = tagList(
        spin_fading_circles(),
        "Loading Data... Generating Plots..."
      )
    )
    waiter$show()
    on.exit(waiter$hide())
    if (filled_once()) {
      showNotification("Your Data is also added and used to generate plots.", type = "message")
    }
    
    if (input$main != "testPanel") {
      updateTabsetPanel(inputId = "main", selected = "entireTestPanel")
      if(nrow(read_sheet(user_data_id, sheet = "main")) != 0){
        EntireTestResultsSetup()
      } else {
        showModal(modal_entiretest_tab)
      }
    }
  })
  
  observeEvent(input$nopredata, {
    updateTabsetPanel(inputId = "main", selected = "introPanel")
    removeModal()
  })
  
  # Last ID
  last_ID <- 0
  append_to_prev <- FALSE
  index_wrong <- rep(0, 3*100)
  
  #------------- Deals with User Info
  filled_once <- reactiveVal(FALSE)
  curr_user <- reactiveVal(curr_user_data)
  
  observeEvent(input$start, {
    if (filled_once()) {
      updateTabsetPanel(inputId = "main", selected = "perfPanel")
      return()
    }
    
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
        "Get Ready.. First One is a Trial..."
      )
    )
    waiter$show()
    on.exit(waiter$hide())
    
    Sys.sleep(4)
    
    updateTabsetPanel(inputId = "main", selected = "testPanel")
    
    active(1)
    dig_seq(sample(0:9, no_of_digs(), replace = FALSE))
    #all_dig_seq()[round_num()] <- dig_seq()
    
    last_ID <<- 0
    append_to_prev <<- FALSE
    temp1 <- read_sheet(user_data_id, sheet = "main")
    if (nrow(temp1) != 0) {
      temp <- temp1
      last_ID <<- temp$ID[length(temp$ID)]
      append_to_prev <<- TRUE
    }
    
    curr_user(tibble(ID = last_ID+1, age = as.numeric(input$age), sex = as.numeric(input$sex), educat = as.numeric(input$educat), job = as.numeric(input$job), academic = as.numeric(input$academic), maths = as.numeric(input$maths), music = as.numeric(input$music), env = as.numeric(input$env)))
    filled_once(TRUE)
  })
  
  
  #------------- Deals with Conducting Test
  
  disp_dig <- reactiveVal("GO") # digit to be displayed
  active <- reactiveVal(4) # Random Number displayer iterator
  dig_seq <- reactiveVal(sample(0:9, 2, replace = FALSE)) # the number to be GOed
  no_of_digs <- reactiveVal(2) # no of digits in the deg seq
  traverse <- reactiveVal(1) # traverse the deg seq to match with the user input if correct or not
  wrong_times <- reactiveVal(0) # No of wrong button clicks -- We will allow upto 3 mistakes
  restart_times <- reactiveVal(0) # No of times restart button is clicked - Each round will allow 2 restarts
  last_try <- reactiveVal(TRUE) # Last try correct or wrong
  user_dig_seq <- reactiveVal(user_dig_seq) # Store digit sequence displayed
  user_restart_wrong <- reactiveVal(user_restart_wrong_data) # Store the number of times reset and wrong was clicked in each round
  user_digit_click_time <- reactiveVal(user_digit_click_time_data) # Stores the time between clicks of the digits
  prev_hit_time <- reactiveVal(Sys.time()) # Stores absolute time of when the prev button was clicked
  net_tot_time <- reactiveVal(0)
  net_mistakes <- reactiveVal(0)
  net_restarts <- reactiveVal(0)
  #round_num <- reactiveVal(1) # Round Number
  
  #last_hit_dig <- reactiveVal(-1) # what was the last clicked button label
  #last_hit_index <- reactiveVal(-1) # Stores the index in the deg seq where wrong button was clicked
  
  output$display_digit <- renderText({
    disp_dig()
  })
  
  #---------- Displaying Random Numbers and Diabling and enabling DigitPad
  observe({
    invalidateLater(1500, session)
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
          disp_dig("GO")
        }
      }
    })
  })
  
  observe({
    if (disp_dig() == "GO") {
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
    if (wrong_times() <= 1) {
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
    }
  }
  
  observeEvent(input$next_correct, {
    if ((((traverse()-1) == no_of_digs()) | (!last_try())) & (wrong_times() <= 1)) {
      next_round()
      restart_times(0)
    } else {
      if ((traverse()-1) < no_of_digs()) {
        showNotification("Can't Skip a Round!", type = "error")
      }
    }
  })
  
  # ---- Results Helper Function
  performancePanelSetup <- function(){
    
    output$DSPScore <- renderValueBox({
      valueBox((no_of_digs()-1), "Digit Span Score", icon = icon("trophy"), color = "teal")
    })
    output$TotalTime <- renderValueBox({
      valueBox(paste(round(net_tot_time(), 2), "secs"), "Total Time Taken", icon = icon("clock"), color = "olive")
    })
    output$TotalMistakes <- renderValueBox({
      valueBox(net_mistakes(), "Mistakes", icon = icon("times"), color = "red")
    })
    output$TotalRestarts <- renderValueBox({
      valueBox(net_restarts(), "Restarts", icon = icon("sync"), color = "maroon")
    })
    
    roundsdata <- tibble(
      rounds = user_dig_seq()$rounds,
      dig_seq = user_dig_seq()$dig_seq
    )
    
    roundstimedata <- user_digit_click_time() %>%
      group_by(rounds) %>%
      summarise(time_taken = sum(time_diff))
    
    roundsdata <- roundsdata %>%
      full_join(roundstimedata) %>%
      full_join(user_restart_wrong()) %>%
      mutate(rounds = paste("Round", parse_number(rounds)))
    
    roundstimedata <- roundstimedata %>%
      mutate(rounds = paste("Round", parse_number(rounds)))
    
    output$roundwisedata <- renderUI({
      tabs <- map(
        1:nrow(roundstimedata), function(i){
          tabPanel(
            title = roundstimedata$rounds[i], 
            value = str_replace_all(roundstimedata$rounds[i], fixed(" "), ""),
            infoBoxOutput(paste0("Sequence",i), width = 8),
            infoBoxOutput(paste0("Mistakes",i), width = 4),
            infoBoxOutput(paste0("Time", i), width = 8),
            infoBoxOutput(paste0("Restarts", i), width = 4)
          )
        }
      )
      args <- c(tabs, list(id = "tabrounds", title = "Rounds", height = "250px", width = 12))
      do.call(tabBox, args)
    })
    
    map(
      1:nrow(roundstimedata),
      function(k) {
        output[[paste0("Sequence", k)]] <- renderInfoBox({
          round_dig_seq <- roundsdata %>%
            filter(rounds == paste("Round", k))
          round_dig_seq <- unique(round_dig_seq$dig_seq)
          infoBox("Sequence", paste(round_dig_seq, collapse = ", "), icon = icon("list"), color = "green")
        })
        output[[paste0("Mistakes",k)]] <- renderInfoBox({
          round_mistakes <- user_restart_wrong() %>%
            filter((rounds == paste0("r", k)) & (variable == "n_wrongs"))
          round_mistakes <- round_mistakes$n_times
          infoBox("Mistakes", round_mistakes, icon = icon("times"), color = "red")
        })
        output[[paste0("Time",k)]] <- renderInfoBox({
          infoBox("Time", paste(round(roundstimedata$time_taken[k], 2), "secs"), icon = icon("clock"), color = "olive")
        })
        output[[paste0("Restarts",k)]] <- renderInfoBox({
          round_restarts <- user_restart_wrong() %>%
            filter((rounds == paste0("r", k)) & (variable == "n_restarts"))
          round_restarts <- round_restarts$n_times
          infoBox("Restarts", round_restarts, icon = icon("sync"), color = "maroon")
        })
      }
    )
    
    output$time_each_round <- renderPlot({
      avg_time_per_round <- user_digit_click_time() %>%
        group_by(rounds) %>%
        summarise(mean_time_diff = mean(time_diff))
      
      avg_time_per_round %>%
        mutate(rounds = parse_number(rounds)) %>%
        ggplot() +
        geom_line(aes(rounds, mean_time_diff), size = 2) +
        labs(
          x = "Rounds",
          y = "Average Time Taken"
        ) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank()
        ) 
    }, res = 96)
    
    user_data_temp <- read_sheet(user_data_id, sheet = "main")
    user_dig_seq_temp <- read_sheet(user_dig_seq_id, sheet = "main")
    user_digit_click_time_temp <- read_sheet(user_digit_click_time_id, sheet = "main")
    
    # Calculating DSP Ranking 
    digit_span_per_ID <- user_dig_seq_temp %>%
      group_by(ID) %>%
      summarise(dig_span = max(parse_number(rounds))+1)
    
    user_data_dig_span <- left_join(user_data_temp, digit_span_per_ID, by = "ID")
    
    user_data_dig_span <- user_data_dig_span %>%
      arrange(dig_span, age, educat, academic, maths, music, job, env) %>%
      add_column(ranking = 1:nrow(user_data_dig_span))
    
    curr_user_rank <- user_data_dig_span %>%
      filter(ID == (last_ID+1))
    
    rankpercent <- nrow(user_data_dig_span %>% filter(dig_span == (no_of_digs()-1)))*100 / nrow(user_data_dig_span)
    DSPranktext <- paste0(round(rankpercent, 2), "%")
    DSPranktext <- ifelse(round(rankpercent, 2) == 0.00, "1st Place", DSPranktext)
    
    # Calculating Mean Time Ranking with same DSP Rank
    avg_time_per_round_per_ID <- user_digit_click_time_temp %>%
      group_by(ID) %>%
      summarise(mean_time_diff = mean(time_diff))
    
    user_data_time <- full_join(full_join(user_data_temp, avg_time_per_round_per_ID, by = "ID"), user_data_dig_span)
    
    user_data_time <- user_data_time %>%
      select(-ranking) %>%
      filter(dig_span == (no_of_digs()-1)) %>%
      arrange(mean_time_diff, age, educat, academic, maths, music, job, env) %>%
      add_column(ranking = 1:nrow(user_data_time %>% filter(dig_span == (no_of_digs()-1))))
    
    curr_user_rank <- user_data_time %>%
      filter(ID == (last_ID+1))
    rankpercent <- ifelse(nrow(user_data_time) == 0, 0.00, (curr_user_rank$ranking)*100 / nrow(user_data_time))
    MeanTimeranktext <- paste0(ifelse(rankpercent <= 50, "Top ", "Bottom "), ifelse(rankpercent <= 50, round(rankpercent, 2), round(100-rankpercent)), "%")
    MeanTimeranktext <- ifelse((round(rankpercent, 2) == 0.00) | (nrow(user_data_time) == 1), "1st Place", MeanTimeranktext)
    
    # Calculating Total Time Ranking with same DSP Rank
    avg_time_per_round_per_ID <- user_digit_click_time_temp %>%
      group_by(ID) %>%
      summarise(mean_time_diff = sum(time_diff))
    
    user_data_time <- full_join(full_join(user_data_temp, avg_time_per_round_per_ID, by = "ID"), user_data_dig_span)
    
    user_data_time <- user_data_time %>%
      select(-ranking) %>%
      filter(dig_span == (no_of_digs()-1)) %>%
      arrange(mean_time_diff, age, educat, academic, maths, music, job, env) %>%
      add_column(ranking = 1:nrow(user_data_time %>% filter(dig_span == (no_of_digs()-1))))
    
    
    curr_user_rank <- user_data_time %>%
      filter(ID == (last_ID+1))
    rankpercent <- ifelse(nrow(user_data_time)==0, 0.00, (curr_user_rank$ranking)*100 / nrow(user_data_time))
    TotalTimeranktext <- paste0(ifelse(rankpercent <= 50, "Top ", "Bottom "), ifelse(rankpercent <= 50, round(rankpercent, 2), round(100-rankpercent)), "%")
    TotalTimeranktext <- ifelse((round(rankpercent, 2) == 0.00) | (nrow(user_data_time) == 1), "1st Place", TotalTimeranktext)
    
    # Calculating Net Ranking
    user_data_dig_span_time <- user_digit_click_time_temp %>%
      group_by(ID) %>%
      summarise(mean_time_diff = mean(time_diff), sum_time_diff = sum(time_diff)) 
    
    user_data_dig_span_time <- full_join(user_data_dig_span_time, user_data_dig_span)
    
    user_data_dig_span_time <- user_data_dig_span_time %>%
      group_by(dig_span) %>%
      arrange(dig_span, sum_time_diff, mean_time_diff, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(ranking = 1:nrow(user_data_dig_span_time))
    
    curr_user_rank <- user_data_dig_span_time %>%
      filter(ID == (last_ID+1))
    rankpercent <- (nrow(user_data_dig_span_time) - curr_user_rank$ranking + ifelse((nrow(user_data_dig_span_time) - curr_user_rank$ranking) != 0.00, 1, 0))*100 / nrow(user_data_dig_span_time)
    ranktext <- paste0(ifelse(rankpercent <= 50, "Top ", "Bottom "), ifelse(rankpercent <= 50, round(rankpercent, 2), round(100-rankpercent)), "%")
    ranktext <- ifelse(round(rankpercent, 2) == 0.00, "1st Place", ranktext)
    
    output$ResultHead <- renderText({
      paste("You are", ifelse(round(rankpercent, 2) == 0.00, "at", "in"), ranktext)
    })
    
    output$DSPRank <- renderInfoBox({
      infoBox("People with same Digit Span", DSPranktext, icon = icon("list"), fill = TRUE, color = "teal")
    })
    
    output$MeanTimeRankWithSameDSPRank <- renderInfoBox({
      infoBox("Mean Time Taken", MeanTimeranktext, "Among Same Digit Span People", icon = icon("clock"), fill = TRUE, color = "green")
    })
    
    output$TotTimeRankWithSameDSPRank <- renderInfoBox({
      infoBox("Total Time Taken", TotalTimeranktext, "Among Same Digit Span People", icon = icon("clock"), fill = TRUE, color = "olive")
    })
    
    output$NumCompetitors <- renderInfoBox({
      infoBox("Number of Competitors", nrow(user_data_temp), icon = icon("users"), fill = TRUE, color = "blue")
    })
    
  }
  
  output$time_this_round <- renderPlot({
    time_per_round<- user_digit_click_time() %>%
      filter(rounds == paste0("r",parse_number(input$tabrounds))) %>%
      group_by(try, clicks) %>%
      mutate(try = as.factor(try)) %>%
      summarise(mean_time_diff = mean(time_diff))
    
    time_per_round %>%
      mutate(clicks = parse_number(clicks)) %>%
      ggplot() +
      geom_line(aes(clicks, mean_time_diff, color = try), size = 2) +
      labs(
        x = "Clicks",
        y = "Time Taken",
        color = "Try"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank()
      ) +
      ggtitle(paste("Round", parse_number(input$tabrounds)))
  }, res = 96)
  
  #--- Restarting 
  observeEvent(input$restart, {
    # Restart Rules - Can restart 1 time in a round, Can restart only when all the numbers are displayed for a particular round
    # Can restart only if the user hasn't tried i.e. one cannot restart if he made a wrong guess
    # Restart is available only when the user hasn't tried anything
    if (restart_times() < 1 & disp_dig() == "GO" & last_try() & (traverse() == 1)) {
      next_round(TRUE)
      restart_times(restart_times()+1)
      if (no_of_digs() > 2){
        write_restart(restart_times())
      }
      showNotification("One restart used, No more restarts available in this round", type = "message")
    } else{
      if ((wrong_times() <= 1) & (restart_times() == 1)){
        showNotification("Only one restart per round is allowed!", type = "error")
      } else if ((wrong_times() <= 1) & (traverse() > 1) & ((traverse()-1) < no_of_digs()) & last_try()){
        showNotification("Can restart only if no guess is made!", type = "error")
      }
    }
  })
  
  # ------ Handling Modal after on Test Completion
  observeEvent(input$performance, {
    waiter <- Waiter$new(
      html = tagList(
        spin_fading_circles(),
        "Loading your performance...."
      )
    )
    waiter$show()
    on.exit(waiter$hide())
    
    output$start_ok <- renderText("")
    updateActionButton(inputId = "start", label = "Your Performance")
    updateTabsetPanel(inputId = "main", selected = "perfPanel")
    performancePanelSetup()
    removeModal()
    
  })
  
  # ---- Download Data
  output$download <- downloadHandler(
    filename = "DSPData.zip",
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      fs <- c("user_data.csv", "user_dig_seq.csv", "user_restart_wrong.csv", "user_digit_click_tiem.csv")
      withProgress(message = "Files getting ready for download", detail = "Zipping .csv files...", {
        write_csv(read_sheet(user_data_id, sheet = "main"), file = "user_data.csv")
        incProgress(1 / 4)
        write_csv(read_sheet(user_dig_seq_id, sheet = "main"), file = "user_dig_seq.csv")
        incProgress(1 / 4)
        write_csv(read_sheet(user_restart_wrong_id, sheet = "main"), file = "user_restart_wrong.csv")
        incProgress(1 / 4)
        write_csv(read_sheet(user_digit_click_time_id, sheet = "main"), file = "user_digit_click_time.csv")
        incProgress(1 / 4)
      })
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  #--- Handling User DigitPad Input after showing a Number
  check_dig_inp <- function(id, val) {
    observe({
      isolate({
        waitress <- Waitress$new(selector = "#display_digit", theme = "overlay-opacity",min = 0, max = no_of_digs())
        
        if (last_try() & (disp_dig() == "GO")) {
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
              waitress$inc(1)
              if ((traverse()-1) == no_of_digs()) {
                waitress$close()
                all_correct()
              }
            } else {
              curr_time <- Sys.time()
              if (no_of_digs() > 2) {
                temp <- user_digit_click_time()
                temp[(((no_of_digs()-3)*3) + (wrong_times()+1)), (traverse()+2)] <- as.numeric(difftime(curr_time, prev_hit_time(), units = "secs"))
                user_digit_click_time(temp)
                prev_hit_time(curr_time)
              }
              last_try(FALSE)
              if (no_of_digs() > 2) {
                write_dig_seq(dig_seq())
                write_wrong()
              }
              waitress$set(0)
              waitress$close()
              traverse(traverse()+1)
              wrong_times(wrong_times() + 1)
              if (wrong_times() <= 1) {
                wrong_input(id)
                showNotification("One Mistake Committed, No more chances for Mistake is available in this round", type = "message")
              } else {
                wrong_input(id, FALSE)
                
                showModal(modal_performance_tab)
                
                withProgress(message = "Adding your data to database...", {
                  # formatting the data lil bit before storing for less space usage and add ID to recognize particular user across all files
                  if (append_to_prev) {
                    sheet_append(data = curr_user(), ss = user_data_id, sheet = "main")
                  } else {
                    sheet_write(data = curr_user(), ss = user_data_id, sheet = "main")
                  }
                  
                  incProgress(1/4)
                  
                  temp <- user_dig_seq()
                  temp <- temp %>%
                    add_column(ID = c(last_ID+1, last_ID+1, last_ID+1)) %>%
                    select(ID, everything()) %>%
                    gather(paste0("r", 1:100), key = "rounds", value = "dig_seq") %>%
                    add_column(mis_ind = index_wrong) %>%
                    filter(dig_seq != "") %>%
                    arrange(parse_number(rounds), try)
                  
                  user_dig_seq(temp)
                  
                  if (append_to_prev) {
                    sheet_append(data = temp, ss = user_dig_seq_id, sheet = "main")
                  } else {
                    sheet_write(data = temp, ss = user_dig_seq_id, sheet = "main")
                  }
                  
                  incProgress(1/4)
                  
                  temp <- user_restart_wrong()
                  temp <- temp %>%
                    add_column(ID = c(last_ID+1, last_ID+1)) %>%
                    select(ID, everything()) %>%
                    gather(paste0("r", 1:100), key = "rounds", value = "n_times")
                  
                  tot_wrongs <- temp %>%
                    filter(variable == "n_wrongs") %>%
                    summarise(tot_wrongs = sum(n_times))
                  net_mistakes((tot_wrongs$tot_wrongs)[1])
                  
                  tot_restarts <- temp %>%
                    filter(variable == "n_restarts") %>%
                    summarise(tot_restarts = sum(n_times))
                  net_restarts((tot_restarts$tot_restarts)[1])
                  
                  user_restart_wrong(temp[1:(2*(no_of_digs()-2)),])
                  
                  if (append_to_prev) {
                    sheet_append(data = temp[1:(2*(no_of_digs()-2)),], ss = user_restart_wrong_id, sheet = "main")
                  } else {
                    sheet_write(data = temp[1:(2*(no_of_digs()-2)),], ss = user_restart_wrong_id, sheet = "main")
                  }
                  
                  incProgress(1/4)
                  
                  temp <- user_digit_click_time()
                  temp <- temp[1:(3*(no_of_digs()-2)), 1:(3*(no_of_digs()-2) + 2)] %>%
                    add_column(ID = (last_ID+1)) %>%
                    select(ID, rounds, everything()) %>%
                    gather(paste0("c", 1:(3*(no_of_digs()-2))), key = "clicks", value = "time_diff") %>%
                    arrange(parse_number(rounds), try) %>%
                    filter(time_diff != -1)
                  net_time <- temp %>%
                    mutate(rounds = parse_number(rounds)) %>%
                    filter(rounds != no_of_digs()) %>%
                    summarise(tot_time = sum(time_diff))
                  net_time <- (net_time$tot_time)[1]
                  net_tot_time(net_time)
                  
                  user_digit_click_time(temp)
                  
                  if (append_to_prev) {
                    sheet_append(data = temp, ss = user_digit_click_time_id, sheet = "main")
                  } else {
                    sheet_write(data = temp, ss = user_digit_click_time_id, sheet = "main")
                  }
                  
                  incProgress(1/4)
                })
                
                
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

