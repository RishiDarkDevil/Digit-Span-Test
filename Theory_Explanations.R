library(shiny)

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
  )
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
  )
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
  
)

ui <- fluidPage(
  DetailsNavListUI
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

