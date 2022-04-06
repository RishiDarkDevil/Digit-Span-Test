library(shiny)
library(bslib)
library(thematic)

ui <- fluidPage(
  #theme = bs_theme(bootswatch = "darkly"),
  useShinydashboard(),
  h1("Test Results", align = "center", style = "font-weight: bold"),
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
    )
  ),
  fluidRow(
    box(
      title = "Mean Click Time Difference vs Age", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsAge")
    ),
    box(
      title = "Mean Click Time Difference vs Education Level", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsEducat")
    ),
    box(
      title = "Mean Click Time Difference vs Gender", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsGender")
    ),
    box(
      title = "Mean Click Time Difference vs Profession", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsJob")
    ),
    box(
      title = "Mean Click Time Difference vs Academic Performance", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsAcad")
    ),
    box(
      title = "Mean Click Time Difference vs Maths", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsMaths")
    ),
    box(
      title = "Mean Click Time Difference vs Music", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsMusic")
    ),
    box(
      title = "Mean Click Time Difference vs Environment", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
      plotOutput("MeanTimevsEnv")
    )
  )
)

server <- function(input, output, session) {
  #thematic_shiny()
  
  digit_span_per_ID <- user_dig_seq %>%
    group_by(ID) %>%
    summarise(dig_span = max(parse_number(rounds))+1)
  user_data_dig_span <- left_join(user_data, digit_span_per_ID, by = "ID")
  
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
  
  output$MeanTimevsAge <- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
      scale_x_discrete(labels = setNames(c("Below 10","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "Above 70"), c(1, 2, 3, 4, 5, 6, 7, 8))) +
      scale_fill_viridis(discrete = TRUE) +
      geom_text(aes(educat, mean_time_diff, label=paste("mean=",round(mean_time_diff, 2),",sd=",round(std_time_diff, 2))), position=position_dodge(width=0.9), vjust=-0.25)
  })
  
  output$MeanTimevsGender<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
  
  output$MeanTimevsJob<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
  
  output$MeanTimevsAcad<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
  
  output$MeanTimevsMaths<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
  
  output$MeanTimevsMusic<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
  
  output$MeanTimevsEnv<- renderPlot({
    user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
    
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
  })
}

shinyApp(ui, server)

