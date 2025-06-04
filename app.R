#loading libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(readr)
library(tidyverse)
library(leaflet.extras)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Readintidyr# Reading the data file
data <- read_csv("data.csv")

# Apply readable labels for state and REC variables
state_labels <- c(
  "1" = "Illinois", "2" = "Indiana", "3" = "Iowa", "4" = "Kansas", "5" = "Michigan", "6" = "Minnesota",
  "7" = "Missouri", "8" = "Nebraska", "9" = "North Dakota", "10" = "Ohio", "11" = "South Dakota", "12" = "Wisconsin",
  "13" = "Connecticut", "14" = "Delaware", "15" = "District of Columbia", "16" = "Maine", "17" = "Maryland",
  "18" = "Massachusetts", "19" = "New Hampshire", "20" = "New Jersey", "21" = "New York", "22" = "Pennsylvania",
  "23" = "Rhode Island", "24" = "Vermont", "25" = "West Virginia", "26" = "Alabama", "27" = "Arkansas",
  "28" = "Florida", "29" = "Georgia", "30" = "Kentucky", "31" = "Louisiana", "32" = "Mississippi",
  "33" = "North Carolina", "34" = "Oklahoma", "35" = "South Carolina", "36" = "Tennessee", "37" = "Texas",
  "38" = "Virginia", "39" = "Other", "999" = "Refused"
)

rec1_labels <- c(
  "1" = "Within 0.5 mile",
  "2" = "Within 1 mile",
  "3" = "Within 3 miles",
  "4" = "More than 3 miles",
  "999" = "Refused"
)

rec2_labels <- c(
  "1" = "< 5 miles",
  "2" = "5–24 miles",
  "3" = "25–49 miles",
  "4" = "50–99 miles",
  "5" = "100+ miles",
  "666" = "Don't know",
  "999" = "Refused"
)

rec4_labels <- c(
  "1" = "Daily/Weekly",
  "2" = "Monthly",
  "3" = "Quarterly",
  "4" = "Yearly",
  "5" = "Not at all",
  "999" = "Refused"
)

rec5_labels <- c(
  "1" = "Within local community",
  "2" = "Within my state",
  "3" = "To a different state",
  "4" = "To another country",
  "5" = "Did not travel",
  "999" = "Refused"
)

rec6_labels <- c(
  "REC6_1" = "Solitude",
  "REC6_2" = "Relaxation",
  "REC6_3" = "Scenic Beauty",
  "REC6_4" = "Family/Friends",
  "REC6_5" = "Health",
  "REC6_6" = "Other",
  "REC6_999" = "Refused"
)

rec7_labels <- c(
  "1" = "Not important",
  "2" = "Somewhat important",
  "3" = "Neutral",
  "4" = "Important",
  "5" = "Very important",
  "999" = "Refused"
)

rec8_labels <- c(
  "REC8_1" = "Lack of Time",
  "REC8_2" = "Access Issues",
  "REC8_3" = "Cost",
  "REC8_4" = "Lack of Info",
  "REC8_5" = "Lack of Facilities",
  "REC8_6" = "Safety Concerns",
  "REC8_7" = "Bad Weather",
  "REC8_8" = "Health Conditions",
  "REC8_9" = "Other",
  "REC8_999" = "Refused"
)

rec9_labels <- c(
  "1" = "Yes",
  "0" = "No",
  "666" = "Don't Know",
  "999" = "Refused"
)

rec10_labels <- c(
  "1" = "No extent",
  "2" = "Some extent",
  "3" = "Great extent",
  "666" = "Don't know",
  "999" = "Refused"
)


# Recode columns using factor labels
data$state <- factor(state_labels[as.character(data$Q5)], levels = unique(state_labels))
data$dist_physical_activity <- factor(rec1_labels[as.character(data$dist_physical_activity)])
data$dist_anypark <- factor(rec2_labels[as.character(data$dist_anypark)])
data$OR_frequency <- factor(rec4_labels[as.character(data$OR_frequency)])
data$OR_travel <- factor(rec5_labels[as.character(data$OR_travel)])
data$OR_home_choice <- factor(rec7_labels[as.character(data$OR_home_choice)])
data$OR_com_programs <- factor(rec9_labels[as.character(data$OR_com_programs)])
data$OR_com_income <- factor(rec10_labels[as.character(data$OR_com_income)],
                     levels = c("No extent", "Some extent", "Great extent", "Don't know", "Refused"))

# Transform REC6 columns into long format
rec6_long <- data %>%
  select(contains("REC6_")) %>%
  pivot_longer(cols = everything(),
               names_to = "motivation_code",
               values_to = "selected") %>%
  filter(selected == 1) %>%
  count(motivation_code) %>%
  mutate(motivation = rec6_labels[motivation_code])

# Transform REC8 columns into long format
rec8_long <- data %>%
  select(contains("REC8_")) %>%
  pivot_longer(cols = everything(), names_to = "barrier_code", values_to = "selected") %>%
  filter(selected == 1) %>%
  count(barrier_code) %>%
  mutate(label = rec8_labels[barrier_code])

# helper function to summarize counts by category for REC7
summarize_rec7 <- function(df, state_label) {
  df %>%
    filter(!is.na(OR_home_choice) & OR_home_choice != "Refused") %>%
    count(OR_home_choice) %>%
    mutate(
      Percent = round(100 * n / sum(n), 1),
      State = state_label
    )
}

# Function to summarize REC9
summarize_rec9 <- function(df, state_label) {
  df %>%
    filter(!is.na(OR_com_programs) & OR_com_programs != "Refused") %>%
    count(OR_com_programs) %>%
    mutate(
      Percent = round(100 * n / sum(n), 1),
      State = state_label
    )
}

# Function to summarize REC10
summarize_rec10 <- function(df, state_label) {
  df %>%
    filter(!is.na(OR_com_income) & OR_com_income != "Refused") %>%
    count(OR_com_income) %>%
    mutate(
      Percent = round(100 * n / sum(n), 1),
      State = state_label
    )
}


# ---- HEADER ----
header <- dashboardHeader(title = "Outdoor Recreation Economy - North Central Region") 

# ---- SIDEBAR ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Participation & Access", tabName = "access", icon = icon("walking")),
    menuItem("Mobility", tabName = "mobility", icon = icon("car")),
    menuItem("Motivations & Barriers", tabName = "motivation", icon = icon("heart")),
    menuItem("Community Context", tabName = "context", icon = icon("users"))
  )
)

# ---- BODY ----
body <- dashboardBody(
  tabItems(
    
    # Tab 1: Participation & Access
    tabItem(tabName = "access",
            fluidRow(
              column(6,
                     box(title = "Select State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose State 1:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States"))
              ),
              column(6,
                     box(title = "Select State 2", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state_2", "Choose State 2:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States"))
              )
            ),
            fluidRow(
              column(6,
                     box(title = "Distance to Physical Activity", width = 12, plotOutput("rec1_plot")),
                     box(title = "Distance to Any Park", width = 12, plotOutput("rec2_plot")),
                     box(title = "Recreation Frequency", width = 12, plotOutput("rec4_plot"))
              ),
              column(6,
                     box(title = "Distance to Physical Activity (State 2)", width = 12, plotOutput("rec1_plot_2")),
                     box(title = "Distance to Any Park (State 2)", width = 12, plotOutput("rec2_plot_2")),
                     box(title = "Recreation Frequency (State 2)", width = 12, plotOutput("rec4_plot_2"))
              )
            )
    ),
    
    # Tab 2: Mobility
    tabItem(tabName = "mobility",
            fluidRow(
              column(6,
                     box(title = "Select State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose State 1:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States")))
            ),
            fluidRow(
              box(title = "Travel Distance (State 1)", width = 12, status = "warning", solidHeader = TRUE,
                  plotOutput("rec5_plot"))
            ),
            fluidRow(
              column(6,
                     box(title = "Select State 2", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state_2", "Choose State 2:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States")))
              
            ),
            fluidRow(
              box(title = "Travel Distance (State 2)", width = 12, status = "warning", solidHeader = TRUE,
                  plotOutput("rec5_plot_2"))
            )
    ),
    
    # Tab 3: Motivation & Barriers
    tabItem(tabName = "motivation",
            fluidRow(
              box(title = "Motivations for Outdoor Recreation (REC6)", width = 12, status = "success", solidHeader = TRUE,
                  plotOutput("rec6_plot", height = "300px"))
            ),
            fluidRow(
              box(title = "Barriers to Participation (REC8)", width = 12, status = "success", solidHeader = TRUE,
                  plotOutput("rec8_plot", height = "300px"))
            )
    ),
    
    # Tab 4: Community Context
    tabItem(tabName = "context",
            fluidRow(
              column(6,
                     box(title = "Select State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose State 1:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States"))
              ),
              column(6,
                     box(title = "Select State 2", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state_2", "Choose State 2:",
                                     choices = c("All States", sort(as.character(unique(data$state)))),
                                     selected = "All States"))
              )
            ),
            fluidRow(
              box(title = "Importance of Recreation for Residence Choice (REC7)", status = "info", solidHeader = TRUE,
                  width = 12,
                  plotOutput("rec7_plot", height = "450px"))
            ),
            fluidRow(
              box(title = "Does Community Support Outdoor Recreation? (REC9)", status = "info", solidHeader = TRUE,
                  width = 12,
                  plotOutput("rec9_plot", height = "450px"))
            ),
            fluidRow(
              box(title = "Community Reliance on Recreation Economy (REC10)", status = "info", solidHeader = TRUE,
                  width = 12,
                  plotOutput("rec10_plot", height = "450px"))
            )
    )
    
    
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  # Reactive for State 1
  filtered_data <- reactive({
    if (input$selected_state == "All States") {
      data
    } else {
      data %>% filter(state == input$selected_state)
    }
  })
  # Plots for State 1
  # Plot: Distance to local physical activity location (REC1)
  output$rec1_plot <- renderPlot({
    filtered_data() %>%
      count(dist_physical_activity) %>%
      ggplot(aes(x = dist_physical_activity, y = n)) +
      geom_bar(stat = "identity", fill = "#1f77b4") +
      labs(x = "Distance to Local Facility", y = "Respondents") +
      theme_minimal()
  })
  
  # Plot: Distance to state/national park (REC2)
  output$rec2_plot <- renderPlot({
    filtered_data() %>%
      count(dist_anypark) %>%
      ggplot(aes(x = dist_anypark, y = n)) +
      geom_bar(stat = "identity", fill = "#2ca02c") +
      labs(x = "Distance to State/National Park", y = "Respondents") +
      theme_minimal()
  })
  
  # Plot: Frequency of outdoor recreation (REC4)
  output$rec4_plot <- renderPlot({
    filtered_data() %>%
      count(OR_frequency) %>%
      ggplot(aes(x = OR_frequency, y = n)) +
      geom_bar(stat = "identity", fill = "#ff7f0e") +
      labs(x = "Outdoor Recreation Frequency", y = "Respondents") +
      theme_minimal()
  })
  
  # Reactive data filtered by selected state 2
  filtered_data_2 <- reactive({
    if (input$selected_state_2 == "All States") {
      data
    } else {
      data %>% filter(state == input$selected_state_2)
    }
  })
  
  # REC1: Distance to local physical activity location (State 2)
  output$rec1_plot_2 <- renderPlot({
    filtered_data_2() %>%
      count(dist_physical_activity) %>%
      ggplot(aes(x = dist_physical_activity, y = n)) +
      geom_bar(stat = "identity", fill = "#1f77b4") +
      labs(x = "Distance to Local Facility", y = "Respondents") +
      theme_minimal()
  })
  
  # REC2: Distance to state/national park (State 2)
  output$rec2_plot_2 <- renderPlot({
    filtered_data_2() %>%
      count(dist_anypark) %>%
      ggplot(aes(x = dist_anypark, y = n)) +
      geom_bar(stat = "identity", fill = "#2ca02c") +
      labs(x = "Distance to State/National Park", y = "Respondents") +
      theme_minimal()
  })
  
  # REC4: Outdoor recreation frequency (State 2)
  output$rec4_plot_2 <- renderPlot({
    filtered_data_2() %>%
      count(OR_frequency) %>%
      ggplot(aes(x = OR_frequency, y = n)) +
      geom_bar(stat = "identity", fill = "#ff7f0e") +
      labs(x = "Outdoor Recreation Frequency", y = "Respondents") +
      theme_minimal()
  })
  
  #REC_5 for state 1
  output$rec5_plot <- renderPlot({
    filtered_data() %>%
      count(OR_travel) %>%
      ggplot(aes(x = OR_travel, y = n)) +
      geom_bar(stat = "identity", fill = "#9467bd") +
      labs(x = "Travel Distance for Outdoor Recreation", y = "Respondents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #REC_5 for state 2
  output$rec5_plot_2 <- renderPlot({
    filtered_data_2() %>%
      count(OR_travel) %>%
      ggplot(aes(x = OR_travel, y = n)) +
      geom_bar(stat = "identity", fill = "#8c564b") +
      labs(x = "Travel Distance for Outdoor Recreation (State 2)", y = "Respondents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #REC_6
  output$rec6_plot <- renderPlot({
    wordcloud(
      words = rec6_long$motivation,
      freq = rec6_long$n,
      min.freq = 1,
      scale = c(3.5, 0.5),
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    )
  })
  
  #REC_8
  output$rec8_plot <- renderPlot({
    wordcloud(
      words = rec8_long$label,
      freq = rec8_long$n,
      min.freq = 1,
      scale = c(3.5, 0.5),
      random.order = FALSE,
      colors = brewer.pal(8, "Set2")
    )
  })
  
  #REC_7
  output$rec7_plot <- renderPlot({
    # Combine summaries
    state1 <- summarize_rec7(filtered_data(), input$selected_state)
    state2 <- summarize_rec7(filtered_data_2(), input$selected_state_2)
    combined <- bind_rows(state1, state2)
    
    ggplot(combined, aes(x = OR_home_choice, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Importance of Access to Outdoor Recreation (REC7)",
           x = "Importance Level", y = "Percentage", fill = "State") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  #REC_9
  output$rec9_plot <- renderPlot({
    state1 <- summarize_rec9(filtered_data(), input$selected_state)
    state2 <- summarize_rec9(filtered_data_2(), input$selected_state_2)
    combined <- bind_rows(state1, state2)
    
    ggplot(combined, aes(x = OR_com_programs, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Does Community Support Outdoor Recreation? (REC9)",
           x = "Response", y = "Percentage") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # REC10 Plot: Reliance on outdoor rec for income
  output$rec10_plot <- renderPlot({
    state1 <- summarize_rec10(filtered_data(), input$selected_state)
    state2 <- summarize_rec10(filtered_data_2(), input$selected_state_2)
    combined <- bind_rows(state1, state2)
    
    ggplot(combined, aes(x = OR_com_income, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Community Reliance on Recreation Economy (REC10)",
           x = "Perceived Extent", y = "Percentage") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


# ---- APP ----
shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)
