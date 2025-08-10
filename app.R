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
library(shinyBS)
library(plotly)
library(DT)
library(leaflet)
library(maps)
library(sf)

# Reading the data file
data <- read_csv("data_full.csv")
data <- data %>% slice(-1) 

# Apply readable labels for state and REC variables
state_labels <- c(
  "1" = "Illinois", "2" = "Indiana", "3" = "Iowa", "4" = "Kansas", "5" = "Michigan", "6" = "Minnesota",
  "7" = "Missouri", "8" = "Nebraska", "9" = "North Dakota", "10" = "Ohio", "11" = "South Dakota", "12" = "Wisconsin",
  "13" = "Connecticut", "14" = "Delaware", "15" = "Dist. of Columbia", "16" = "Maine", "17" = "Maryland",
  "18" = "Massachusetts", "19" = "New Hampshire", "20" = "New Jersey", "21" = "New York", "22" = "Pennsylvania",
  "23" = "Rhode Island", "24" = "Vermont", "25" = "West Virginia", "26" = "Alabama", "27" = "Arkansas",
  "28" = "Florida", "29" = "Georgia", "30" = "Kentucky", "31" = "Louisiana", "32" = "Mississippi",
  "33" = "North Carolina", "34" = "Oklahoma", "35" = "South Carolina", "36" = "Tennessee", "37" = "Texas",
  "38" = "Virginia", "39" = "Other", "999" = "Refused"
)

ncr_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan",
                "Minnesota", "Missouri", "Nebraska", "North Dakota",
                "Ohio", "South Dakota", "Wisconsin")

ner_states <- c("Connecticut", "Delaware", "Dist. of Columbia", "Maine",
                "Maryland", "Massachusetts", "New Hampshire", "New Jersey",
                "New York", "Pennsylvania", "Rhode Island", "Vermont", "West Virginia")

sr_states  <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
                "Louisiana", "Mississippi", "North Carolina", "Oklahoma",
                "South Carolina", "Tennessee", "Texas", "Virginia")

data <- data %>%
  filter(Q3 != "3") %>%  # Exclude non-binary respondents
  mutate(
    Q3 = as.character(Q3),  
    gender_label = case_when(
      Q3 == "1"   ~ "Man",
      Q3 == "2"   ~ "Woman",
      Q3 == "4"   ~ "Other",
      Q3 == "999" ~ "Refuse to answer",
      TRUE        ~ NA_character_
    )
  )


data <- data %>%
  mutate(
    Q6 = as.character(Q6),
    urban_rural = case_when(
      Q6 == "1"               ~ "Urban (city or town)",
      Q6 == "2"               ~ "Suburban (commuting distance)",
      Q6 == "3"               ~ "Rural (countryside)",
      Q6 == "999"             ~ "Refuse to answer",
      TRUE                    ~ NA_character_
    )
  )
income_map <- c(
  "1"   = "Less than $25,000",
  "2"   = "$25,000–$49,999",
  "3"   = "$50,000–$74,999",
  "4"   = "$75,000–$99,999",
  "5"   = "$100,000–$149,999",
  "6"   = "$150,000–$199,999",
  "7"   = "$200,000–$299,000",
  "8"   = "$300,000 and above",
  "666" = "Don't know",
  "999" = "Refuse to answer"
)

farm_map <- c(
  "1"   = "Yes",
  "0"   = "No",
  "999" = "Refuse to answer"
)

race_map <- c(
  "1" = "American Indian or Alaska Native",
  "2" = "Asian",
  "3" = "Black or African American",
  "4" = "Hispanic or Latino",
  "5" = "Middle Eastern or North African",
  "6" = "Native Hawaiian & Other Pacific Islander",
  "7" = "White",
  "8" = "Some other races",
  "999" = "Refuse to answer"
)
data <- data %>%
  mutate(
    race_label = case_when(
      # if exactly one of the keys above
      Q4 %in% names(race_map) ~ race_map[Q4],
      # if it’s a combination (contains a comma)
      str_detect(Q4, ",")     ~ "Multiracial",
      TRUE                    ~ NA_character_
    )
  )

edu_map <- c(
  "1"   = "Less than high school",
  "2"   = "High school diploma or equivalent (GED)",
  "3"   = "Career/technical/vocational school",
  "4"   = "Junior or community college",
  "5"   = "Bachelor’s degree (BA, BS, AB)",
  "6"   = "Graduate degree (master’s, professional, doctorate)",
  "999" = "Refuse to answer"
)

data <- data %>%
  mutate(
    education_label = case_when(
      # exact match in the map
      as.character(HD3) %in% names(edu_map) ~ edu_map[as.character(HD3)],
      # otherwise missing or invalid
      TRUE                                  ~ NA_character_
    )
  )

data <- data %>%
  mutate(
    HD2 = as.numeric(HD2)
  )

data <- data %>%
  mutate(
    HI2 = as.character(HI2),            
    income_label = income_map[HI2],
    HD8 = as.character(HD8),
    farm_label   = farm_map[HD8]
  )

rec1_labels <- c(
  "1" = "Within 0.5 mile",
  "2" = "Within 1 mile",
  "3" = "Within 3 miles",
  "4" = "More than 3 miles",
  "999" = "Refused"
)

rec1_vals <- c(
  "Within 0.5 mile"      = 0.5,
  "Within 1 mile"        = 1.0,
  "Within 3 miles"       = 3.0,
  "More than 3 miles"    = 5.0,   # or whatever “>3” you prefer
  "Refused"              = NA
)

rec2_labels <- c(
  "1" = "< 5 miles",
  "2" = "5–24 miles",
  "3" = "25–49 miles",
  "4" = "25–49 miles",
  "5" = "25–49 miles",
  "666" = "50–99 miles",
  "999" = "Refused"
)

rec3_labels <- c(
  "REC3_1"   = "Visited a State Park",
  "REC3_2"   = "Visited a Forest",
  "REC3_3"   = "Visited a National Park",
  "REC3_4"   = "Did not visit any",
  "REC3_666" = "Don’t know",
  "REC3_999" = "Refused"
)

rec4_labels <- c(
  "1" = "Daily/Weekly",
  "2" = "Monthly",
  "3" = "Quarterly",
  "4" = "Once a year",
  "5" = "Not at all",
  "999" = "Refused"
)

rec5_labels <- c(
  "1" = "Within local community",
  "2" = "Within my state",
  "3" = "To a different state",
  "4" = "To another country",
  "0" = "Did not travel",
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

QOL5_D_lables <- c(
  "1" = "Very unsatisfied",
  "2" = "Unsatisfied",
  "3" = "Neutral",
  "4" = "Satisfied",
  "5" = "Very satisfied",
  "999" = "Refused"

)


# Recode columns using factor labels
data$state <- factor(state_labels[as.character(data$Q5)], levels = unique(state_labels))
data <- data %>%
  mutate(
    region = case_when(
      state %in% ncr_states        ~ "North Central Region",
      state %in% ner_states        ~ "North Eastern Region",
      state %in% sr_states         ~ "Southern Region",
      TRUE                         ~ NA_character_
    )
  )
data$region <- factor(data$region, levels = c("North Central Region", "North Eastern Region", "Southern Region"))
data$income_label <- factor(data$income_label, levels = income_map)
data$farm_label   <- factor(data$farm_label,   levels = farm_map)
data$dist_physical_activity <- factor(
  rec1_labels[as.character(data$REC1)],
  levels = c(
    "Within 0.5 mile",
    "Within 1 mile",
    "Within 3 miles",
    "More than 3 miles",
    "Refused"
  )
)

data$dist_anypark <- factor(
  rec2_labels[as.character(data$REC2)],
  levels = c("< 5 miles", "5–24 miles", "25–49 miles", "50–99 miles", "Refused")
)
data$OR_frequency <- factor(
  rec4_labels[as.character(data$REC4)],
  levels = c("Daily/Weekly", "Monthly", "Quarterly", "Once a year", "Not at all", "Refused")
)
data$OR_travel <- factor(
  rec5_labels[as.character(data$REC5)],
  levels = c(
    "Within local community",
    "Within my state",
    "To a different state",
    "To another country",
    "Did not travel",
    "Refused"
  )
)
data$OR_home_choice <- factor(
  rec7_labels[as.character(data$REC7)],
  levels = c(
    "Not important",
    "Somewhat important",
    "Neutral",
    "Important",
    "Very important",
    "Refused"
  )
)
data$OR_com_programs <- factor(
  rec9_labels[as.character(data$REC9)],
  levels = c("Yes", "No", "Don't Know", "Refused")
)
data$OR_com_income <- factor(rec10_labels[as.character(data$REC10)],
                     levels = c("No extent", "Some extent", "Great extent", "Don't know", "Refused"))
data$outdoor_spaces_sat <- factor(
  QOL5_D_lables[as.character(data$QOL5_D)],
  levels = c(
    "Very unsatisfied",
    "Unsatisfied",
    "Neutral",
    "Satisfied",
    "Very satisfied",
    "Refused"
  )
)

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

# Function to summarize outdoor_spaces_sat
summarize_QOL5_D<- function(df, state_label) {
  df %>%
    filter(!is.na(outdoor_spaces_sat) & outdoor_spaces_sat != "Refused") %>%
    count(outdoor_spaces_sat) %>%
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
header <- dashboardHeader(title = "Outdoor Recreation Economy - North Central Region"
                          )
                        


# ---- SIDEBAR ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
    menuItem("Access",        tabName = "access",        icon = icon("map-marker-alt")),
    menuItem("Participation", tabName = "participation", icon = icon("walking")),
    menuItem("Mobility", tabName = "mobility", icon = icon("car")),
    menuItem("Motivations & Barriers", tabName = "motivation", icon = icon("heart")),
    menuItem("Community Context", tabName = "context", icon = icon("users"))
  ),
  # footer container pinned to the bottom
  tags$div(
    style = "
      position: absolute;
      bottom: 0;
      width: 100%;
      padding: 10px;
      text-align: center;
      border-top:2px solid #005f73;
      background: #0a9396;
    ",
    tags$a(
      href   = "https://github.com/prayash106/NCRCD_dashboard",
      target = "_blank",
      style  = "display: block; margin-bottom: 5px;",
      tagList(icon("github"), " GitHub")
    ),
    
    tags$a(
      href   = "qianx@umn.edu",
      icon("envelope"), " Contact Us",
      style  = "display: block;"
    ) 
  )  
)


# ---- BODY ----
body <- dashboardBody(
  tabItems(
    
    # Tab 0: Introduction
    # Tab 0: Introduction
    tabItem(
      tabName = "introduction",
      fluidRow(
        box(
          title       = "Welcome to the Outdoor Recreation Dashboard",
          width       = 12,
          status      = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          tags$div(
            tags$p(
              "This interactive dashboard summarizes results from the outdoor recreation module of the 2024 NCR-Stat: Baseline Survey, 
        a multi-state effort to understand household, business, and community well-being. 
        The survey was conducted across 12 North Central states, with comparable data collected in selected Northeastern and Southern states, 
        enabling regional comparisons on recreation access, behavior, motivations, and community context."
            ),
            
            tags$h4("📊 About the Survey"),
            tags$ul(
              tags$li(tags$b("Target Population:"), " Adults aged 18 and older living in participating U.S. states."),
              tags$li(tags$b("Methodology:"), " Online questionnaire fielded in 2024, designed to collect baseline data on outdoor recreation alongside broader household and community indicators."),
              tags$li(tags$b("Sample Size:"), " Over 14,000 respondents, stratified by state and key demographics to ensure representativeness.")
            ),
            
            tags$h4("📁 Survey Codebook"),
            tags$p(
              "The full survey instrument, including question wording and variable definitions, 
        is available in the official ",
              tags$a(href = "https://ncrcrd.ag.purdue.edu/ncr-stat/ncr-stat-baseline-2024/", 
                     target = "_blank", 
                     "NCR-Stat: Baseline Survey 2024 website"),
              "."
            ),
            
            tags$h4("📌 Dashboard Structure"),
            tags$ul(
              tags$li(tags$b("Access:"), " How close residents live to parks and physical activity facilities ?"),
              tags$li(tags$b("Participation:"), " Which facilities people visit and how often ?"),
              tags$li(tags$b("Mobility:"), " Travel distances typically covered for recreation."),
              tags$li(tags$b("Motivations & Barriers:"), " Reasons for participating—or not—in outdoor activities."),
              tags$li(tags$b("Community Context:"), " Perceptions of neighborhood support, importance, and satisfaction.")
            ),
            
            tags$h4("🧭 How to Use the Dashboard"),
            tags$p("Use the navigation tabs to explore various aspects of outdoor recreation."),
            tags$p("Each tab features interactive charts and tables that allow you to filter data by state, demographic group, and other relevant variables."),
            tags$p("At the top of each tab, you’ll find a collapsible 'About the Chart' section that, when expanded, provides details about the related survey question."),
            tags$p("Apply filters at the top of each tab to view results by gender, race/ethnicity, age group, income level, location, or farming status."),
            tags$p("Use the chart type selector to switch between bar, pie, or donut charts."),
            tags$p("Toggle between a single-region view or a comparison view to examine two states or regions side by side."),
            tags$p("For interactive maps, hover over a state to view exact percentages. You can also download your preferred plot type."),
            
            
            tags$h4("🙌 Acknowledgments"),
            tags$p(
              "The 2024 NCR-Stat: Baseline Survey was coordinated by the ",
              "North Central Regional Center for Rural Development",
              ", in collaboration with the ",
              "Northeast Regional Center for Rural Development",
              " and the ",
              "Southern Rural Development Center",
              "."
            ),
            tags$p(
              "Funding for this dashboard was provided by the ",
              tags$a(
                href = "https://extension.umn.edu/community-development/tourism",
                target = "_blank",
                "University of Minnesota Tourism Center"
              ),
              "."
            )
          )
        )
      )
    ),
    
    
      tabItem(tabName = "participation",
            ## About this chart ##
            fluidRow(
              box(
                title       = "About the charts",
                width       = 12,
                status      = "success",
                solidHeader = TRUE,
                
                # make it collapsible
                collapsible = TRUE,
                collapsed   = TRUE,
                
                p("Question (Visited Parks): Did you visit a state park, forest, or national park in the last 12 months?"),
                p("Question (Recreation Frequency):In the last 12 months, how often did you participate in outdoor recreation activities?"),
                p("Use the controls below to select which regions/states to compare, choose your preferred plot type, and optionally apply demographic filters.")
              )
            ),
            ##Demographic filters ##
            fluidRow(
              box(
                title       = tagList(icon("filter", class = "fa-2x"), "Demographic Filters"),
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                collapsible = TRUE,
                collapsed   = TRUE,   
                # Gender
                selectInput("demo_gender", "Gender",
                            choices = c("All", unique(data$gender_label)),
                            selected = "All"),
                # Race / ethnicity
                selectInput("demo_race", "Race/Ethnicity",
                            choices = c("All", sort(unique(na.omit(data$race_label)))),
                            selected = "All"),
                # Urban / rural
                selectInput("demo_urban", "Residential Location",
                            choices = c("All", unique(data$urban_rural)),
                            selected = "All"),
                # Age
                sliderInput("demo_age", "Age in years",
                            min   = min(data$HD2, na.rm = TRUE),
                            max   = max(data$HD2, na.rm = TRUE),
                            value = c(min(data$HD2, na.rm = TRUE),
                                      max(data$HD2, na.rm = TRUE))),
                #Income
                selectInput("demo_income", "2023 Household Income",
                            choices = c("All", levels(data$income_label)),
                            selected = "All"
                ),
                
                #Farm
                selectInput("demo_farm", "Do you farm?",
                            choices = c("All", levels(data$farm_label)),
                            selected = "All"
                )
                
              )
            ),
            # Plot type selector
            fluidRow(
              box(
                width = 12, solidHeader = TRUE, status = "warning",
                selectInput(
                  "part_plot_type", "Choose plot type:",
                  choices = c("Bar chart" = "bar", "Pie chart" = "pie", "Donut chart" = "donut"),
                  selected = "bar"
                )
              )
            ),
            
            # Toggle: Single vs Compare
            fluidRow(
              column(12,
                     box(width = 12, solidHeader = TRUE, status = "warning",
                         radioButtons("view_mode", "Comparison Mode",
                                      choices = c("Compare Two Regions/States", "Single Region/State View"),
                                      selected = "Compare Two Regions/States",
                                      inline = TRUE
                         )
                     )
              )
            ),
            
            # State selection UI
            fluidRow(
              column(6,
                     box(title = "Select Region 1 or State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose Region or State:",
                                     choices = list(
                                       "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                       "States"  = sort(unique(data$state))
                                     ),
                                     selected = "North Central Region"
                         )
                     )
              ),
              column(6,
                     conditionalPanel(
                       condition = "input.view_mode == 'Compare Two Regions/States'",
                       box(title = "Select Region 2 or State 2", width = 12, solidHeader = TRUE, status = "primary",
                           selectInput("selected_state_2", "Choose Region or State :",
                                       choices = list(
                                         "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                         "States"  = sort(unique(data$state))
                                       ),
                                       selected = "North Central Region"
                           )
                       )
                     )
              )
            ),
            
            # Plot layout using consistent columns
            fluidRow(
              column(6,
                     box(title = "Visited Parks", width = 12, plotOutput("rec3_plot")),
                     box(title = "Recreation Frequency", width = 12, plotOutput("rec4_plot"))
              ),
              column(6,
                     conditionalPanel(
                       condition = "input.view_mode == 'Compare Two Regions/States'",
                       box(title = "Visited Parks (Comparison Region/State)", width = 12, plotOutput("rec3_plot_2")),
                       box(title = "Recreation Frequency (Comparison Region/State)", width = 12, plotOutput("rec4_plot_2"))
                     )
              )
            ),
            
            #top-3 states
            fluidRow(
              column(6,
                     box(
                       title = "Top-5 States with the most 'Visited a State Park'response",
                       status = "primary", solidHeader = TRUE, width = 12,
                       DT::DTOutput("top3_rec3")
                     )
              ),
              column(6,
                     box(
                       title = "Top-5 States with the most 'Daily/Weekly' response ",
                       status = "primary", solidHeader = TRUE, width = 12,
                       DT::DTOutput("top3_rec4")
                     )
              )
            )
            
            
            ),
    
    # Tab 2: Access
    tabItem(tabName = "access",
            ## About this chart ##
            fluidRow(
              box(
                title       = "About the charts",
                width       = 12,
                status      = "success",
                solidHeader = TRUE,
                
                # make it collapsible
                collapsible = TRUE,
                collapsed   = TRUE,
                
                p("Question (Distance to Physical Activity): How far is the nearest location for physical activity (park or recreational facility) from your home?"),
                p("Question (Distance to Any Park): How far do you live from any state park, forest, or national park?"),
                p("Use the controls below to select which states to compare, choose your preferred plot type, and optionally apply demographic filters.")
              )
            ),
            ##Demographic filters ##
            fluidRow(
              box(
                title       = tagList(icon("filter", class = "fa-2x"), "Demographic Filters"),
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                collapsible = TRUE,
                collapsed   = TRUE,   
                # Gender
                selectInput("demo_gender", "Gender",
                            choices = c("All", unique(data$gender_label)),
                            selected = "All"),
                # Race / ethnicity
                selectInput("demo_race", "Race/Ethnicity",
                            choices = c("All", sort(unique(na.omit(data$race_label)))),
                            selected = "All"),
                # Urban / rural
                selectInput("demo_urban", "Residential Location",
                            choices = c("All", unique(data$urban_rural)),
                            selected = "All"),
                # Age
                sliderInput("demo_age", "Age in years",
                            min   = min(data$HD2, na.rm = TRUE),
                            max   = max(data$HD2, na.rm = TRUE),
                            value = c(min(data$HD2, na.rm = TRUE),
                                      max(data$HD2, na.rm = TRUE))),
                #Income
                selectInput("demo_income", "2023 Household Income",
                            choices = c("All", levels(data$income_label)),
                            selected = "All"
                ),
                
                #Farm
                selectInput("demo_farm", "Do you farm?",
                            choices = c("All", levels(data$farm_label)),
                            selected = "All"
                )
              )
            ),
            fluidRow(
              valueBoxOutput("avg_distance", width = 4),
              valueBoxOutput("min_distance_state", width = 4),
              valueBoxOutput("max_distance_state", width = 4)
            ),
            fluidRow(
              box(
                width = 12, solidHeader = TRUE, status = "warning",
                selectInput(
                  "access_plot_type", "Choose plot type:",
                  choices = c("Pie chart" = "pie", "Bar chart" = "bar", "Donut chart" = "donut"),
                  selected = "pie"
                )
              )
            ),
            fluidRow(
              column(12,
                     box(width = 12, solidHeader = TRUE, status = "warning",
                         radioButtons("view_mode", "Comparison Mode",
                                      choices = c("Compare Two Regions/States", "Single Region/State View"),
                                      selected = "Compare Two Regions/States",
                                      inline = TRUE)
                     )
              )
            ),
            fluidRow(
              column(6,
                     box(title = "Select Region 1 or State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose Region or State:",
                                     choices = list(
                                       "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                       "States"  = sort(unique(data$state))
                                     ),
                                     selected = "North Central Region"))
              ),
              column(6,
                     conditionalPanel(
                       condition = "input.view_mode == 'Compare Two Regions/States'",
                       box(title = "Select Region 2 or State 2", width = 12, solidHeader = TRUE, status = "primary",
                           selectInput("selected_state_2", "Choose Region or State:",
                                       choices = list(
                                         "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                         "States"  = sort(unique(data$state))
                                       ),
                                       selected = "North Central Region"))
                     )
              )
            ),
            fluidRow(
              column(6,
                     box(title = "Distance to Physical Activity", width = 12, plotOutput("rec1_plot")),
                     box(title = "Distance to Any Park", width = 12, plotOutput("rec2_plot"))
              ),
              conditionalPanel(
                condition = "input.view_mode == 'Compare Two Regions/States'",
                column(6,
                       box(title = "Distance to Physical Activity (Comparison State)", width = 12, plotOutput("rec1_plot_2")),
                       box(title = "Distance to Any Park (Comparison State)", width = 12, plotOutput("rec2_plot_2"))
                )
              )
              
            )
    ),
    
    # Tab 3: Mobility
    tabItem(tabName = "mobility",
            ## About this chart ##
            fluidRow(
              box(
                title       = "About the charts",
                width       = 12,
                status      = "success",
                solidHeader = TRUE,
                
                # make it collapsible
                collapsible = TRUE,
                collapsed   = TRUE,
                
                p("Question (Travel Distance to outdoor recreation): In the last 12 months, how far did you typically travel to enjoy outdoor recreation activities?"),
                p("Use the controls below to select which states to compare, choose your preferred plot type, and optionally apply demographic filters.")
              )
            ),
            ##Demographic filters ##
            fluidRow(
              box(
                title       = tagList(icon("filter", class = "fa-2x"), "Demographic Filters"),
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                collapsible = TRUE,
                collapsed   = TRUE,   
                # Gender
                selectInput("demo_gender", "Gender",
                            choices = c("All", unique(data$gender_label)),
                            selected = "All"),
                # Race / ethnicity
                selectInput("demo_race", "Race/Ethnicity",
                            choices = c("All", sort(unique(na.omit(data$race_label)))),
                            selected = "All"),
                # Urban / rural
                selectInput("demo_urban", "Residential Location",
                            choices = c("All", unique(data$urban_rural)),
                            selected = "All"),
                # Age
                sliderInput("demo_age", "Age in years",
                            min   = min(data$HD2, na.rm = TRUE),
                            max   = max(data$HD2, na.rm = TRUE),
                            value = c(min(data$HD2, na.rm = TRUE),
                                      max(data$HD2, na.rm = TRUE))),
                #Income
                selectInput("demo_income", "2023 Household Income",
                            choices = c("All", levels(data$income_label)),
                            selected = "All"
                ),
                
                #Farm
                selectInput("demo_farm", "Do you farm?",
                            choices = c("All", levels(data$farm_label)),
                            selected = "All"
                )
              )
            ),
            fluidRow(
              box(
                width = 12, solidHeader = TRUE, status = "warning",
                selectInput(
                  "mob_plot_type", "Choose plot type:",
                  choices = c( "Donut chart" = "donut", "Bar chart" = "bar", "Pie chart" = "pie"),
                  selected = "donut"
                )
              )
            ),
            fluidRow(
              column(12,
                     box(width = 12, solidHeader = TRUE, status = "warning",
                         radioButtons("view_mode", "Comparison Mode",
                                      choices = c("Compare Two Regions/States", "Single Region/State View"),
                                      selected = "Compare Two Regions/States",
                                      inline = TRUE)
                     )
              )
            ),
            fluidRow(
              column(6,
                     box(
                       width = 12, solidHeader = TRUE, status = "primary",
                       title = "Travel Distance to outdoor recreation",
                       selectInput(
                         "selected_state", "Choose Region or State:",
                         choices = list(
                           "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                           "States"  = sort(unique(data$state))
                         ),
                         selected = "North Central Region"
                       ),
                       plotOutput("rec5_plot", height = "350px")
                     )
              ),
              column(6,
                     conditionalPanel(
                       condition = "input.view_mode == 'Compare Two Regions/States'",
                       box(
                         title = "Travel Distance (Comparison State)", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state_2", "Choose Region 2 or State 2:",
                                     choices = list(
                                       "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                       "States"  = sort(unique(data$state))
                                     ),
                                     selected = "North Central Region"
                         ),
                         plotOutput("rec5_plot_2", height = "350px")
                       )
                     )
              )
            ),
            fluidRow(
              column(4,
                     box(
                       title = "Top-5 States with the most “Within local community” responses",
                       status = "primary", solidHeader = TRUE, width = 12,
                       DT::DTOutput("top5_local", height = "220px")
                     )
              ),
              column(4,
                     box(
                       title = "Top-5 States with the most “Within my state” responses",
                       status = "primary", solidHeader = TRUE, width = 12,
                       DT::DTOutput("top5_instate", height = "220px")
                     )
              ),
              column(4,
                     box(
                       title = "Top-5 States with the most “To a different state” responses",
                       status = "primary", solidHeader = TRUE, width = 12,
                       DT::DTOutput("top5_diff", height = "220px")
                     )
              )
            )
    ),
    
    # Tab 4: Motivation & Barriers
    tabItem(tabName = "motivation",
            ## About this chart ##
            fluidRow(
              box(
                title       = "About the charts",
                width       = 12,
                status      = "success",
                solidHeader = TRUE,
                
                # make it collapsible
                collapsible = TRUE,
                collapsed   = TRUE,
                
                p("Question (Motivation for outdoor recreation): What are your primary motivations for participating in outdoor recreation activities?"),
                p("Question (Barriers to Participation): What challenges or barriers have you faced in participating in outdoor recreation activities? "),
                p("Use the controls below to select which states to compare, choose your preferred plot type, and optionally apply demographic filters.")
              )
            ),
            ##Demographic filters ##
            fluidRow(
              box(
                title       = tagList(icon("filter", class = "fa-2x"), "Demographic Filters"),
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                collapsible = TRUE,
                collapsed   = TRUE,   
                # Gender
                selectInput("demo_gender", "Gender",
                            choices = c("All", unique(data$gender_label)),
                            selected = "All"),
                # Race / ethnicity
                selectInput("demo_race", "Race/Ethnicity",
                            choices = c("All", sort(unique(na.omit(data$race_label)))),
                            selected = "All"),
                # Urban / rural
                selectInput("demo_urban", "Residential Location",
                            choices = c("All", unique(data$urban_rural)),
                            selected = "All"),
                # Age
                sliderInput("demo_age", "Age in years",
                            min   = min(data$HD2, na.rm = TRUE),
                            max   = max(data$HD2, na.rm = TRUE),
                            value = c(min(data$HD2, na.rm = TRUE),
                                      max(data$HD2, na.rm = TRUE))),
                #Income
                selectInput("demo_income", "2023 Household Income",
                            choices = c("All", levels(data$income_label)),
                            selected = "All"
                ),
                
                #Farm
                selectInput("demo_farm", "Do you farm?",
                            choices = c("All", levels(data$farm_label)),
                            selected = "All"
                )
              )
            ),
            fluidRow(
              column(4,
                     box(title = "Select Region or State", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("selected_state", "Choose Region or State:",
                                     choices = list(
                                       "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                       "States"  = sort(unique(data$state))
                                     ),
                                     selected = "North Central Region"))
              ),
              column(4,
                     valueBoxOutput("top_motivation", width = 12)
              ),
              column(4,
                     valueBoxOutput("top_barrier", width = 12)
              )
            ),
            fluidRow(
              box(title = "Motivations for Outdoor Recreation", width = 12, status = "success", solidHeader = TRUE,
                  plotlyOutput("rec6_plot", height = "300px"))
            ),
            fluidRow(
              box(title = "Barriers to Participation", width = 12, status = "success", solidHeader = TRUE,
                  plotlyOutput("rec8_plot", height = "300px"))
            ),
            fluidRow(
              box(
                title = "Statewide Motivation & Barrier Counts — hover over a state to view top motivation or barrier responses.",
                width = 12, solidHeader = TRUE, status = "primary",
                leafletOutput("state_mot_bar_map", height = "400px")
              )
            )
    ),
    
    # Tab 5: Community Context
    tabItem(tabName = "context",
            ## About this chart ##
            fluidRow(
              box(
                title       = "About the charts",
                width       = 12,
                status      = "success",
                solidHeader = TRUE,
                
                # make it collapsible
                collapsible = TRUE,
                collapsed   = TRUE,
                
                p("Question (Importance of Recreation Access): How important is access to outdoor recreation areas to your choice of residence?"),
                p("Question (Community support for Recreation): Does your community implement any policies or programs to support outdoor recreation?"),
                p("Question (Community reliance on Recreation): To what extent does your community rely on outdoor recreation activities for income?"),
                p("Question (Satisfaction with outdoor space): How satisfied are you with Outdoor recreational spaces and facilities (public parks, trails, lakes, etc.)?"),
                p("Use the controls below to select which states to compare, choose your preferred plot type, and optionally apply demographic filters.")
              )
            ),
            ##Demographic filters ##
            fluidRow(
              box(
                title       = tagList(icon("filter", class = "fa-2x"), "Demographic Filters"),
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                collapsible = TRUE,
                collapsed   = TRUE,   
                # Gender
                selectInput("demo_gender", "Gender",
                            choices = c("All", unique(data$gender_label)),
                            selected = "All"),
                # Race / ethnicity
                selectInput("demo_race", "Race/Ethnicity",
                            choices = c("All", sort(unique(na.omit(data$race_label)))),
                            selected = "All"),
                # Urban / rural
                selectInput("demo_urban", "Residential Location",
                            choices = c("All", unique(data$urban_rural)),
                            selected = "All"),
                # Age
                sliderInput("demo_age", "Age in years",
                            min   = min(data$HD2, na.rm = TRUE),
                            max   = max(data$HD2, na.rm = TRUE),
                            value = c(min(data$HD2, na.rm = TRUE),
                                      max(data$HD2, na.rm = TRUE))),
                #Income
                selectInput("demo_income", "2023 Household Income",
                            choices = c("All", levels(data$income_label)),
                            selected = "All"
                ),
                
                #Farm
                selectInput("demo_farm", "Do you farm?",
                            choices = c("All", levels(data$farm_label)),
                            selected = "All"
                )
              )
            ),
            fluidRow(
              column(12,
                     box(width = 12, solidHeader = TRUE, status = "warning",
                         radioButtons("view_mode", "Comparison Mode",
                                      choices = c("Compare Two Regions/States", "Single Region/State View"),
                                      selected = "Compare Two Region/States",
                                      inline = TRUE)
                     )
              )
            ),
            fluidRow(
              column(6,
                     box(title = "Select Region 1 or State 1", width = 12, solidHeader = TRUE, status = "primary",
                         selectInput("ctx_state1", "Choose Region or State:",
                                     choices = list(
                                       "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                       "States"  = sort(unique(data$state))
                                     ),
                                     selected = "North Central Region")
                     )
              ),
              column(6,
                     conditionalPanel(
                       condition = "input.view_mode == 'Compare Two Regions/States'",
                       box(title = "Select Region 2 or State 2", width = 12, solidHeader = TRUE, status = "primary",
                           selectInput("ctx_state2", "Choose Region 2 or State 2:",
                                       choices = list(
                                         "Regions" = c("North Central Region", "North Eastern Region", "Southern Region"),
                                         "States"  = sort(unique(data$state))
                                       ),
                                       selected = "North Central Region")
                       )
                     )
              )
            ),
            fluidRow(
              box(title = "Importance of Recreation Access", status = "info", solidHeader = TRUE,
                  width = 12, plotlyOutput("rec7_plot", height = "450px")
              )
            ),
            fluidRow(
              box(title = "Does Community Support Outdoor Recreation?", status = "info", solidHeader = TRUE,
                  width = 12, plotlyOutput("rec9_plot", height = "450px")
              )
            ),
            fluidRow(
              box(title = "Community Reliance on Recreation Economy ", status = "info", solidHeader = TRUE,
                  width = 12, plotlyOutput("rec10_plot", height = "450px")
              )
            ),
            fluidRow(
              box(title = "Satisfaction with outdoor spaces", status = "info", solidHeader = TRUE,
                  width = 12, plotlyOutput("QOL5_D_plot", height = "450px")
              )
            ),
            
            # State Summary Card
            fluidRow(
              column(12,
                     box(title = "State Summary Card", width = 12, solidHeader = TRUE, status = "info",
                         uiOutput("state_summary_card")
                     )
              )
            )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive for State 1
  filtered_data <- reactive({
    df <- data
    
    # Gender
    if (input$demo_gender != "All") {
      df <- df %>% filter(gender_label == input$demo_gender)
    }
    # Race / ethnicity
    if (input$demo_race != "All") {
      df <- df %>% filter(race_label == input$demo_race)
    }
    # Urban / rural
    if (input$demo_urban != "All") {
      df <- df %>% filter(urban_rural == input$demo_urban)
    }
    # Age
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    # Income
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    #Farm
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    
    # Now state‐level filtering
    sel1 <- input$selected_state
    if (sel1 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
      df <- df %>% filter(region == sel1)
    } else {
      df <- df %>% filter(state == sel1)
    }
    
    df
  })
  
  # Plots for State 1
  state_rec1_summary <- reactive({
    data %>%
      filter(!is.na(dist_physical_activity)) %>%
      mutate(dist_num = rec1_vals[as.character(dist_physical_activity)]) %>%
      group_by(state) %>%
      summarize(
        avg_dist = mean(dist_num, na.rm = TRUE),
        max_dist = max(dist_num, na.rm = TRUE),
        min_dist = min(dist_num, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # overall average across all states
  output$avg_distance <- renderValueBox({
    avg_all <- mean(state_rec1_summary()$avg_dist, na.rm = TRUE)
    valueBox(
      sprintf("%.2f mi", avg_all),
      "Avg. Nearest Location for Physical Activity",
      icon  = icon("map-marker-alt"),
      color = "purple"
    )
  })
  
  # state with the largest average distance
  output$max_distance_state <- renderValueBox({
    top <- state_rec1_summary() %>% slice_max(avg_dist, n = 1)
    valueBox(
      top$state,
      paste0("Maximum distance for Physical Activity: ", sprintf("%.2f mi", top$avg_dist)),
      icon  = icon("arrow-down"),
      color = "red"
    )
  })
  
  # state with the smallest average distance
  output$min_distance_state <- renderValueBox({
    bot <- state_rec1_summary() %>% slice_min(avg_dist, n = 1)
    valueBox(
      bot$state,
      paste0("Minimum distance for Physical Activity: ", sprintf("%.2f mi", bot$avg_dist)),
      icon  = icon("arrow-up"),
      color = "green"
    )
  })
  
  # Reactive summaries for the two selected states, without slider
  rec1_bin_summary <- reactive({
    # use your existing filtered_data() and filtered_data_2()
    pct_half <- function(df) {
      if (nrow(df) == 0) return(NA_real_)
      round(
        nrow(df %>% filter(dist_physical_activity == "Within 0.5 mile")) /
          nrow(df) * 100,
        1
      )
    }
    
    list(
      pct1 = pct_half(filtered_data()),
      pct2 = pct_half(filtered_data_2())
    )
  })
  
  # Render the narrative
  output$access_narrative <- renderUI({
    summs <- rec1_bin_summary()
    HTML(sprintf(
      "<p>In <strong>%s</strong>, <strong>%.1f%%</strong> live within half a mile of a facility; in <strong>%s</strong>, only <strong>%.1f%%</strong> do.</p>",
      input$selected_state,   summs$pct1,
      input$selected_state_2, summs$pct2
    ))
  })
  
  
  # Plot: Distance to local physical activity location (REC1)
  output$rec1_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(dist_physical_activity), dist_physical_activity != "Refused") %>%
      count(dist_physical_activity) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$access_plot_type,
                  
                  # === Bar Chart ===
                  "bar" = ggplot(df, aes(x = dist_physical_activity, y = Percent)) +
                    geom_col(fill = "#fb5607") + 
                    labs(x = "Distance to Facility", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie Chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = dist_physical_activity)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut Chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = dist_physical_activity)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add larger percentage labels
    plt + geom_text(
      data = df,
      aes(
        label = paste0(round(Percent, 1), "%"),
        x     = if (input$access_plot_type == "bar") dist_physical_activity else 1,
        y     = Percent
      ),
      size     = 5,  # Increase label size
      position = position_stack(vjust = 0.5)
    )
  })
  
  
  # Plot: Distance to state/national park (REC2)
  output$rec2_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(dist_anypark), dist_anypark != "Refused") %>%
      count(dist_anypark) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$access_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = dist_anypark, y = Percent)) +
                    geom_col(fill = "#fb5607") + 
                    labs(x = "Distance to State/National Park", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = dist_anypark)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = dist_anypark)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percentage labels
    plt + geom_text(
      data = df,
      aes(
        label = paste0(round(Percent, 1), "%"),
        x     = if (input$access_plot_type == "bar") dist_anypark else 1,
        y     = Percent
      ),
      size     = 5,
      position = position_stack(vjust = 0.5)
    )
  })
  
  
  # Plot: Visited parks (REC3)
  output$rec3_plot <- renderPlot({
    df <- filtered_data() %>%
      select(starts_with("REC3_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "selected") %>%
      filter(selected == 1, code != "REC3_999") %>%
      count(code) %>%
      mutate(
        label   = rec3_labels[code],
        Percent = n / sum(n) * 100
      ) %>%
      arrange(desc(n)) %>%
      mutate(label = factor(label, levels = unique(label)))
    
    plt <- switch(input$part_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = label, y = Percent)) +
                    geom_col(fill = "#4CAF50") +
                    labs(x = "Visited in Last 12 Months", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Visit Type") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Visit Type") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percentage labels
    if (input$part_plot_type == "bar") {
      plt +
        geom_text(aes(label = paste0(round(Percent, 1), "%")),
                  vjust = 1, size = 5)
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  
  # Plot: Frequency of outdoor recreation (REC4)
  output$rec4_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(OR_frequency), OR_frequency != "Refused") %>%
      count(OR_frequency) %>%
      mutate(
        label   = OR_frequency,
        Percent = n / sum(n) * 100
      ) %>%
      arrange(desc(n)) %>%
      mutate(label = factor(label, levels = unique(label)))
    
    plt <- switch(input$part_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = label, y = Percent)) +
                    geom_col(fill = "#ff7f0e") +
                    labs(x = "Outdoor Recreation Frequency", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Frequency") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Frequency") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    if (input$part_plot_type == "bar") {
      plt +
        geom_text(aes(label = paste0(round(Percent, 1), "%")),
                  vjust = 1, size = 5)
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  
  # State Park
  output$top3_rec3 <- DT::renderDT({
    data %>%
      group_by(state) %>%
      summarise(share = sum(REC3_1 == 1, na.rm = TRUE) / n()) %>%
      arrange(desc(share)) %>%
      slice_head(n = 5) %>%
      mutate(share = scales::percent(share, accuracy = 0.1)) %>%
      select(state, share) %>%
      DT::datatable(options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  output$top3_rec4 <- DT::renderDT({
    data %>%
      filter(!is.na(REC4)) %>%
      group_by(state) %>%
      summarise(share = mean(REC4 == "1", na.rm = TRUE)) %>%
      arrange(desc(share)) %>%
      slice_head(n = 5) %>%
      mutate(share = scales::percent(share, accuracy = 0.1)) %>%
      select(state, share) %>%
      DT::datatable(options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  
  
  
  # Reactive data filtered by selected state 
  filtered_data_2<- reactive({
    df <- data
    # apply same demo filters
    if (input$demo_gender != "All")   df <- df %>% filter(gender_label == input$demo_gender)
    if (input$demo_race != "All")     df <- df %>% filter(race_label == input$demo_race)
    if (input$demo_urban != "All")    df <- df %>% filter(urban_rural == input$demo_urban)
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    
    # then state 2
    if (input$view_mode == "Compare Two Regions/States") {
      sel2 <- input$selected_state_2
      if (sel2 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
        df <- df %>% filter(region == sel2)
      } else {
        df <- df %>% filter(state == sel2)
      }
    }
    
    df
  })
  
  
  # REC1: Distance to local physical activity location (State 2)
  output$rec1_plot_2 <- renderPlot({
    df <- filtered_data_2() %>%
      filter(!is.na(dist_physical_activity), dist_physical_activity != "Refused") %>%
      count(dist_physical_activity) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$access_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = dist_physical_activity, y = Percent)) +
                    geom_col(fill = "#B54C4C") +
                    labs(x = "Distance to Facility", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = dist_physical_activity)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = dist_physical_activity)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percentage labels
    if (input$access_plot_type == "bar") {
      plt +
        geom_text(
          aes(label = paste0(round(Percent, 1), "%")),
          vjust = 1, size = 5
        )
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  # REC2: Distance to state/national park (State 2)
  output$rec2_plot_2 <- renderPlot({
    df <- filtered_data_2() %>%
      filter(!is.na(dist_anypark), dist_anypark != "Refused") %>%
      count(dist_anypark) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$access_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = dist_anypark, y = Percent)) +
                    geom_col(fill = "#B54C4C") +
                    labs(x = "Distance to State/National Park", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = dist_anypark)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = dist_anypark)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    if (input$access_plot_type == "bar") {
      plt +
        geom_text(
          aes(label = paste0(round(Percent, 1), "%")),
          vjust = 1,
          size = 5
        )
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  # Rec 3: Visited parks (State 2)
  output$rec3_plot_2 <- renderPlot({
    df <- filtered_data_2() %>%
      select(starts_with("REC3_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "selected") %>%
      filter(selected == 1, code != "REC3_999") %>%
      count(code) %>%
      mutate(
        label   = rec3_labels[code],
        Percent = n / sum(n) * 100
      ) %>%
      arrange(desc(n)) %>%
      mutate(label = factor(label, levels = unique(label)))
    
    plt <- switch(input$part_plot_type,
                  
                  # === Bar chart ===
                  "bar" = ggplot(df, aes(x = label, y = Percent)) +
                    geom_col(fill = "#4CAF50") +
                    labs(x = "Visited in Last 12 Months", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Visit Type") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Visit Type") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percentage text
    if (input$part_plot_type == "bar") {
      plt +
        geom_text(
          aes(label = paste0(round(Percent, 1), "%")),
          vjust = 1, size = 5
        )
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  output$rec4_plot_2 <- renderPlot({
    df <- filtered_data_2() %>%
      filter(!is.na(OR_frequency), OR_frequency != "Refused") %>%
      count(OR_frequency) %>%
      mutate(
        label   = OR_frequency,
        Percent = n / sum(n) * 100
      ) %>%
      arrange(desc(n)) %>%
      mutate(label = factor(label, levels = unique(label)))
    
    plt <- switch(input$part_plot_type,
                  
                  # === Bar Chart ===
                  "bar" = ggplot(df, aes(x = label, y = Percent)) +
                    geom_col(fill = "#ff7f0e") +
                    labs(x = "Outdoor Recreation Frequency", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie Chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Frequency") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut Chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = label)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Frequency") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percent labels
    if (input$part_plot_type == "bar") {
      plt +
        geom_text(
          aes(label = paste0(round(Percent, 1), "%")),
          vjust = 1,
          size = 5
        )
    } else {
      plt +
        geom_text(
          data = df,
          aes(label = paste0(round(Percent, 1), "%")),
          position = position_stack(vjust = 0.5),
          size = 5
        )
    }
  })
  
  
  # A) Reactive that first applies demographics, *then* state filter:
  filtered_mob <- reactive({
    df <- data
    
    # Gender
    if (input$demo_gender != "All") {
      df <- df %>% filter(gender_label == input$demo_gender)
    }
    # Race / ethnicity
    if (input$demo_race != "All") {
      df <- df %>% filter(race_label == input$demo_race)
    }
    # Urban / rural
    if (input$demo_urban != "All") {
      df <- df %>% filter(urban_rural == input$demo_urban)
    }
    # Age
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    # Income
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    #Farm
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    
    # Now state‐level filtering
    sel1 <- input$selected_state
    if (sel1 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
      df <- df %>% filter(region == sel1)
    } else {
      df <- df %>% filter(state == sel1)
    }
    
    df
  })
  
  # B) And for the second state in compare mode:
  filtered_mob2 <- reactive({
    df <- data
    # apply same demo filters
    if (input$demo_gender != "All")   df <- df %>% filter(gender_label == input$demo_gender)
    if (input$demo_race != "All")     df <- df %>% filter(race_label == input$demo_race)
    if (input$demo_urban != "All")    df <- df %>% filter(urban_rural == input$demo_urban)
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    # Income
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    #Farm
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    # then state 2
    if (input$view_mode == "Compare Two Regions/States") {
      sel2 <- input$selected_state_2
      if (sel2 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
        df <- df %>% filter(region == sel2)
      } else {
        df <- df %>% filter(state == sel2)
      }
    }
    
    df
  })
  
  
  #REC_5 for state 1
  output$rec5_plot <- renderPlot({
    df <- filtered_mob() %>%
      filter(!is.na(OR_travel), OR_travel != "Refused") %>%
      count(OR_travel) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$mob_plot_type,
                  
                  # === Bar Chart ===
                  "bar" = ggplot(df, aes(x = OR_travel, y = Percent)) +
                    geom_col(fill = "#ff7f0e") +
                    labs(x = "Travel Distance", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie Chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = OR_travel)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Travel Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut Chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = OR_travel)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Travel Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add percentage labels
    plt + geom_text(
      aes(label = paste0(round(Percent, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 5
    )
  })
  
  #REC_5 for state 2
  output$rec5_plot_2 <- renderPlot({
    df <- filtered_mob2() %>%
      filter(!is.na(OR_travel), OR_travel != "Refused") %>%
      count(OR_travel) %>%
      mutate(Percent = n / sum(n) * 100)
    
    plt <- switch(input$mob_plot_type,
                  
                  # === Bar Chart ===
                  "bar" = ggplot(df, aes(x = OR_travel, y = Percent)) +
                    geom_col(fill = "#ff7f0e") +
                    labs(x = "Travel Distance (State 2)", y = "Percentage of Respondents") +
                    theme_classic() +
                    theme(
                      panel.grid        = element_blank(),
                      panel.border      = element_rect(colour = "black", fill = NA, size = 1),
                      axis.line         = element_line(colour = "black"),
                      axis.ticks.length = unit(0.25, "cm"),
                      axis.ticks        = element_line(colour = "black"),
                      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),
                      axis.text.y       = element_text(size = 14),
                      axis.title        = element_text(size = 16),
                      legend.title      = element_text(size = 16),
                      legend.text       = element_text(size = 14)
                    ),
                  
                  # === Pie Chart ===
                  "pie" = ggplot(df, aes(x = "", y = Percent, fill = OR_travel)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(fill = "Travel Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    ),
                  
                  # === Donut Chart ===
                  "donut" = ggplot(df, aes(x = 2, y = Percent, fill = OR_travel)) +
                    geom_col(width = 1) +
                    coord_polar(theta = "y") +
                    xlim(0.5, 2.5) +
                    theme_void() +
                    labs(fill = "Travel Distance") +
                    theme(
                      legend.title = element_text(size = 16),
                      legend.text  = element_text(size = 14)
                    )
    )
    
    # Add labels
    plt +
      geom_text(
        aes(label = paste0(round(Percent, 1), "%")),
        position = position_stack(vjust = 0.5),
        size = 5
      )
  })
  
  
  
  # Top-5 “Within local community”
  output$top5_local <- DT::renderDataTable({
    data %>%
      filter(OR_travel == "Within local community") %>%
      count(state) %>%
      mutate(Percent = round(100 * n / sum(n), 1)) %>%
      arrange(desc(Percent)) %>%
      slice_head(n = 5) %>%
      select(state, Percent)
  }, options = list(dom = 't', paging = FALSE))
  
  # Top-5 “Within my state”
  output$top5_instate <- DT::renderDataTable({
    data %>%
      filter(OR_travel == "Within my state") %>%
      count(state) %>%
      mutate(Percent = round(100 * n / sum(n), 1)) %>%
      arrange(desc(Percent)) %>%
      slice_head(n = 5) %>%
      select(state, Percent)
  }, options = list(dom = 't', paging = FALSE))
  
  # Top-5 “To a different state”
  output$top5_diff <- DT::renderDataTable({
    data %>%
      filter(OR_travel == "To a different state") %>%
      count(state) %>%
      mutate(Percent = round(100 * n / sum(n), 1)) %>%
      arrange(desc(Percent)) %>%
      slice_head(n = 5) %>%
      select(state, Percent)
  }, options = list(dom = 't', paging = FALSE))
  
  #REC_6
  output$rec6_plot <- renderPlotly({
    df <- filtered_data() %>%
      select(starts_with("REC6_")) %>%
      pivot_longer(everything(),
                   names_to  = "motivation_code",
                   values_to = "selected") %>%
      filter(selected == 1) %>%
      count(motivation_code) %>%
      mutate(
        Percent    = n / sum(n) * 100,
        motivation = rec6_labels[motivation_code]
      ) %>%
      select(-motivation_code) %>%
      # 2) ensure every label, even if zero
      complete(
        motivation = rec6_labels[names(rec6_labels) != "REC6_999"],
        fill       = list(n = 0, Percent = 0)
      ) %>%
      arrange(n) %>%
      mutate(motivation = factor(motivation, levels = motivation))
    
    p <- ggplot(df, aes(x = motivation, y = Percent,
                        text = paste0(round(Percent,1), "%)"))) +
      geom_col(fill = "#4CAF50") +
      coord_flip() +
      labs(
        x     = NULL,
        y     = "Percentage of respondents",
        title = "Motivations for Outdoor Recreation"
      )  +
      theme_classic() +
      theme(
        panel.border = element_blank(),
        axis.line   = element_line(colour = "black"),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  #REC_8
  output$rec8_plot <- renderPlotly({
    # Recompute from the *filtered* data each time the state changes
    df <- filtered_data() %>%
      select(starts_with("REC8_")) %>%
      pivot_longer(everything(),
                   names_to  = "barrier_code",
                   values_to = "selected") %>%
      filter(selected == 1) %>%
      count(barrier_code) %>%
      mutate(
        Percent = n / sum(n) * 100,
        label   = rec8_labels[barrier_code]
      ) %>%
      select(-barrier_code) %>%
      filter(label != "Refused") %>%
      complete(
        label = rec8_labels[names(rec8_labels) != "REC8_999"],
        fill  = list(n = 0, Percent = 0)
      ) %>%
      arrange(n) %>%
      mutate(label = factor(label, levels = label))
    
    p <- ggplot(df, aes(x = label, y = Percent,
                        text = paste0(round(Percent,1), "%)"))) +
      geom_col(fill = "#4CAF50") +
      coord_flip() +
      labs(
        x     = NULL,
        y     = "Percentage of respondents",
        title = "Barriers to Outdoor Recreation"
      ) +
      theme_classic() +
      theme(
        panel.border = element_blank(),
        axis.line   = element_line(colour = "black"),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    
    
    ggplotly(p, tooltip = "text")
  })
  
  # Top motivation valueBox
  output$top_motivation <- renderValueBox({
    # Build long‐format for REC6 in the selected state
    df_mot <- filtered_data() %>%
      select(starts_with("REC6_")) %>%
      pivot_longer(
        cols     = everything(),
        names_to = "code",
        values_to= "selected"
      ) %>%
      filter(selected == 1) %>%
      count(code, name = "n") %>%
      mutate(
        total    = sum(n),
        percent  = round(100 * n / total, 1),
        motivation = rec6_labels[code]
      ) 
    
    top <- df_mot %>% arrange(desc(n)) %>% slice(1)
    
    valueBox(
      top$motivation,
      paste0(top$percent, " % of respondents"),
      icon  = icon("heart"),
      color = "green"
    )
  })
  
  # Top barrier valueBox
  output$top_barrier <- renderValueBox({
    df_bar <- filtered_data() %>%
      select(starts_with("REC8_")) %>%
      pivot_longer(
        cols     = everything(),
        names_to = "code",
        values_to= "selected"
      ) %>%
      filter(selected == 1) %>%
      count(code, name = "n") %>%
      mutate(
        total   = sum(n),
        percent = round(100 * n / total, 1),
        barrier = rec8_labels[code]
      ) %>%
      filter(barrier != "Refused")
    
    
    topb <- df_bar %>% arrange(desc(n)) %>% slice(1)
    
    valueBox(
      topb$barrier,
      paste0(topb$percent, " % of respondents"),
      icon  = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  #interactive leaflet map
  states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
    mutate(region = tolower(ID))
  
  # Reactive summary of each state’s top motivation & barrier
  state_summary <- reactive({
    # Helper to get top label for a REC group
    get_top <- function(df_long, label_map) {
      df_long %>%
        filter(selected == 1) %>%
        count(state, code, name = "n") %>%
        group_by(state) %>%
        mutate(
          total   = sum(n),
          percent = round(100 * n / total, 1)
        ) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(label = label_map[code]) %>%
        select(state, label, n, percent)
    }
    
    # Pivot REC6 for motivations
    mot_long <- data %>%
      select(state, starts_with("REC6_")) %>%
      pivot_longer(-state, names_to = "code", values_to = "selected")
    
    # Pivot REC8 for barriers
    bar_long <- data %>%
      select(state, starts_with("REC8_")) %>%
      pivot_longer(-state, names_to = "code", values_to = "selected")
    
    top_mot <- get_top(mot_long, rec6_labels) %>%
      rename(top_mot_label   = label,
             top_mot_n       = n,
             top_mot_percent = percent)
    
    top_bar <- get_top(bar_long, rec8_labels) %>%
      filter(label != "Refused") %>%
      rename(top_bar_label   = label,
             top_bar_n       = n,
             top_bar_percent = percent)
    
    left_join(top_mot, top_bar, by = "state") %>%
      mutate(region = tolower(state))
  })
  
  
  # Render the Leaflet map
  output$state_mot_bar_map <- renderLeaflet({
    df <- state_summary()
    map_data <- left_join(states_sf, df, by = "region")
    
    # Define a color palette for top motivation count
    valid_vals <- map_data$top_mot_percent[!is.na(map_data$top_mot_percent) & is.finite(map_data$top_mot_percent)]
    
    pal_mot <- colorNumeric(
      palette = "YlGnBu",
      domain  = valid_vals,
      na.color = "#CCCCCC"
    )
    
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal_mot(top_mot_percent),
        weight      = 1,
        color       = "#444444",
        fillOpacity = 0.7,
        label = ~sprintf(
          "<strong>%s</strong><br/>
   Top Motivation: %s (%s%%)<br/>
   Top Barrier: %s (%s%%)",
          tools::toTitleCase(region),
          top_mot_label,    top_mot_percent,
          top_bar_label,    top_bar_percent
        ) %>% lapply(htmltools::HTML),
        highlight = highlightOptions(
          weight       = 2,
          color        = "#555555",
          bringToFront = TRUE
        )
      ) %>% 
      addLegend(
        position = "bottomright",
        pal      = pal_mot,
        values   = map_data$top_mot_percent,
        title    = "Top Motivation (%)",
        labFormat = labelFormat(suffix = "%"),
        opacity  = 1
      )
  })
  
  # --- NEW: Context‐specific reactives ---
  filtered_ctx1 <- reactive({
    df <- data
    # apply same demo filters
    if (input$demo_gender != "All")   df <- df %>% filter(gender_label == input$demo_gender)
    if (input$demo_race != "All")     df <- df %>% filter(race_label == input$demo_race)
    if (input$demo_urban != "All")    df <- df %>% filter(urban_rural == input$demo_urban)
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    # Income
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    #Farm
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    
    # then state 1
    sel1 <- input$ctx_state1
    if (sel1 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
      df <- df %>% filter(region == sel1)
    } else {
      df <- df %>% filter(state == sel1)
    }
    
    df
  })
  
  # Reactive data filtered by selected state 
  filtered_ctx2<- reactive({
    df <- data
    # apply same demo filters
    if (input$demo_gender != "All")   df <- df %>% filter(gender_label == input$demo_gender)
    if (input$demo_race != "All")     df <- df %>% filter(race_label == input$demo_race)
    if (input$demo_urban != "All")    df <- df %>% filter(urban_rural == input$demo_urban)
    df <- df %>%
      filter(HD2 >= input$demo_age[1],
             HD2 <= input$demo_age[2])
    # Income
    if (input$demo_income != "All") {
      df <- df %>% filter(income_label == input$demo_income)
    }
    #Farm
    if (input$demo_farm != "All") {
      df <- df %>% filter(farm_label == input$demo_farm)
    }
    
    # then state 2
    if (input$view_mode == "Compare Two Regions/States") {
      sel2 <- input$ctx_state2
      if (sel2 %in% c("North Central Region", "North Eastern Region", "Southern Region")) {
        df <- df %>% filter(region == sel2)
      } else {
        df <- df %>% filter(state == sel2)
      }
    }
    
    df
  })
  
  
  # REC7 for Context
  output$rec7_plot <- renderPlotly({
    s1 <- summarize_rec7(filtered_ctx1(),  input$ctx_state1)
    if (input$view_mode == "Compare Two Regions/States") {
      s2 <- summarize_rec7(filtered_ctx2(), input$ctx_state2)
      df <- bind_rows(s1, s2)
    } else {
      df <- s1
    }
    p <- ggplot(df, aes(x = OR_home_choice, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      labs(
           x = "Importance Level", y = "Percentage of Respondents", fill = "Region/State") +
      theme_classic() +
      theme(
        panel.grid        = element_blank(),
        panel.border      = element_rect(colour = "black", fill = NA, size = 1),
        axis.line         = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks        = element_line(colour = "black"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    ggplotly(p, tooltip = "y")
  })
  
  # REC9 for Context
  output$rec9_plot <- renderPlotly({
    s1 <- summarize_rec9(filtered_ctx1(),  input$ctx_state1)
    if (input$view_mode == "Compare Two Regions/States") {
      s2 <- summarize_rec9(filtered_ctx2(), input$ctx_state2)
      df <- bind_rows(s1, s2)
    } else {
      df <- s1
    }
    p <- ggplot(df, aes(x = OR_com_programs, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      labs(
           x = "Response", y = "Percentage of Respondents", fill = "Region/State") +
      theme_classic() +
      theme(
        panel.grid        = element_blank(),
        panel.border      = element_rect(colour = "black", fill = NA, size = 1),
        axis.line         = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks        = element_line(colour = "black"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    ggplotly(p, tooltip = "y")
  })
  
  # REC10 for Context
  output$rec10_plot <- renderPlotly({
    s1 <- summarize_rec10(filtered_ctx1(),  input$ctx_state1)
    if (input$view_mode == "Compare Two Regions/States") {
      s2 <- summarize_rec10(filtered_ctx2(), input$ctx_state2)
      df <- bind_rows(s1, s2)
    } else {
      df <- s1
    }
    p <- ggplot(df, aes(x = OR_com_income, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set3") +
      labs(
           x = "Perceived Extent", y = "Percentage of Respondents", , fill = "Region/State") +
      theme_classic() +
      theme(
        panel.grid        = element_blank(),
        panel.border      = element_rect(colour = "black", fill = NA, size = 1),
        axis.line         = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks        = element_line(colour = "black"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    ggplotly(p, tooltip = "y")
  })
  
  #QOL5_D plot
  output$QOL5_D_plot <- renderPlotly({
    s1 <- summarize_QOL5_D(filtered_ctx1(),  input$ctx_state1)
    if (input$view_mode == "Compare Two Regions/States") {
      s2 <- summarize_QOL5_D(filtered_ctx2(), input$ctx_state2)
      df <- bind_rows(s1, s2)
    } else {
      df <- s1
    }
    p <- ggplot(df, aes(x = outdoor_spaces_sat, y = Percent, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set3") +
      labs(
           x = "Perceived Extent", y = "Percentage of Respondents", fill = "Region/State") +
      theme_classic() +
      theme(
        panel.grid        = element_blank(),
        panel.border      = element_rect(colour = "black", fill = NA, size = 1),
        axis.line         = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks        = element_line(colour = "black"),
        axis.text.x=element_text(angle=45,hjust=1), 
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14)
      )
    ggplotly(p, tooltip = "y")
  })
  
  # State summary card for Context
  output$state_summary_card <- renderUI({
    req(input$ctx_state1)
    rec7  <- summarize_rec7(filtered_ctx1(),  input$ctx_state1)
    rec9  <- summarize_rec9(filtered_ctx1(),  input$ctx_state1)
    rec10 <- summarize_rec10(filtered_ctx1(), input$ctx_state1)
    QOL5_D <- summarize_QOL5_D(filtered_ctx1(), input$ctx_state1)
    top7  <- rec7 %>%  arrange(desc(Percent)) %>% slice(1)
    top9  <- rec9 %>%  arrange(desc(Percent)) %>% slice(1)
    top10 <- rec10 %>% arrange(desc(Percent)) %>% slice(1)
    topQOL5_D <- QOL5_D %>% arrange(desc(Percent)) %>% slice(1)
    
    HTML(paste0(
      "<h4>Summary for ", input$ctx_state1, "</h4>",
      "<ul>",
      "<li><b>Importance for Residence Choice:</b> ", top7$OR_home_choice,
      " (", round(top7$Percent, 1), "%)</li>",
      "<li><b>Community Support:</b> ", top9$OR_com_programs,
      " (", round(top9$Percent, 1), "%)</li>",
      "<li><b>Reliance on Outdoor Rec:</b> ", top10$OR_com_income,
      " (", round(top10$Percent, 1), "%)</li>",
      "<li><b>Satisfaction with Outdoor Rec:</b> ", topQOL5_D$outdoor_spaces_sat,
      " (", round(topQOL5_D$Percent, 1), "%)</li>",
      "</ul>"
    ))
  })
  
}


# ---- APP ----
shinyApp(
  ui = dashboardPage(
                     header, sidebar, body,
                     ),
  server = server
)
