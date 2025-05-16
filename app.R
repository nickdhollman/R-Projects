library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# Load the dataset
nhanes_data <- read.csv("nhanes_.csv")

# Convert necessary variables to factors for display
# need to adjust below to have labels for age_cat, educat, PIR_cat
df <- nhanes_data %>%
  mutate(
    age_cat = factor(age_cat, levels = c(0, 1, 2, 3, 4, 5),
                     labels = c("20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70+")),
    educat = factor(educat, levels = c(1, 2, 3, 4, 5),
                    labels = c("Less than 9th grade", "9 - 11th grade", "HS Grad/GED", "Some college/AA", "College Grad or above")),
    PIR_cat = factor(PIR_cat, levels = c(0, 1, 2, 3),
                     labels = c("> 3.5", "1.86–3.5", "1.31–1.85", "<= 1.30")),
    FSDHH = factor(FSDHH, levels = c(1, 2, 3, 4),
                   labels = c("High", "Marginal", "Low", "Very Low"))
  )

ui <- dashboardPage(
  dashboardHeader(titleWidth = 450, title = "Social Determinants & Health Outcomes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    selectInput("age", "Age Category:", choices = levels(df$age_cat), selected = levels(df$age_cat), multiple = TRUE),
    selectInput("educ", "Education Level:", choices = levels(df$educat), selected = levels(df$educat), multiple = TRUE),
    selectInput("pir", "PIR Category:", choices = levels(df$PIR_cat), selected = levels(df$PIR_cat), multiple = TRUE),
    selectInput("foodsec", "Food Security:", choices = levels(df$FSDHH), selected = levels(df$FSDHH), multiple = TRUE)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("insuranceBox"),
                valueBoxOutput("depressionBox"),
                valueBoxOutput("diabetesBox")
              )
      ),
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Depression Risk by Food Security", width = 6, plotOutput("plot1")),
                box(title = "Insurance by Education", width = 6, plotOutput("plot2"))
              ),
              fluidRow(
                box(title = "PHQ-9 Scores by PIR Category", width = 12, plotOutput("plot3"))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Filtered NHANES Data", width = 12, DTOutput("table"))
              ),
              fluidRow(
                column(12, tags$p(
                  "Data Source: National Health and Nutrition Examination Survey (NHANES), Centers for Disease Control and Prevention (CDC).",
                  tags$a(href = "https://www.cdc.gov/nchs/nhanes", "https://www.cdc.gov/nchs/nhanes"),
                  style = "font-size:12px; color:gray; padding-top:10px;"
                ))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>%
      filter(
        age_cat %in% input$age,
        educat %in% input$educ,
        PIR_cat %in% input$pir,
        FSDHH %in% input$foodsec
      )
  })
  
  output$insuranceBox <- renderValueBox({
    data <- filtered_data()
    pct <- round(mean(data$No_insurance, na.rm = TRUE) * 100, 1)
    valueBox(paste0(pct, "%"), "Without Insurance", icon = icon("medkit"), color = "orange")
  })
  
  output$depressionBox <- renderValueBox({
    data <- filtered_data()
    pct <- round(mean(data$PHQ9_binary, na.rm = TRUE) * 100, 1)
    valueBox(paste0(pct, "%"), "Depression Risk", icon = icon("heartbeat"), color = "red")
  })
  
  output$diabetesBox <- renderValueBox({
    data <- filtered_data()
    pct <- round(mean(data$diabetes, na.rm = TRUE) * 100, 1)
    valueBox(paste0(pct, "%"), "Diagnosed with Diabetes", icon = icon("tint"), color = "blue")
  })
  
  output$plot1 <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = FSDHH, fill = factor(PHQ9_binary))) +
      geom_bar(position = "fill") +
      labs(x = "Food Security", y = "Proportion", fill = "Depression Risk") +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = educat, fill = factor(No_insurance))) +
      geom_bar(position = "fill") +
      labs(x = "Education Level", y = "Proportion", fill = "No Insurance") +
      theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = PIR_cat, y = PHQ9)) +
      geom_boxplot() +
      labs(x = "PIR Category", y = "PHQ-9 Score") +
      theme_minimal()
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)