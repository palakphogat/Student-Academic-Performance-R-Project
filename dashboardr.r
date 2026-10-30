library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(corrplot)
library(cluster)
library(factoextra)
                                                                                                                                                
# Load dataset
data_url <- "https://raw.githubusercontent.com/guipsamora/pandas_exercises/master/04_Apply/Students_Alcohol_Consumption/student-mat.csv"
student_data <- read.csv(data_url, sep = ",")
df_clean <- student_data %>% select(studytime, absences, failures, freetime, goout, G1, G2, G3)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Student Performance & Recommendation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Student Intervention Checker"),
      numericInput("study_hours", "Weekly Study Time (1: <2h, 2: 2-5h, 3: 5-10h, 4: >10h):", value = 1, min = 1, max = 4),
      numericInput("absences_cnt", "Number of Absences:", value = 10, min = 0, max = 100),
      numericInput("past_failures", "Past Class Failures:", value = 2, min = 0, max = 4),
      actionButton("btn_predict", "Get Recommendation", class = "btn-primary w-100"),
      hr(),
      uiOutput("rec_output")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Cluster Analysis", plotOutput("cluster_plot", height = "450px")),
        tabPanel("Study Time vs Final Grade", plotOutput("boxplot_plot", height = "450px")),
        tabPanel("Absences Impact", plotOutput("scatter_plot", height = "450px"))
      )
    )
  )
)

server <- function(input, output) {
  
  set.seed(123)
  scaled_data <- scale(df_clean %>% select(studytime, absences, failures, G3))
  kmeans_res <- kmeans(scaled_data, centers = 3, nstart = 25)
  
  output$cluster_plot <- renderPlot({
    fviz_cluster(kmeans_res, data = scaled_data, geom = "point", ellipse.type = "convex", ggtheme = theme_minimal())
  })
  
  output$boxplot_plot <- renderPlot({
    ggplot(df_clean, aes(x = factor(studytime), y = G3, fill = factor(studytime))) +
      geom_boxplot() +
      scale_x_discrete(labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs")) +
      labs(title = "Effect of Weekly Study Time on Final Grade", x = "Study Time Category", y = "Final Grade (0-20)", fill = "Study Time") +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(df_clean, aes(x = absences, y = G3)) +
      geom_point(color = "#2b5c8f", alpha = 0.7) +
      geom_smooth(method = "lm", color = "#e74c3c") +
      labs(title = "Impact of Absences on Final Performance", x = "Absences", y = "Final Grade (0-20)") +
      theme_minimal()
  })
  
  observeEvent(input$btn_predict, {
    output$rec_output <- renderUI({
      sh <- input$study_hours
      ab <- input$absences_cnt
      pf <- input$past_failures
      
      if (pf > 0 || sh < 2) {
        div(class = "alert alert-danger",
            tags$b("Status: HIGH RISK STUDENT"), br(),
            "Recommendation: Assign to academic mentoring & require at least 5 hours/week of guided study sessions.")
      } else if (ab > 10) {
        div(class = "alert alert-warning",
            tags$b("Status: ATTENDANCE RISK"), br(),
            "Recommendation: Attendance tracking review required; issue warning letter.")
      } else {
        div(class = "alert alert-success",
            tags$b("Status: ON TRACK / GOOD STANDING"), br(),
            "Recommendation: Eligible for advanced honors coursework and peer tutoring roles.")
      }
    })
  })
}

shinyApp(ui = ui, server = server)