# Shiny App for displaying the results of Linear Regression

library(shiny)
library(shinythemes)
library(PerformanceAnalytics)  # for chart.correlation
library(ggplot2)
library(caTools)


ui <- fluidPage(
  #theme = shinytheme("cyborg"),
  shinythemes :: themeSelector(),
  # App title ----
  titlePanel("Simple Linear Regression"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Horizontal line ----
      tags$hr(),     #html header styling
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               Tail = "tail",
                               All = "all"),
                   selected = "head")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("contents")), 
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Correlation", plotOutput("corrplot")),
        tabPanel("Histogram", plotOutput("Hist1"),
                 plotOutput("Hist2")),
        tabPanel("Prediction", verbatimTextOutput("modelSummary")),
        tabPanel("Regression Plot", plotOutput("Viz"))
        
      )
      
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) 
{
  data <- reactive({  #reactive is uses widget input and returns a value
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
  })
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else if(input$disp == "tail"){
      return(tail(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$summary <- renderPrint({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    print(summary(df))
    
  })
  
  
  output$corrplot<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    chart.Correlation(df, histogram=TRUE, pch=19)
    
    #Visualization of a Correlation Matrix. On top the (absolute) value of the correlation plus the result of the cor.test as stars. On bottom, the bivariate scatterplots, with a fitted line
  })
  
  
  output$Hist1<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    ggplot(df, aes(x=YearsExperience)) + 
      geom_histogram(bins=10, color="black", fill="skyblue") + 
      geom_vline(aes(xintercept=mean(YearsExperience)), 
                 color="red", linetype="dashed", size=2) + 
      ggtitle("Histogram of Years of Experience") + 
      theme_bw()
      
    
    
    #hist(df$YearsExperience,
         #main="Histogram for Years of Experience",
         #xlab = "YearsExperience",
         #border = "blue")
  })
  
  output$Hist2<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    ggplot(df, aes(x=Salary)) + 
      geom_histogram(bins=10, color="black", fill="pink") + 
      geom_vline(aes(xintercept=mean(Salary)), 
                 color="red", linetype="dashed", size=2) + 
      ggtitle("Histogram of Salary") + 
      theme_bw()
  })
  
  output$modelSummary <- renderPrint({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    regressor = lm(formula = Salary ~ YearsExperience, data = df)
    print(data.frame(YearsExperience=df$YearsExperience, 
                     ActualSalary=df$Salary,
                     PredictedSalary = predict(regressor), 
                     Residuals=residuals(regressor)))
    
  })
  
  output$Viz<- renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    regressor = lm(formula = Salary ~ YearsExperience, data = df)
    ggplot() +
      geom_point(aes(x = df$YearsExperience, y = df$Salary),
                 shape=21, colour = 'black', fill="white", size=2, stroke=4) +
      geom_line(aes(x = df$YearsExperience, 
                    y = predict(regressor, newdata = df)),
                colour = 'red', size=2) +
      ggtitle('Salary vs Experience') +
      xlab('Years of experience') +
      ylab('Salary') + 
      theme_bw()
  })
}
shinyApp(ui, server)
