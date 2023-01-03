library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(DT)
library(RColorBrewer)


#import mtcar dataset from R
myData <- mtcars


# Define UI for random distribution app 
ui <- fluidPage(
  
  #App title
  titlePanel("Prediction of MPG based on MTCars Dataset"),
  #Sidebar layout with input and output definitions
  sidebarLayout(
    #Sidebar panel for inputs
    sidebarPanel(
      
      #Defining the Input: file datatype
      fileInput("datafile", "Please upload the car file", accept = c(".csv")),
      
      
      #Input: Slider for people to enter horse power
      sliderInput(inputId = "hp", 
                  label = "Enter Horse Power:",
                  min = 0, max = 250, value = c(50,150)),
      #Input: Slider for user to enter weight 
      sliderInput(inputId = "wt",
                  label = "Weight Range",
                  min = 1, max = 6, value = 1),
      #Input: select number of bins input
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 15,
                  value = 7),
      #Input: select the number of cylinders
      selectInput(inputId = "cyl","Select cylinder",choices = c(mtcars$cyl),multiple = TRUE),
      #Input: select number of gear
      selectInput("gear","Select gear",choices = c(mtcars$gear),multiple = TRUE),
      
      #Input: select colour option
      selectInput("colours", "Color Scheme",rownames(subset(brewer.pal.info,category %in% c("seq", "div")))),
      #Submit button to submit all the variables
      submitButton("Submit")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Scatterlot",plotOutput("scatterplot"),br(), br(),plotOutput("scatterplot2"),br(), br(),plotOutput("scatterplot3")),
                  tabPanel("Histogram",plotOutput("histogram"),br(), br(),plotOutput("histogram2")),
                  tabPanel("Boxplot",plotOutput("boxplot")),
                  tabPanel("Average MPG",tableOutput("averageMpg")),
                  tabPanel("Summary",verbatimTextOutput("MtcarsSummary")),
                  tabPanel("Table",DT::dataTableOutput("MtcarsTable"))
                  
      )
      
    )
  )
)


# Define server logic for random distribution app
server <- function(input, output, session){
  defaultData = mtcars
  
  
  # Receiving the data from input
  datasetInput <- reactive({
    read.csv(input$datafile$datapath, header = TRUE)
  })
  
  #Create a variable: sliderData to obtain the user inputs
  sliderData <- reactive({
    #make sure the inputs are not NULL
    req(input$hp, input$wt, input$cyl, input$gear)
    #filter the input data
    sliderData <- filter(defaultData(), between(hp, input$hp[1], input$hp[2]))
    return(sliderData)
  })
  
  hp <- reactive(seq())
  
  
  # Scatterplot of Horsepower and MPG
  output$scatterplot <- renderPlot({
    #Create a plot
    ggplot(mtcars, aes(hp, mpg)) + geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ylab("Miles per Gallon") +
      xlab("No. of Horsepower") +
      ggtitle("Impact of Number of Horsepower on MPG")
  })
  
  # Scatterplot of Weight and MPG
  output$scatterplot2 <- renderPlot({
    #Create a plot
    ggplot(data = myData, aes(wt, mpg)) +
      geom_point(aes(color = cyl))+
      ggtitle("Impact of Number of Weight on MPG")
  })
  
  # Scatterplot og Number of Cylinder and MPG
  output$scatterplot3 <- renderPlot({
    ggplot(mtcars, aes(cyl, mpg)) + geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ylab("Miles per Gallon") +
      xlab("No. of Cylinder") +
      ggtitle("Impact of Number of Cylinder on MPG")
  })
  
  
  # Histogram of MPG
  output$histogram <- renderPlot({
    x    <- mtcars$mpg  
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #Create histogram plot
    hist(x, breaks = bins, main = 'Histogram of MPG', col = 'blue', border = 'black', xlab = "MPG")
  })
  
  # Histogram of Weight
  output$histogram2 <- renderPlot({
    y    <- mtcars$wt  
    bins <- seq(min(y), max(y), length.out = input$bins + 1)
    #Create histogram plot
    hist(y, breaks = bins, main = 'Histogram of Weight', col = 'peachpuff', border = 'black', xlab = "Weight")
  })
  
  
  # Boxplot for number of cylinder
  output$boxplot <- renderPlot ({
    ggplot(mtcars, aes(factor(cyl),mpg))+
      geom_boxplot()
  })
  
 
  
  # Generate the average MPG using variables
  check <- reactive({
    if(is.null(input$cyl) & is.null(input$gear)){
      mtcars %>% summarise(Average_mpg = mean(mpg))
    }else if(!is.null(input$cyl) & is.null(input$gear)){
      a <- mtcars %>% group_by(cyl) %>% summarise(Average_mpg = mean(mpg))
      a %>% filter(cyl==input$cyl)
    }else if(is.null(input$cyl) & !is.null(input$gear)){
      a <- mtcars %>% group_by(gear) %>% summarise(Average_mpg = mean(mpg))
      a %>% filter(gear==input$gear)
    }else{
      a <-  mtcars %>% group_by(gear,cyl) %>% summarise(Average_mpg = mean(mpg))
      a %>% filter(cyl==input$cyl & gear==input$gear)
    }
  })
  
  output$averageMpg <- renderTable(
    check()
  ) 
  
  
  
  
  # Generate data summary
  #The Output$MtcarsSummary depends on the datasetInput reactive expression,
  output$MtcarsSummary <-renderPrint({
    summary(mtcars)
  })
  
  
  # Generate dataset
  
  output$MtcarsTable <- DT::renderDataTable({
    DT::datatable(mtcars, options = list(orderClasses = TRUE))
  })
  
  
  
}


shinyApp(ui = ui, server = server)


