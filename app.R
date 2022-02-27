library(shiny)

ui<-shinyUI(fluidPage(
  titlePanel("Daily Alcohol Consumption And Grades/Gender"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Exploring student alcohol Consumption"),
      
      selectInput("var", 
                  label = "Choose a Grade to display G1,G2,G3",
                  choices = c("G1", "G2",
                              "G3"),
                  selected = "G3"),
      
      sliderInput("bins", 
                  label = "Binwidth",
                  min = 0, max = 10, value = 5),
      selectInput("gender",
                  label="Choose a Gender",
                  choices=c("Male","Female"),
                  selected="Female"),
      selectInput("higher",
                  label="Higher Education",
                  choices = c("Yes","No"),
                  selected="Yes")
      
    ),
    
    mainPanel(plotOutput("plot"))
    
  )
))

library(ggplot2)

alc <- read.csv("student-por.csv",stringsAsFactors = F,header=T)
plot_function <- function(var,color,wbins,title_text)
{
  names(var)<- c("Sex","Higher","Dalc","grade")
  var <- var[,c(-1,-2)]
  var$Dalc <- as.factor(var$Dalc)
  g <- ggplot(var,aes(x=grade))+geom_histogram(binwidth = wbins,fill=color)+facet_grid(Dalc~.)+ggtitle(title_text)+theme(
    plot.title = element_text(hjust=0.4)
  )
  g
}
server <- shinyServer(
  function(input, output) {
    output$plot <- renderPlot({
      data <- switch(input$var, 
                     "G1" = alc[,c(2,21,27,31)],
                     "G2" = alc[,c(2,21,27,32)],
                     "G3" = alc[,c(2,21,27,33)]
      )
      
      color <- switch(input$var, 
                      "G1" = "darkgreen",
                      "G2" = "black",
                      "G3" = "darkorange"
      )
      title <- switch(input$var,
                      "G1"="Daily Alcohol Consumption and Grade 1",
                      "G2"="Daily Alcohol Consumption and Grade 2",
                      "G3"="Daily Alcohol Consumption and Grade 3")
      data <- switch(input$gender,
                     "Male"=subset(data,sex=="M"),
                     "Female"=subset(data,sex=="F"))
      title <- switch(input$gender,
                      'Male'=paste(title,"for Male Students"),
                      "Female"=paste(title,"for female students"))
      data <- switch(input$higher,
                     "Yes"=subset(data,higher=="yes"),
                     "No"=subset(data,higher=="no"))
      title <- switch(input$higher,
                      "Yes"=paste(title,"who intend to study further"
                      ),
                      "No"=paste(title,"who do not intend to study further"))
      
      
      
      plot_function(var = data, 
                    color = color, 
                    wbins = input$bins,
                    title_text=title)
    })
  }
)

# Run the application 
shinyApp(ui = ui, server = server)
