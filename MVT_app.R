#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('S:/PAM/Client or Account Group Folders/Twitter/Twitter 2017/Analytics/Measurement Reco/Testing Strategy/MVT.R')

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Mutlvariate Testing On Ad Copy"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     
     column(3,
            fileInput("file", label = h3("load data.."))),

     
     column(3,
            radioButtons("s.level", label = h3("Significance Level"),
                      choices = list("0.80" = 0.80, "0.90" = 0.90,
                                     "0.95" = 0.95,"0.99"=0.99),selected = 0.99))
   ),
  
      # Show a plot of the generated distribution
   mainPanel(
     tabsetPanel(
       tabPanel("P-Value of ANOVA",
                h5(textOutput("P-Value of ANOVA")),
                textOutput("contents")
       ),
       tabPanel("Wining Combination",
                h5(textOutput("Wining Combination")),
                tableOutput("tables")
       )
     )
   )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
     
      
      # generate bins based on input$bins from ui.R
      #result<-MVT(file=input$file,s.level=input$s.level)
      
  output$contents <- renderText({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file
    
    if (is.null(inFile)) {
      print("Data File not uploaded.")
    } else {
    result<-MVT(file=inFile$datapath,s.level=as.numeric(input$s.level))
    if(result$anova >= 1-as.numeric(input$s.level)) {
      paste("The P-Value of Anova is: ",round(result$anova.p.value,2),". there is not statistically siginificant difference regarding CTR among the variants tested.")
    }
    }
})
  output$tables <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file
    
    if (is.null(inFile)) {

    } else {
      result<-MVT(file=inFile$datapath,s.level=as.numeric(input$s.level))
      if(result$anova < 1-as.numeric(input$s.level)) {
        result$table
      }
    }
  })
}
)


# Run the application 
shinyApp(ui = ui, server = server)

