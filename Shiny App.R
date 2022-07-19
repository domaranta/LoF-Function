###############################################
# Preliminaries                               #
###############################################

# import required libraries
library(shiny) 

#source required R scripts 
#source('LoF_Function.R', encoding = 'UTF-8')


###############################################
# User Interface (UI)                         #
###############################################

#Define the UI 
ui <- fluidPage(
  
  # app title
  titlePanel("Lack of Fit Test (rename to whatever you would like)"),
  
  
  #create layout 
  
  #Left sidebar
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      
      #Create a data import box here (search online for favorite version)
      # to start go to https://shiny.rstudio.com/gallery/#demos and scroll down to widgets
      # or start looking around online.  BE SURE TO DO VERSION CONTROL as you start
      # to make changes.
      #br() creates a line break
      div("Data Import Here (delete this text later)",align="left", style = "font-size: 8pt"),br(),
      
      #Make a way for the user to specify desired formula (e.g. y~sqrt(x1)+x2+I(x2^2))
      div("Formula Creation Here (delete this text later)",align="left", style = "font-size: 8pt"),br(),
      
      #Make a way for the user to specify cluster
      div("Custer Selection Here (delete this text later)", align="left", style = "font-size: 8pt"),br(),
      
      #Test selection drop-down
      #h5 is the 5th level of a hiearchy of decreasing subheadings;i.e.subsubsubsubheading
      selectInput(inputId="test", 
                  label=h5("Test:"), 
                  choices = list("Christensen '89","Atwood & Ryan", "Su and Yang", "Shillington", "JSL", "Christensen '91", "Utts")),
      
      #submit button
      div(submitButton("Submit"),align="right"),br(), 
      
      #Contact Info
      div("Lack of Fit Shiny app (rename to whatever you would like)",align="center", style = "font-size: 8pt"),
      div("maintained by",
          #replace with your name and for webpage you can use email or your 
          # own webpage if you have one (e.g. github)
          a(href="https://statistics.calpoly.edu/bret-holladay",target="_blank",
            "Bret Holladay (replace with your name and webpage/email here)"),align="center", style = "font-size: 8pt"), br(), br(),
      
      #close left sidebar   
    ),
    
    #main panel for displaying outputs 
    #tells the app the names of "objects" to be outputted in main panel
    #these objects are defined in the server below
    mainPanel(
      
      #tells the app to output text "text_test" defined below
      textOutput("text_test"),
      br(),
      
      #tells the app to output "results" defined below
      tableOutput("results")
      
      #close main panel   
    )
  )
  #close the UI definition  
)

###############################################
# Server                                      #
###############################################

#define the SERVER 
server <- function(input, output) {
  
  #Defines "text_test" to be outputted in main panel
  output$text_test <- renderText({
    paste("Test:", input$test)
  })
  
  #Defines "results" to be outputted in main panel
  output$results <- renderTable(digits=4,{
    
    #extract input to be used in LoF function
    method <- input$test
    #formula <- 
    #Cluster <-
    #data <-
    
    data.frame(method=method)  #later replace this with below... or something similar
    #LoF(formula, cluster=Cluster, data=DF, method = method)
    
  }) 
  #close the server definition  
}


###############################################
# Launch App                                  #
###############################################

#generic line that launches the app
shinyApp(ui = ui, server = server)
