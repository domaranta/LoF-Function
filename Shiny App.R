###############################################
# Preliminaries                               #
###############################################

# import required libraries
library(shiny) 

#source required R scripts 
source('LoF Function.R', encoding = 'UTF-8')


###############################################
# User Interface (UI)                         #
###############################################

#Define the UI 
ui <- fluidPage(
  
  # app title
  titlePanel("Lack of Fit Test without Replicates"),
  
  
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
    
      # Copy the line below to make a file upload manager
      fileInput("data", label = h3("Import CSV File",align="left"), accept =  c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
        
      hr(),
      checkboxInput("header", "Header", TRUE),
      hr(),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
    hr(),
    
      #Make a way for the user to specify desired formula (e.g. y~sqrt(x1)+x2+I(x2^2))
      textInput("formula", label = h3("Formula Input:"), value = "Sale.Price~Square.Feet+Age"),
      
      hr(),
      #Make a way for the user to specify cluster
      textInput("cluster", label = h3("Cluster Input:"), value = "Cluster"),
      
      hr(),
      #Test selection drop-down
      #h5 is the 5th level of a hiearchy of decreasing subheadings;i.e.subsubsubsubheading
      selectInput(inputId="test", 
                  label=h3("Test:"), 
                  choices = list("Christensen '89","Atwood & Ryan", "Su & Yang", "Shillington", "JSL", "Christensen '91", "Utts")),
      
      #submit button
      div(submitButton("Submit"),align="right"),br(), 
      
      #Contact Info
      div("Lack of Fit Shiny App Without Replicates",align="center", style = "font-size: 8pt"),
      div("maintained by",
          #replace with your name and for webpage you can use email or your 
          # own webpage if you have one (e.g. github)
          a(href="https://github.com/domaranta",target="_blank",
            "Dominic Maranta dmaranta@calpoly.edu"),align="center", style = "font-size: 8pt"), br(), br(),
      
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
      tableOutput("results"),
      
      tableOutput("lof"),
      #close main panel
      
    
      # Create a new row for the table.
      
      tableOutput('table')
        
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
  output$table <- renderTable({
    df <- read.csv(input$data$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  output$formula <- renderPrint({ input$formula })
  output$cluster <- renderPrint({ input$cluster })
  
  #Defines "results" to be outputted in main panel
  output$results <- renderTable(digits=4,{
    
    #extract input to be used in LoF function
    method <- if(input$test=="Christensen '89"){
      method="C89"
    } else if(input$test=="Atwood & Ryan"){
      method ="AR"
    } else if(input$test=="Su & Yang"){
      method = "SY"
    }else if(input$test=="Shillington"){
      method = "S"
    }else if(input$test=="Christensen '91"){
      method = "C91"
    }else if(input$test=="JSL"){
      method = "JSL"
    }else if(input$test=="Utts"){
      method = "U"
    }
    formula <- input$formula
    Cluster <- input$cluster
    data <- if(is.null(input$data)){"Enter Data"}else{sub(".csv$", "", basename(input$data$name))}
    
    
    data.frame(method=method, formula = formula, cluster = Cluster, data = data)  #later replace this with below... or something similar
    #LoF(formula=formula, cluster=Cluster, data=DF, method = method)
  }) 
  output$lof <- renderTable(digits=4,{
    
    #extract input to be used in LoF function
    method <- if(input$test=="Christensen '89"){
      method="C89"
    } else if(input$test=="Atwood & Ryan"){
      method ="AR"
    } else if(input$test=="Su & Yang"){
      method = "SY"
    }else if(input$test=="Shillington"){
      method = "S"
    }else if(input$test=="Christensen '91"){
      method = "C91"
    }else if(input$test=="JSL"){
      method = "JSL"
    }else if(input$test=="Utts"){
      method = "U"
    }
    df <- read.csv(input$data$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)          
  
    formula <- input$formula
    Cluster <- input$cluster
    data <- if(is.null(input$data)){"Enter Data"}else{df}#sub(".csv$", "", basename(input$data$name))}
    #if(data == "Enter Data"){return(NULL)}else{
      LoF(formula=formula, cluster=Cluster, data=data, method = method)
      #}
  }) 
  #close the server definition  
}


###############################################
# Launch App                                  #
###############################################

#generic line that launches the app
shinyApp(ui = ui, server = server)
