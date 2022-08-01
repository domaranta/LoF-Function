###############################################
# Preliminaries                               #
###############################################

library(shiny) 

source('LoF Function.R', encoding = 'UTF-8')

###############################################
# User Interface (UI)                         #
###############################################

ui <- fluidPage(
  
  titlePanel("Lack of Fit Test without Replicates"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      # file input
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
    
      #Formula input
      textInput("formula", label = h3("Formula Input:"), value = "Sale.Price~Square.Feet+Age"),
      
      hr(),
      #Cluster input
      textInput("cluster", label = h3("Cluster Input:"), value = "Cluster"),
      
      hr(),
      numericInput("k", label = h3("K-Means Cluster Input:"), value = 0),
    
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
          a(href="https://github.com/domaranta",target="_blank",
            "Dominic Maranta dmaranta@calpoly.edu"),align="center", style = "font-size: 8pt"), br()
      
    ),
    
    mainPanel(
      
      
      tableOutput("results"),
      
      tableOutput("lof"),
    
      tableOutput('table')
        
    )
  )
)

###############################################
# Server                                      #
###############################################

server <- function(input, output) {
  
  #Defines "text_test" to be outputted in main panel
  output$text_test <- renderText({
    paste("Test:", input$test)
  })
  output$table <- renderTable({
    
    req(input$data)
    
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
  
  #Defines table "results" to be outputted in main panel
  output$results <- renderTable(digits=4,{
    
    method1 <- if(input$test=="Christensen '89"){
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
    formula1 <- input$formula
    cluster1 <- input$cluster
    data <- if(is.null(input$data)){"Enter Data"}else{sub(".csv$", "", basename(input$data$name))}
    
    
    data.frame(Method=method1, Formula = formula1, Cluster = cluster1, Data = data)
  }) 
  #Compute LoF results
  output$lof <- renderTable(digits=4,{
    
    req(input$data)
    
    method1 <- if(input$test=="Christensen '89"){
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
  
    
    data1 <- if(is.null(input$data)){"Enter Data"}else{df}
    
    LoF(formula=eval(parse(text = input$formula)), cluster=input$cluster, k = input$k, data=data1, method = method1, shiny = TRUE)
  }) 
}


###############################################
# Launch App                                  #
###############################################

shinyApp(ui = ui, server = server)
