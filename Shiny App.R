###############################################
# Preliminaries                               #
###############################################

library(shiny) 
library(cluster)
library(factoextra)

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
      numericInput("k", label = h3("K-Means Cluster Input:"), value = NULL, min = 2),
      div("Leave blank if cluster column is already specified above.",align="center", style = "font-size: 8pt"),
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
      tableOutput('results'),
      
      tabsetPanel(
        tabPanel("Input Table", tableOutput("table")), 
        tabPanel("Clustering", plotOutput("clusterplot"), verbatimTextOutput("clusterwarning")), 
        tabPanel("Results",tableOutput("lof"), verbatimTextOutput("pvalwarning"), plotOutput("clusterfinal"))
      )
        
    )
  )
)

###############################################
# Server                                      #
###############################################

server <- function(input, output) {
  
  #Defines "text_test" to be outputted in main panel
  output$clusterwarning <- renderText({
    "*Optimal number of clusters occurs where the elbow or beginning of less change in SSE becomes apparent"
  })
  output$pvalwarning <- renderText({
    "*Only the left tail p-value is shown, for right tail p-value subtract p-value from one. A small right tail p-value can be indicative of within cluster lack of fit"
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
  
  output$clusterplot <- renderPlot({
    req(input$data)
    df <- read.csv(input$data$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    formula1 <- eval(parse(text = input$formula))
    if(length(all.vars(formula1))==2){
      X=df[,all.vars(formula1)[2]]
      fviz_nbclust(scale(matrix(X)),kmeans,method = "wss")
      #plot(X)
    }
    if(length(all.vars(formula1))==3){
      X1=df[,all.vars(formula1)[2]]
      X2=df[,all.vars(formula1)[3]]
      fviz_nbclust(scale(matrix(c(X1, X2), ncol=2)),kmeans,method = "wss")
    }
    })
    output$clusterfinal <- renderPlot({
      req(input$data)
      req(input$k)
      df <- read.csv(input$data$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      formula1 <- eval(parse(text = input$formula))
      if(length(all.vars(formula1))==2){
        Y=df[,all.vars(formula1)[1]]
        X=df[,all.vars(formula1)[2]]
        km <- kmeans(scale(X), input$k, nstart = 25)
        fviz_cluster(km, data = data.frame(X, Y))
        #plot(X,Y)
      }
      if(length(all.vars(formula1))==3){
        X1=df[,all.vars(formula1)[2]]
        X2=df[,all.vars(formula1)[3]]
        km <- kmeans(scale(matrix(c(X1, X2), ncol=2)), input$k, nstart = 25)
        fviz_cluster(km,data = data.frame(X1, X2), xlab = all.vars(formula1)[2], ylab = all.vars(formula1)[3])
      }
    
   }
    )
  
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
    k <- input$k
    data <- if(is.null(input$data)){"Enter Data"}else{sub(".csv$", "", basename(input$data$name))}
    
    
    data.frame(Method=method1, Formula = formula1, Cluster = cluster1, k = k, Data = data)
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
    #if(is.null(input$k)){k1 = NULL}else{k1 = input$k}
    k1 = NULL
    if(is.numeric(input$k)){
      k1 = input$k
    }
    LoF(formula=eval(parse(text = input$formula)), cluster=input$cluster, k = k1, data=data1, method = method1, shiny = TRUE)
  }) 
}


###############################################
# Launch App                                  #
###############################################

shinyApp(ui = ui, server = server)
