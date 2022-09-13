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
      textInput("formula", label = h3("Formula Input:"), value = ""),
      
      hr(),
      #Cluster input
      textInput("cluster", label = h3("Cluster Input:"), value = ""),
      
      
      hr(),
      numericInput("k", label = h3("K-Means Cluster Input:"), value = NULL, min = 2),
      div("Leave blank if cluster column is already specified above.",align="center", style = "font-size: 8pt"),
      hr(),
      #Test selection drop-down
      #h5 is the 5th level of a hiearchy of decreasing subheadings;i.e.subsubsubsubheading
      selectInput(inputId="test", 
                  label=h3("Test:"), 
                  choices = list("Christensen '89","Atwood & Ryan", "Su & Yang", "Shillington", "JSL", "Christensen '91", "Utts", "Burn & Ryan")),
      
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
        tabPanel("Information", div("This R Shiny App allows for lack of fit testing without the assumption of residuals. Eight different methods are provided for lack of fit testing from various journals. 
Data input is provided first which accepts txt, tsv, or csv files. The user specifies the desired regression formula using a maximum of two predictor variables in the formula form Y~X1+X2 or any variation of this.
If the dataset has a prespecified cluster form the column name for the cluster can be specified under the cluster input. This allows the methods to operate around these clusters.
If the data does not have a cluster column, the user may specify the number of clusters for k-means clustering. For assistance, under the clustering tab, once the data has been entered, there is a plot showing the optimal number of clusters for k-means clustering."), 
                 br(),
div("The user may then specify the desired method."), div("Christensen (1989) compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with the same slope in each cluster. Good for small sample sizes but does not account for within-cluster variation."),
div("Atwood and Ryan (1976) compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with varying slopes in each cluster. Better for within-cluster variation of the first three methods, and still allows for moderate sized samples."),
div("Su and Yang (2006) compares the fitted model specified in the formula with a full model approximation using clustering to plot second order models in each cluster. Powerful for both between cluster lack of fit and within cluster lack of fit, requires larger sample. "),
div("Shillington (1979).This method compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with the same slope in each cluster, the same full model as Christensen '89. "),
div("Joglekar, Schuenemyer, LaRiccia (1989) compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with different slopes in each cluster, the same full model as Atwood and Ryan. ")  ,   
div("Christensen (1991) compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by comparing the fitted regression model to the regression model of the cluster means and adjusting with the cluster means themselves. This test is UMPI for orthogonal lack of fit. "),
div("Utts (1982) compares the mean square lack of fit estimate of residual variance determined by comparing the overall fitted model to a model fitting a linear model to the central cluster, determined within the algorithm, as the centermost half of observations, and fitting the outside points exactly. This variance estimate is then compared to the mean square variance estimate from the pure error by the same model fitting a line to the central cluster and exact fitting the other observations. Does not require cluster input."),
div("Burn and Ryan (1983) examines each predictor individually for curvature and interaction lack of fit. One predictor case utilizes only the curvature test while more predictors produce 2k total tests, 2 for each variable. The user should use the Bonferonni adjustment, and note that a rejection in any of the tests indicates a lack of fit. It does not need cluster specification. Additionally, Burn and Ryan can only be ran for simple additive models at this time (no transformations or higher order models)."),
br(),
div("The tests and code are further expanded on my GitHub Page: https://github.com/domaranta/LoF-Function"),
div("The FCData found in the above GitHub repository works as a strong example. This dataset includes a clustering column, along with variables for home age, square feet and the sales price of the home. By default, the app shows the formula, and clustering so the user can see how the app is setup and where the results and information are. ")
),

        tabPanel("Clustering", plotOutput("clusterplot"), verbatimTextOutput("clusterwarning")), 
        tabPanel("Results",tableOutput("lof"), verbatimTextOutput("br"),verbatimTextOutput("pvalwarning"), tabsetPanel(tabPanel("Column Clustering",plotOutput("clusterfinalc")),tabPanel("K Clustering",plotOutput("clusterfinal")))),
        tabPanel("References", 
                 div("Atwood, C., and Ryan, T. (1976), 'A Class of Tests for Lack of Fit to a Regression Model,' unpublished manuscript.
"),
br(),
div('Burn, D. A. & Ryan, Jr. T. A. (1983) A diagnostic test for lack of fit in regression models, American Statistical Association, Proceedings of the Statistical Computing Section, pp. 286–290.'),
br(),
div('Christensen, R. (1989), "Lack-of-Fit Test Based on Near or Exact Replicates," The Annals of Statistics, 17, 673-683.'),
br(),
div('Christensen, R. (1991), "Small-Sample Characterizations of Near-Replicate Lack-of-Fit Tests," Journal of the American Statistical Association, 88, 752-756.'),
br(),
div('Joglekar, G., Schuenemeyer, J. H., and LaRicca, V. (1989), "Lack-of-Fit Test- ing When Replicates Are Not Available," The American Statistician, 43, 125-143.'),
br(),
div('Shillington, E. R. (1979), "Testing Lack of Fit in Regression Without Replica- tion," Canadian Journal of Statistics, 7, 137-146.'),
br(),
div('Su, Z., & Yang, S.-S. (2006). A Note on Lack-of-Fit Tests for Linear Models without Replication. Journal of the American Statistical Association, 101(473), 205–210.'),
br(),
div('Utts, J. M. (1982) The rainbow test for lack of fit in regression, Communications in Statistics, Series A, 11, pp. 2801–2815.')
                 )
      )
        
    )
  )
)

###############################################
# Server                                      #
###############################################

server <- function(input, output) {
  
  #Defines "text_test" to be outputted in main panel
  output$info <- renderText({
    "This R Shiny App allows for lack of fit testing without the assumption of residuals. Eight different methods are provided for lack of fit testing from various journals. 
     Data input is provided first which accepts txt, tsv, or csv files. The user specifies the desired regression formula using a maximum of two predictor variables in the formula form Y~X1+X2 or any variation of this.
     If the dataset has a prespecified cluster form the column name for the cluster can be specified under the cluster input. This allows the methods to operate around these clusters.
     If the data does not have a cluster column, the user may specify the number of clusters for k-means clustering. For assistance, under the clustering tab, once the data has been entered, there is a plot showing the optimal number of clusters for k-means clustering.
     The user may then specify the desired method.
     Christensen (1989) compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with the same slope in each cluster. Good for small sample sizes but does not account for within-cluster variation.
     Atwood and Ryan (1976) compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with varying slopes in each cluster. Better for within-cluster variation of the first three methods, and still allows for moderate sized samples.
     Su and Yang (2006) compares the fitted model specified in the formula with a full model approximation using clustering to plot second order models in each cluster. Powerful for both between cluster lack of fit and within cluster lack of fit, requires larger sample. 
     Shillington (1979).This method compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with the same slope in each cluster, the same full model as Christensen '89. 
     Joglekar, Schuenemyer, LaRiccia (1989) compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with different slopes in each cluster, the same full model as Atwood and Ryan. 
     Christensen (1991) compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by comparing the fitted regression model to the regression model of the cluster means and adjusting with the cluster means themselves. This test is UMPI for orthogonal lack of fit. 
     Utts (1982) compares the mean square lack of fit estimate of residual variance determined by comparing the overall fitted model to a model fitting a linear model to the central cluster, determined within the algorithm, as the centermost half of observations, and fitting the outside points exactly. This variance estimate is then compared to the mean square variance estimate from the pure error by the same model fitting a line to the central cluster and exact fitting the other observations. Does not require cluster input.   
     Burn and Ryan (1983) examines each predictor individually for curvature and interaction lack of fit. One predictor case utilizes only the curvature test while more predictors produce 2k total tests, 2 for each variable. The user should use the Bonferonni adjustment, and note that a rejection in any of the tests indicates a lack of fit. It does not need cluster specification. Additionally, Burn and Ryan can only be ran for simple additive models at this time (no transformations or higher order models)."
  })
  output$clusterwarning <- renderText({
    "*Optimal number of clusters occurs where the elbow or beginning of less change in SSE becomes apparent"
  })
  output$pvalwarning <- renderText({
    "*Only the right tail p-value is shown, for left tail p-value subtract p-value from one. A small left tail p-value can be indicative of within cluster lack of fit"
  })
  output$br <-renderText({
    if(input$test=="Burn & Ryan"){
      "For two predictor Burn and Ryan test, use the Bonferonni adjustment on the four tests. 
      Additionally, Burn and Ryan can only be ran for simple additive models at this time (no transformations or higher order models)."
    }
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
    req(input$formula)
    df <- read.csv(input$data$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    formula1 <- eval(parse(text = input$formula))
    if(length(all.vars(formula1))==2){
      X=df[,all.vars(formula1)[2]]
      fviz_nbclust(scale(X),kmeans,method = "wss")
    }
     else if(length(all.vars(formula1))==3){
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
        fviz_cluster(km, data = data.frame(X, Y),xlab = all.vars(formula1)[2], ylab = paste(all.vars(formula1)[1], " - Y-variable for plotting purposes only"))
      }
       else if(length(all.vars(formula1))==3){
        X1=df[,all.vars(formula1)[2]]
        X2=df[,all.vars(formula1)[3]]
        km <- kmeans(scale(matrix(c(X1, X2), ncol=2)), input$k, nstart = 25)
        fviz_cluster(km,data = data.frame(X1, X2), xlab = all.vars(formula1)[2], ylab = all.vars(formula1)[3])
      }
    
   }
    )
  
    output$clusterfinalc <- renderPlot({
      req(input$data)
      req(input$cluster)
      #!req(input$k)
      df <- read.csv(input$data$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      formula1 <- eval(parse(text = input$formula))
      Cluster <- as.factor(df[,input$cluster])
      if(length(all.vars(formula1))==2){
        Y=df[,all.vars(formula1)[1]]
        X=df[,all.vars(formula1)[2]]
       # ggplot(aes(x= X, y = Y, col = cluster1))
        qplot(x= X1, y = Y, color = Cluster, main = "Cluster Plot",xlab = all.vars(formula1)[2], ylab = paste(all.vars(formula1)[1], " - Y-variable for plotting purposes only"))
      }
      else if(length(all.vars(formula1))==3){
        X1=df[,all.vars(formula1)[2]]
        X2=df[,all.vars(formula1)[3]]
        qplot(x= X1, y = X2, color = Cluster, main = "Cluster Plot",xlab = all.vars(formula1)[2], ylab = all.vars(formula1)[3])
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
    }else if(input$test=="Burn & Ryan"){
      method = "BR"
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
    else if(input$test=="Burn & Ryan"){
      method = "BR"}
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
