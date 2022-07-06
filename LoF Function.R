########################################
#--------------------------------------#
# LoF Functions                        #
#--------------------------------------#
########################################
# Frost M22
# Dominic Maranta (Cal Poly)
# Advisor: Bret Holladay (Cal Poly)
########################################
# Import & format sample data          #
########################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(readr)
#FCData = read_table("FirstCityData.txt")
FCData = read_table2("FirstCityData.txt")
str(FCData)

#remove dollar signs from Sale.Price column
FCData$Sale.Price = as.numeric(gsub("[\\$,]", "", FCData$Sale.Price))

#Change Cluster to type factor
FCData$Cluster = factor(FCData$Cluster)
str(FCData)

########################################
# Experimenting with formulas          #
########################################
# references
# https://www.datacamp.com/tutorial/r-formula-tutorial

formula<-Sale.Price~Age
formula
terms(formula)
all.vars(formula)
all.names(formula)
length(all.vars(formula))


formula<-Sale.Price~Age+I(Age^2)+log(Square.Feet)
all.vars(formula)
terms(formula)
tf<-terms(formula)
attr(tf,"variables")
noquote(all.vars(formula))

#How to extract Y, X1, X2 from formula
(Y=FCData[,all.vars(formula)[1]])
(X1=FCData[,all.vars(formula)[2]])
(X2=FCData[,all.vars(formula)[3]])

formula = Sale.Price~Age+Square.Feet+Cluster



########################################
# LoF Su & Yang                        #
########################################

LoF <-function(formula, cluster, data, method){
  cluster = data[,deparse(substitute(cluster))]
  #If a 1 predictor model
  if(length(all.vars(formula))==2){
    
    #Extract Y, X from formula
    Y=data[,all.vars(formula)[1]]
    X=data[,all.vars(formula)[2]]
    #cluster= data[,paste("",clusters)]
    #cluster = data$paste("",clusters)
    
    #If method selected is Su and Yang
    if(method=='SY'){
      #Calculate each model information
      model1 = lm(formula, data)
      model6 = lm(Y~X*cluster+cluster:I(X^2))
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_XW=(anova(model6))$'Sum Sq'[length(anova(model6)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_XW=(anova(model6))$'Df'[length(anova(model6)$'Df')]
      
      #compute test values
      df1=df_X-df_XW
      df2=df_XW
      F.stat=(SSE_X-SSE_XW)/df1/(SSE_XW/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Atwood and Ryan
    if(method == 'AR'){
      #Calculate each model information
      model1 = lm(formula, data)
      model5 = lm(Y~X+cluster+X:cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_P=(anova(model5))$'Sum Sq'[length(anova(model5)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_P=(anova(model5))$'Df'[length(anova(model5)$'Df')]
      
      #compute test values
      df1=df_X-df_P
      df2=df_P
      F.stat=(SSE_X-SSE_P)/df1/(SSE_P/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Christensen '89
    if(method == 'C'){
      #Calculate each model information
      model1 = lm(formula, data)
      model4 = lm(Y~X+cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_XZ=(anova(model4))$'Sum Sq'[length(anova(model4)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_XZ=(anova(model4))$'Df'[length(anova(model4)$'Df')]
      
      #compute test values
      df1=df_X-df_XZ
      df2=df_XZ
      F.stat=(SSE_X-SSE_XZ)/df1/(SSE_XZ/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    
  
  }
  
  #If a 2 predictor model
  if(length(all.vars(formula))==3){
    
    #Extract Y, X1, X2 from formula
    Y=data[,all.vars(formula)[1]]
    X1=data[,all.vars(formula)[2]]
    X2=data[,all.vars(formula)[3]]
    
    #If method selected is Su and Yang
    if(method=='SY'){
      #Calculate each model information
      model1 = lm(formula, data)
      model6 = lm(Y~X1*X2*cluster+cluster:I(X1^2)+cluster:I(X2^2))
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_XW=(anova(model6))$'Sum Sq'[length(anova(model6)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_XW=(anova(model6))$'Df'[length(anova(model6)$'Df')]
      
      #compute test values
      df1=df_X-df_XW
      df2=df_XW
      F.stat=(SSE_X-SSE_XW)/df1/(SSE_XW/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Atwood and Ryan
    if(method == 'AR'){
      model1 = lm(formula, data)
      model5 = lm(Y~X1+X2+cluster+X1:cluster+X2:cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_P=(anova(model5))$'Sum Sq'[length(anova(model5)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_P=(anova(model5))$'Df'[length(anova(model5)$'Df')]
      
      #compute test values
      df1=df_X-df_P
      df2=df_P
      F.stat=(SSE_X-SSE_P)/df1/(SSE_P/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Christensen '89
    if(method == 'C'){
      model1 = lm(formula, data)
      model4 = lm(Y~X1+X2+cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_XZ=(anova(model4))$'Sum Sq'[length(anova(model4)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_XZ=(anova(model4))$'Df'[length(anova(model4)$'Df')]
      
      #compute test values
      df1=df_X-df_XZ
      df2=df_XZ
      F.stat=(SSE_X-SSE_XZ)/df1/(SSE_XZ/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }

  }
  
  return(data.frame(F=F.stat,df1=df1, df2=df2, p.value=p.value))
}
#=======================================
#Examples
LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'SY')
LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData, method = 'SY')

LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'AR')
LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData,method = 'AR')

LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'C')
LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData,method = 'C')



