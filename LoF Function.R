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

summary(lm(formula,data=FCData))

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

########################################
# LoF Su & Yang                        #
########################################

#LoF <-function(formula, cluster, data, method){
LoF.SY <-function(formula, cluster, data){
  
  #If a 1 predictor model
  if(length(all.vars(formula))==2){
    
    #Extract Y, X from formula
    Y=FCData[,all.vars(formula)[1]]
    X=FCData[,all.vars(formula)[2]]
    
    #If method selected is Su and Yang
    # if(method=='SY'){
    #   
    # }
    model1 = lm(formula, FCData)
    model6 = lm(Y~X*cluster+cluster:I(X^2))
    model6 = lm(Y~X*FCData$Cluster+FCData$Cluster:I(X^2))
    anova(model1, model6)
    df1=
      df2=
      F.stat=
      p.value=
  }
  
  #If a 2 predictor model
  if(length(all.vars(formula))==3){
    
    #Extract Y, X1, X2 from formula
    Y=FCData[,all.vars(formula)[1]]
    X1=FCData[,all.vars(formula)[2]]
    X2=FCData[,all.vars(formula)[3]]
    
    formula = Sale.Price~Age+Square.Feet
    model1 = lm(formula, FCData)
    model6 = lm(Y~X1*cluster+cluster:I(X1^2)+cluster:I(X1^2))
    model6 = lm(Y~X1*X2*FCData$Cluster+FCData$Cluster:I(X1^2)+FCData$Cluster:I(X2^2))
    
    SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
    SSE_XW=(anova(model6))$'Sum Sq'[length(anova(model6)$'Sum Sq')]
    
    df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
    df_XW=(anova(model6))$'Df'[length(anova(model6)$'Df')]
      df1=df_X-df_XW
      df2=df_XW
      F.stat=(SSE_X-SSE_XW)/df1/(SSE_XW/df2)
      p.value=pf(F, df1, df2,lower.tail=FALSE)
  }
  
  return(data.frame(F=F.stat,df1=df1, df2=df2, p.value=p.value))
}

#=======================================
#Examples
LoF(Sale.Price~Age, cluster=Cluster, data=FCData)
LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData)
