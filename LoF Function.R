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
    
    df1=
      df2=
      F.stat=
      p.value=
  }
  
  return(data.frame(F=F.stat,df1=df1, df2=df2, p.value=p.value))
}

#=======================================
#Examples
LoF(Sale.Price~Age, cluster=Cluster, data=FCData)
LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData)
