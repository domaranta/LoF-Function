########################################
#--------------------------------------#
# LoF Functions                        #
#--------------------------------------#
########################################
# Frost M22
# Dominic Maranta (Cal Poly)
# Advisor: Bret Holladay (Cal Poly)

########################################
# LoF                                  #
########################################

LoF <-function(formula, cluster = NULL, k = NULL, data, method, shiny = FALSE){
  
  if(length(all.vars(formula))>3){
    stop('function designed for <3 predictors')
  }
  if(method == "U" | method == "BR"){}
  else if(is.null(k)){
    if(shiny == FALSE){
      cluster = data[,deparse(substitute(cluster))]
    }else{
      cluster = data[,cluster]
    }
    if(is.numeric(cluster)== TRUE){
      cluster <- as.factor(cluster)
    }
  }
  #If a 1 predictor model
  if(length(all.vars(formula))==2){
    
    #Extract Y, X from formula
    Y=data[,all.vars(formula)[1]]
    X=data[,all.vars(formula)[2]]
    
    if(!is.null(k)){
      km <- kmeans(scale(X), k, nstart = 25)
      cluster <- as.factor(km$cluster)
    }
    
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
    if(method == 'C89'){
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
    #If method selected is Shillington
    if(method == 'S'){
      #Calculate each model information
      x.means=aggregate(X,by=list(cluster),mean)$x
      x.mean = NA
      for(i in 1:length(unique(cluster))){
        x.mean[cluster == i] = x.means[i]
      }
      
      
      model2 = lm(Y~x.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      model4 = lm(Y~X+cluster)
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      SSE_XZ=(anova(model4))$'Sum Sq'[length(anova(model4)$'Sum Sq')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      df_XZ=(anova(model4))$'Df'[length(anova(model4)$'Df')]
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_XZ
      F.stat=(SSE_X0-SSE_Z)/df1/(SSE_XZ/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is JSL
    if(method == 'JSL'){
      #Calculate each model information
      x.means=aggregate(X,by=list(cluster),mean)$x
      x.mean = NA
      for(i in 1:length(unique(cluster))){
        x.mean[cluster == i] = x.means[i]
      }
      
      model2 = lm(Y~x.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      model5 = lm(Y~X+cluster+X:cluster)
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      SSE_P=(anova(model5))$'Sum Sq'[length(anova(model5)$'Sum Sq')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      df_P=(anova(model5))$'Df'[length(anova(model5)$'Df')]
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_P
      F.stat=(SSE_X0-SSE_Z)/df1/(SSE_P/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Christensen '91
    if(method == 'C91'){
      #Calculate each model information
      x.means=aggregate(X,by=list(cluster),mean)$x
      x.mean = NA
      for(i in 1:length(unique(cluster))){
        x.mean[cluster == i] = x.means[i]
      }
      
      model1 = lm(formula, data)
      model2 = lm(Y~x.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_X-df_X0+df_Z
      F.stat=((SSE_X0-SSE_Z)/df1)/((SSE_X-SSE_X0+SSE_Z)/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Utts
    if(method == 'U'){
      #Calculate each model information
      fit <- lm(formula, data)
      hat <- hatvalues(fit)
      
      n = length(Y)

      clusterc = NA
      for(i in 1:n){
        if(hat[i]<=median(hat)){
          clusterc[i] = "C"
        }else{
          clusterc[i] = "O"
        }
      }
      clusterc = as.factor(clusterc)
      
      tempdf <- data.frame(data, clusterc)
      tempcdf <- subset(tempdf, subset = (clusterc == "C"))
  
      
      model1 = lm(formula, data)
      modelC = lm(formula, tempcdf) 

      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_C=(anova(modelC))$'Sum Sq'[length(anova(modelC)$'Sum Sq')]
      DF_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      DF_C=(anova(modelC))$'Df'[length(anova(modelC)$'Df')]
      
      #compute test values
      df1=DF_X-DF_C
      df2=DF_C
      F.stat=((SSE_X-SSE_C)/df1)/((SSE_C)/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If Burn and Ryan
    if(method == "BR"){
      Model <- lm(Y~X, data)
      Model_C <- lm(Y~X+(X>mean(X))+X:(X>mean(X)))
      SSE=(anova(Model))$'Sum Sq'[length(anova(Model)$'Sum Sq')]
      SSE_C=(anova(Model_C))$'Sum Sq'[length(anova(Model_C)$'Sum Sq')]
      df=(anova(Model))$'Df'[length(anova(Model)$'Df')]
      df_C=(anova(Model_C))$'Df'[length(anova(Model_C)$'Df')]
      
      df1 = df-df_C
      df2 = df_C
      F.stat = ((SSE-SSE_C)/df1)/((SSE_C)/df2)
      p.value = pf(F.stat, df1, df2,lower.tail=FALSE)
      
    }
  }
  
  #If a 2 predictor model
  if(length(all.vars(formula))==3){
    
    #Extract Y, X1, X2 from formula
    Y=data[,all.vars(formula)[1]]
    X1=data[,all.vars(formula)[2]]
    X2=data[,all.vars(formula)[3]]
    
    if(!is.null(k)){
      km <- kmeans(scale(matrix(c(X1, X2), ncol=2)), k, nstart = 25)
      cluster <- as.factor(km$cluster)
    }
    
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
    if(method == 'C89'){
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
    #If method selected is Shillington
    if(method == 'S'){
      #Calculate each model information
      x1.means=aggregate(X1,by=list(cluster),mean)$x
      x2.means=aggregate(X2,by=list(cluster),mean)$x
      x1.mean = NA
      x2.mean = NA
      for(i in 1:length(unique(cluster))){
        x1.mean[cluster == i] = x1.means[i]
        x2.mean[cluster == i] = x2.means[i]
      }
      
      
      model2 = lm(Y~x1.mean+x2.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      model4 = lm(Y~X1+X2+cluster)
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      SSE_XZ=(anova(model4))$'Sum Sq'[length(anova(model4)$'Sum Sq')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      df_XZ=(anova(model4))$'Df'[length(anova(model4)$'Df')]
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_XZ
      F.stat=(SSE_X0-SSE_Z)/df1/(SSE_XZ/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is JSL
    if(method == 'JSL'){
      #Calculate each model information
      x1.means=aggregate(X1,by=list(cluster),mean)$x
      x2.means=aggregate(X2,by=list(cluster),mean)$x
      x1.mean = NA
      x2.mean = NA
      for(i in 1:length(unique(cluster))){
        x1.mean[cluster == i] = x1.means[i]
        x2.mean[cluster == i] = x2.means[i]
      }
      
      
      model2 = lm(Y~x1.mean+x2.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      model5 = lm(Y~X1+X2+cluster+X1:cluster+X2:cluster)
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      SSE_P=(anova(model5))$'Sum Sq'[length(anova(model5)$'Sum Sq')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      df_P=(anova(model5))$'Df'[length(anova(model5)$'Df')]
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_P
      F.stat=(SSE_X0-SSE_Z)/df1/(SSE_P/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Christensen '91
    if(method == 'C91'){
      #Calculate each model information
      x1.means=aggregate(X1,by=list(cluster),mean)$x
      x2.means=aggregate(X2,by=list(cluster),mean)$x
      x1.mean = NA
      x2.mean = NA
      for(i in 1:length(unique(cluster))){
        x1.mean[cluster == i] = x1.means[i]
        x2.mean[cluster == i] = x2.means[i]
      }
      
      model1 = lm(formula, data)
      model2 = lm(Y~x1.mean+x2.mean, data) #does this allow for formula properly?
      model3 = lm(Y~cluster)
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_X0=(anova(model2))$'Sum Sq'[length(anova(model2)$'Sum Sq')]
      SSE_Z=(anova(model3))$'Sum Sq'[length(anova(model3)$'Sum Sq')]
      df_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      df_X0=(anova(model2))$'Df'[length(anova(model2)$'Df')]
      df_Z=(anova(model3))$'Df'[length(anova(model3)$'Df')]
      
      
      #compute test values
      df1=df_X0-df_Z
      df2=df_X-df_X0+df_Z
      F.stat=((SSE_X0-SSE_Z)/df1)/((SSE_X-SSE_X0+SSE_Z)/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If method selected is Utts
    if(method == 'U'){
      #Calculate each model information
      fit <- lm(formula, data)
      hat <- hatvalues(fit)
      
      n = length(Y)

      cluster = NA
      for(i in 1:n){
        if(hat[i]<=median(hat)){
          cluster[i] = "C"
        }else{
          cluster[i] = "O"
        }
      }
      cluster = as.factor(cluster)
      
      tempdf <- data.frame(data, cluster)
      tempcdf <- subset(tempdf, subset = (cluster == "C"))
      
      
      model1 = lm(formula, data)
      modelC = lm(formula, tempcdf) 
      
      SSE_X=(anova(model1))$'Sum Sq'[length(anova(model1)$'Sum Sq')]
      SSE_C=(anova(modelC))$'Sum Sq'[length(anova(modelC)$'Sum Sq')]
      DF_X=(anova(model1))$'Df'[length(anova(model1)$'Df')]
      DF_C=(anova(modelC))$'Df'[length(anova(modelC)$'Df')]
      #compute test values
      df1=DF_X-DF_C
      df2=DF_C
      F.stat=((SSE_X-SSE_C)/df1)/((SSE_C)/df2)
      p.value=pf(F.stat, df1, df2,lower.tail=FALSE)
    }
    #If Burn and Ryan
    if(method == "BR"){
      #X1
      Model <- lm(Y~X1+X2)
      Model_C <- lm(Y~X1+X2+(X1>mean(X1))+X1:(X1>mean(X1)))
      Model_I <- lm(Y~X1+X2+(X1>mean(X1))+X1:(X1>mean(X1))+X2:(X1>mean(X1)))
      SSE=(anova(Model))$'Sum Sq'[length(anova(Model)$'Sum Sq')]
      SSE_C=(anova(Model_C))$'Sum Sq'[length(anova(Model_C)$'Sum Sq')]
      SSE_I=(anova(Model_I))$'Sum Sq'[length(anova(Model_I)$'Sum Sq')]
      df=(anova(Model))$'Df'[length(anova(Model)$'Df')]
      df_C=(anova(Model_C))$'Df'[length(anova(Model_C)$'Df')]
      df_I=(anova(Model_I))$'Df'[length(anova(Model_I)$'Df')]
      
      #X1 Curvature
      df11C=df-df_C
      df21C=df_I
      F1C=((SSE-SSE_C)/df11C)/((SSE_I)/df21C)
      p.value1C = pf(F1C, df11C, df21C,lower.tail=FALSE)
      #X1 Interaction
      df11I=df_C-df_I
      df21I=df_I
      F1I=((SSE_C-SSE_I)/df11I)/((SSE_I)/df21I)
      p.value1I =pf(F1I, df11I, df21I,lower.tail=FALSE)
      
      #X2
      Model <- lm(formula, data)
      Model_C <- lm(Y~X1+X2+(X2>mean(X2))+X2:(X2>mean(X2)))
      Model_I <- lm(Y~X1+X2+(X2>mean(X2))+X2:(X2>mean(X2))+X1:(X2>mean(X2)))
      SSE=(anova(Model))$'Sum Sq'[length(anova(Model)$'Sum Sq')]
      SSE_C=(anova(Model_C))$'Sum Sq'[length(anova(Model_C)$'Sum Sq')]
      SSE_I=(anova(Model_I))$'Sum Sq'[length(anova(Model_I)$'Sum Sq')]
      df=(anova(Model))$'Df'[length(anova(Model)$'Df')]
      df_C=(anova(Model_C))$'Df'[length(anova(Model_C)$'Df')]
      df_I=(anova(Model_I))$'Df'[length(anova(Model_I)$'Df')]
      
      #X2 Curvature
      df12C=df-df_C
      df22C=df_I
      F2C=((SSE-SSE_C)/df12C)/((SSE_I)/df22C)
      p.value2C = pf(F2C, df12C, df22C,lower.tail=FALSE)
      #X1 Interaction
      df12I=df_C-df_I
      df22I=df_I
      F2I=((SSE_C-SSE_I)/df12I)/((SSE_I)/df22I)
      p.value2I = pf(F2I, df12I, df22I,lower.tail=FALSE)
      data1 = data.frame(Test = c("X1 Curve", "X1 Interaction", "X2 Curve", "X2 Interaction"),F = c(F1C, F1I, F2C, F2I), Df1 = c(df11C, df11I,df12C,df12I),Df2 = c(df21C, df21I,df22C,df22I), p.value = c(p.value1C, p.value1I, p.value2C, p.value2I))
      return(data1)
    }
  }
  
  return(data.frame(F=F.stat,Df1=df1, Df2=df2, p.value=p.value))
}
#=======================================
#Examples
#LoF(Sale.Price~Age, data=FCData, method = 'U')
#LoF(Sale.Price~Age+Square.Feet, data=FCData, method = 'U')
#LoF(Volume~Girth+Height, data = trees, method = 'BR')
#LoF(Volume~log(Girth)+Height, data = trees, method = 'BR')


#LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'SY')
#LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData, method = 'SY')
#LoF(Sale.Price~Square.Feet+Age, cluster="Cluster", data=FCData, method = 'SY', shiny = TRUE)
#LoF(Sale.Price~Square.Feet+Age, k=5, data=FCData, method = 'SY')
#LoF(Sale.Price~Age, cluster = Cluster,k = 0, data=FCData, method = 'SY')

#LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'AR')
#LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData,method = 'AR')

#LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'C89')
#LoF(Sale.Price~Square.Feet+Age, cluster=Cluster, data=FCData,method = 'C89')


#LoF(Sale.Price~Age, cluster=Cluster, data=FCData, method = 'U')
