library(MASS)
library(rcompanion)
attach(diet_data)
library(car)


onHazirlik <- function(dataSet){
  
  orj_data = dataSet  
  normalDagilir =  norm_test(dataSet)
  
  if(normalDagilir==FALSE){
    cat("\n")
    cat("  Veriler Transforme Edildi")
    dataSet = transformTukey(dataSet)
    normalDagilir =  norm_test(dataSet)
  }
  
  if(normalDagilir==TRUE){
    return(dataSet)
  }
  
  if(normalDagilir==FALSE){
    cat("\n")
    cat("  Veriler Normal Dagilmiyor")
    return(orj_data)
  }
  
}

norm_test <- function(dataSet){
  
  normalDagilir = FALSE
  
  nortest <- shapiro.test(dataSet)
  
  if(nortest$p.value>0.05){
    cat("Null hipotez kabul edilir: Veriler Normal Dagilir")
    normalDagilir = TRUE
  }else{
    cat("Alternatif hipotez kabul edilir: Veriler Normal Dagilmaz")
  }
  
  return(normalDagilir)
}

varyans_test <- function(data1,data2){
  
  varState = FALSE # FALSE : Not Equal
  VarTest = var.test(data1,data2)
  if(VarTest$p.value > 0.05){
    cat(VarTest$p.value)
    varState=TRUE
    return(varState)
    
  }else{
    return(varState)
  }
  
  
}


names(diet_data)<-c("weightLoss","dietType")



leveneTest(diet_data$weightLoss~diet_data$dietType)

anova_test_sonucu <- aov(diet_data$weightLoss~diet_data$dietType) 
summary(anova_test_sonucu)

boxplot(diet_data$weightLoss~diet_data$dietType)

tukey_sonuc = TukeyHSD(anova_test_sonucu)
plot(tukey_sonuc)










