library(MASS)
library(rcompanion)
attach(met_hist_2)


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
    varState=TRUE
    return(varState)
    
  }else{
    return(varState)
  }
  
  
}

#--------- tek örnek -------------

#15 bireyin sistolik kan basınçları ölcülmüstür 
#bireylere ait toplum parametresi olan 120mm/Hg’ ye eşit midir?

basınç <- c(117,125,118,140,104,120,127,113,112,128,119,126,104,130,134)

basınç = onHazirlik(basınç)

t.test(basınç,mu=120)


#--------- tek örnek -------------


#-----------------cift bagimsiz----------------------

#ABD'de ve Hindistanda 2018-2008 yılları arasında enerji sektöründe emisyon değerleri ölçülür
#Null hipotez = iki ülke değerleri eşit
#Alternatif = ABD'nin emisyon değerleri daha büyük

usa <- c(293.60,289.99,286.37,296.52,299.63,297.31,290.02,298.80,307.85,308.63)
india <- c(99.08,99.34,99.59,99.85,100.79,101.73,102.67,103.61,104.55,101.95)

usa = onHazirlik(usa)

india = onHazirlik(india)

var.test(usa,india)

var_state = varyans_test(usa,india)


t.test(usa,india,alternative="greater", var.equal=var_state)

boxplot(data.frame(usa,india))

#---------------cift bagimsiz------------------------

#---------------çift bagimli-------------------------

#20 tavşan ile gerçekleştirilen deneyde, 
#yara bandı ve dikiş yöntemlerinin yara kapandıktan 10 gün sonra ölçülen 
#germe mukavemeti değerleri üzerinde etkisi araştırılmıştır.

bant <- c(6.59,9.84 ,3.97,5.74,4.47,4.79,6.76,7.61,6.47,5.77,
       7.36,10.45,4.98,5.85,5.65,5.88,7.77,8.84,7.68,6.89)

dikis <- c(4.52,5.87,4.60,7.87,3.51,2.77,2.34,5.16,5.77,5.13,
        5.55,6.99,5.78,7.41,4.51,3.96,3.56,6.22,6.72,5.17)


bant = onHazirlik(bant)

dikis = onHazirlik(dikis)

var_state = varyans_test(bant,dikis)

t.test(bant,dikis,paired=TRUE,var.equal = var_state)


#---------------çift bagimli-------------------------








