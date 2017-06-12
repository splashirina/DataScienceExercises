
#Loading Libraries

if(!require(dplyr)) {
  install.packages("dplyr")
}
if(!require(pwr)){
  install.packages("pwr")
}
if(!require(FSA)){
  install.packages("FSA")
  install.packages("dunn.test")
}
library(dplyr)
library(pwr)
library(FSA)

## Defining Functions

MVT <- function(file,s.level) {

  test <- read.csv(file,stringsAsFactors = FALSE)
  test$Impression <- as.integer(test$Impression)
  test$Click <- as.integer(test$Click)
  test$ctr <- round(test$Click/test$Impression,4)
  result <- list()
  temp.filer <- NULL

  test.for.homoscedasticity <- bartlett.test(ctr ~ Variation,test)
  if(test.for.homoscedasticity$p.value < 1-s.level) {
    homoscedasticity <- "The variance among groups is not equal. Equal variance assumption does not hold.,use alternatives."
    aovs<-oneway.test(ctr~Variation,test,var.equal = FALSE)
    anova.pv <- avos[[3]]
  
  } else {
    homoscedasticity <- "The variance among groups is equal. Equal variance assumption is satisfied."
    aovs<-aov(ctr~Variation,test)
    anova.pv <- (aovs %>% summary())[[1]]$`Pr(>F)`[1]
  }

  if(anova.pv>=0.05) {
    anova.result <- "there is not statistically siginificant difference regarding CTR among the variants tested."
  } else {
    if(test.for.homoscedasticity$p.value < 1-s.level) {
      post.hoc <- dunnTest(ctr~factor(Variation),test,method="bh")
      temp <- post.hoc$res
      temp.filter <- temp %>% filter(P.adj<1-s.level)
    } else {
      post.hoc<-TukeyHSD(aovs,conf.level = s.level)
      temp <- post.hoc$Variation %>% as.data.frame()
      names(temp) <- c("Ad Copy","Z-Value","P-Value(Unadjusted)","P-Value(Adjusted)")
      temp.filter <- temp %>% filter(`p adj`<1-s.level)
    }
  }
  result[['homoscedasticity']] <- homoscedasticity
  result[['anova.p.value']] <- anova.pv
  result[['table']] <- temp.filter
  return(result)
}


