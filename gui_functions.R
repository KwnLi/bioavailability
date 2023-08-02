library(ggplot2)
library(egg)

step1_plot <- function(step1.output){
  frcAct <- step1.output$step1$frcAct[1]
  errortype <- ifelse(frcAct > 0, "Type 1", "Type 2")
  
  ggplot(step1.output$step1, aes(x = n_tot, y = err_pb*100)) + 
    geom_line() + geom_point() + 
    xlab("Num. samples analyzed (total concentration)") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "Type 1", 5, 20), color = "red") + 
    ggtitle(paste(errortype, " error with more sampling, when true \nbioavailable ", step1.output$AsPb, " is ",
                  round(abs(frcAct*100),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), 
                  " action level",
                  sep = ""),) + 
    theme_bw()
}
