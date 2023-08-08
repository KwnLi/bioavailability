# package checking and installation
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

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
    ylim(c(0,100)) +
    theme_bw()
}


step2_plot <- function(step2.output){
  errortype <- ifelse(median(step2.output$step2$frcAct) > 0, "Type 1", "Type 2")
  
  ggplot(step2.output$step2, aes(x = 100*frcAct, y = err_pb*100)) + 
    geom_line() + geom_point() + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    xlab(paste("% ", ifelse(errortype == "Type 1", "above", "below"), " action level", sep = "")) +
    geom_hline(yintercept = ifelse(errortype == "Type 1", 5, 20), color = "red") + 
    ggtitle(
      paste(errortype, " error with ", ifelse(errortype=="Type 1", "increasing", "decreasing"),
            "\nbioavailable ", step2.output$AsPb,
            ifelse(errortype=="Type 1", " above", " below"), " the action level",
                  sep = ""),) + 
    ylim(c(0,100)) +
    theme_bw()
}
