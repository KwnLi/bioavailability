library(tidyverse)
library(egg)
library(cowplot)

below <- read.csv("Below25plotdata.csv")[-1,]
above <- read.csv("Above25plotdata.csv")[-1,]

plotbelow <- ggplot(below, aes(pc.actlvl, probability)) + 
  geom_line() + ylab("False Exceedance Probabiity (%)") +
  xlab("True RBA-adjusted soil [Pb] relative to the unadjusted AL (%)") +
  geom_hline(yintercept = 20, color = "red") +
  theme_article()
  # theme(axis.title.x = element_blank())

plotabove <- ggplot(above, aes(pc.actlvl, probability)) + 
  geom_line() + ylab("False Compliance Probabiity (%)") +
  xlab("True RBA-adjusted soil [Pb] relative to the unadjusted AL (%)") +
  geom_hline(yintercept = 5, color = "red") +
  # scale_y_continuous(position = 'right', sec.axis = dup_axis()) + 
  theme_article()
  # theme(axis.title.x = element_blank())
        # axis.title.y.left = element_blank())

bothplots <- plot_grid(plotbelow, plotabove, labels = c("a", "b"))

ggsave("Figure3.png", bothplots, height = 4, width = 10, dpi=300, units="in",
       bg="white")

hista <- readRDS("Fig4a.rds")
hista.iter = hista$sim_attributes$iter
hista.ba = hista$sim_attributes$samp.prd_ba[1:hista.iter,]

hisb <- readRDS("Fig4b.rds")
histb.iter = hisb$sim_attributes$iter
histb.ba = hisb$sim_attributes$samp.prd_ba[1:histb.iter,]

fig4a <- ggplot(hista.ba, aes(ba_DU)) + geom_histogram(fill="cornsilk3") +
  xlab("Predicted bioavailability [mg/kg]") + ylab("Result frequnecy") +
  theme_article()

fig4b <- ggplot(histb.ba, aes(ba_DU)) + geom_histogram(fill="cornsilk3") +
  xlab("Predicted bioavailability [mg/kg]") + ylab("Result frequnecy") +
  theme_article()

fig4 <- plot_grid(fig4a, fig4b, labels = c("a", "b"))

ggsave("Figure4.png", fig4, height = 4, width = 10, dpi=300, units="in",
       bg="white")

# write.csv(hista.ba, "Fig4a_data.csv", row.names = FALSE)
# write.csv(histb.ba, "Fig4b_data.csv", row.names = FALSE)
# write.csv(below, "Fig3a_data.csv", row.names = FALSE)
# write.csv(above, "Fig3b_data.csv", row.names = FALSE)
