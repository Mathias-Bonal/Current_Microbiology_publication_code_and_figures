## Pearson correlation test on growth rates vs carrying capacities values

# Set of working directory
setwd("~/R/Data/stats_mu_K")

# Load packages
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(readxl)


# Load files of interest
d.location="both_means.txt"
both_means=read.table(d.location,header=TRUE)

d.location="both_triplicates.txt"
both_triplicates=read.table(d.location,header=TRUE)

d.location="cultures_means.txt"
cultures_means=read.table(d.location,header=TRUE)

d.location="cultures_triplicates.txt"
cultures_triplicates=read.table(d.location,header=TRUE)



## Pearson correlation test including colors + shapes + error bars



## mu mono vs K bi and quadri (= co) ##

mu_K_co <- read_excel("both_means.xlsx", sheet="co")

plot_mu_K_co <- ggscatter(mu_K_co, x = "mu.mono", y = "lnK.co",
                          add = "reg.line", conf.int = TRUE, cor.coef.size = 5,
                          cor.coef = TRUE, cor.method = "pearson", cor.coef.coord=c(0.25,25), 
                          color="strain", shape="culture", size=4,
                          palette=c("darkorange2","gray47","green3","purple4"),
                          add.params=list(color="black", fill="lightgray", shape=18), 
                          xlim=c(0.2,0.67), ylim=c(12,26),
                          xlab = expression(bold(paste("growth rate in monoculture (", h^-1, ")",sep=""))),
                          ylab = "carrying capacity in co-culture [ln(cells/mL)]") +
  geom_point(fill=c("darkorange2","gray47","green3","purple4", "darkorange2","gray47","green3","purple4",
                    "darkorange2","gray47","green3","purple4", "darkorange2","gray47","green3","purple4"), alpha=5) +
  scale_shape_manual(
    #name="shape", 
    values = c(5, 7, 25, 15, 16, 17, 11)) +
  geom_errorbar(aes(x=mu.mono, y=lnK.co, ymin=lnK.co-sdK, ymax=lnK.co+sdK), 
                width=0.0025, colour="cornflowerblue", alpha=0.7, size=0.5) +
  geom_errorbarh(aes(xmin=mu.mono-sdmu, xmax=mu.mono+sdmu), colour="cornflowerblue", alpha=0.7, size=0.5) +
  theme(
    axis.title.x = element_text(#color="burlywood4",  
      size=16, face="bold"),
    axis.title.y = element_text(#color="cornflowerblue",  
      size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.position = c(1,0.01),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

plot_mu_K_co

ggsave(file="plot_mu_K_co.svg", plot=plot_mu_K_co, width=15, height=7.5)




## mu vs K in bi, quadri and mix ##

# with error bars

co_means <- read_excel("both_means.xlsx", sheet="bi_quadri_mix")

plot_co_means <- ggscatter(co_means, x = "mu", y = "lnK",
                           add = "reg.line", conf.int = TRUE, cor.coef.size = 5,
                           cor.coef = TRUE, cor.method = "pearson", cor.coef.coord=c(0.25,25),
                           color="strain", shape="culture",
                           palette=c("darkorange2","gray47","green3","purple4"),
                           add.params=list(color="black", fill="lightgray", shape=18),
                           xlim=c(0.1,0.6), ylim=c(12,26),
                           xlab = expression(bold(paste("growth rate (", h^-1, ")",sep=""))),
                           ylab = "carrying capacity [ln(cells/mL)]") +
  geom_errorbar(aes(x=mu, y=lnK, ymin=lnK-sdK, ymax=lnK+sdK), 
                width=0.0025, colour="cornflowerblue", alpha=0.7, size=0.5) +
  geom_errorbarh(aes(xmin=mu-sdmu, xmax=mu+sdmu), colour="cornflowerblue", alpha=0.7, size=0.5) +
  theme(
    axis.title.x = element_text(#color="burlywood4",  
      size=16, face="bold"),
    axis.title.y = element_text(#color="cornflowerblue",  
      size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.position = c(1,0.01),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

plot_co_means




## arrange mu_K_co + co_means ##

two_graphs <- ggarrange(plot_mu_K_co, plot_co_means,
                        #labels = c("A", "B"),
                        #label.x=0.1,
                        ncol = 2, nrow = 1)
two_graphs

final_graphs <- annotate_figure(two_graphs, top = text_grob(c("a", "b"), size=20, face="bold", x=c(0.07, 0.57)))
final_graphs

ggsave(file="final_graphs_error_bars.svg", plot=final_graphs, width=15, height=7.5)
ggsave(file="final_graphs_error_bars.png", plot=final_graphs, width=15, height=7.5)