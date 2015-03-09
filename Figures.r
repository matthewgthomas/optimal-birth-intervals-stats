library(ggplot2)
library(plyr)
library(reshape)

# clear workspace
rm(list=ls())


########################################################################
## initialise stuff and import data
########################################################################
source("init.r")
source("import-ibi-data.r")


########################################################################
## Figure 1a : mortality curves for each population
########################################################################
source("siler.r")

mort.plots <- list()

mort.plots[[1]] <- ggplot(siler.melt, aes(age, value, group=factor(Population))) +
  geom_line(aes(colour=Population)) +

  scale_x_continuous("Age (years)", limits=c(0, 90)) +
  coord_cartesian(xlim = c(0, 90), ylim = c(0, 1)) +
  scale_y_continuous("Mortality hazard", limits=c(0,1)) +
  scale_colour_manual(values=cbPalette) +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=graph_text_size)
    ,axis.title=element_text(size=graph_text_size)
  ) +
  
  # title
  ggtitle("A") +
  theme(plot.title = element_text(hjust = -0.05, face="bold")) +
  
  # hide legend
  #theme(legend.position="none") +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))


########################################################################
## Figure 1b : maternal mortality curves
########################################################################
source("maternal mortality.r")

mort.plots[[2]] <- ggplot(matmort.blanc, aes(x=age, y=value, group=level)) +
  geom_line(aes(colour=level)) +
  geom_line(data=matmort.grimes, aes(x=age, y=value, group=level, colour=level), linetype=2) +
  
  scale_x_continuous("Age (years)", limits=c(15, max.age)) +
  coord_cartesian(xlim = c(15, max.age), ylim = c(0, 0.1)) +
  scale_y_continuous("Mortality hazard", limits=c(0, 0.1)) +
  scale_colour_manual(values=cbPalette) +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=graph_text_size)
    ,axis.title=element_text(size=graph_text_size)
  ) +
  
  # title
  ggtitle("B") +
  theme(plot.title = element_text(hjust = -0.05, face="bold")) +
  
  # hide legend
  #theme(legend.position="none") +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# write to file
pdf( paste(graph.path, "fig1.pdf", sep="/"), height=4, width=3.3)
multiplot(plotlist=mort.plots, cols=1, layout=matrix(c(1,2), nrow=2, byrow=TRUE))
dev.off()


########################################################################
## Figure 2 : sibling effects
########################################################################
source("sib effects.r")

sib.plots <- list()

sib.plots[[1]] <- ggplot(sibcomps.melt, aes(age, value, group=SiblingCompetition)) +
  #geom_point(aes(shape=SiblingCompetition, colour=SiblingCompetition)) +
  geom_line(aes(colour=SiblingCompetition)) +
  
  scale_x_continuous("Child's age (years)") +
  scale_y_continuous("Weight on siblings' mortality") +
  scale_colour_manual(values=cbPalette) +
  
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=graph_text_size)
    ,axis.title=element_text(size=graph_text_size)
  ) +
  
  ggtitle("A") +
  theme(plot.title = element_text(hjust = -0.05, face="bold")) +
  
  # hide legend
  theme(legend.position="none") +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))


sib.plots[[2]] <- ggplot(sibhelps.melt, aes(age, value, group=SiblingHelp)) +
  #geom_point(aes(shape=SiblingHelp, colour=SiblingHelp)) +
  geom_line(aes(colour=SiblingHelp)) +
  
  scale_x_continuous("Child's age (years)") +
  scale_y_continuous("Weight on siblings' mortality") +
  scale_colour_manual(values=cbPalette) +

  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=graph_text_size)
    ,axis.title=element_text(size=graph_text_size)
  ) +
  
  ggtitle("B") +
  theme(plot.title = element_text(hjust = -0.05, face="bold")) +
  
  # hide legend
  theme(legend.position="none") +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# save graphs
pdf( paste(graph.path, "fig2.pdf", sep="/"), height=4.2, width=3.3)
multiplot(plotlist=sib.plots, cols=1, layout=matrix(c(1,2), nrow=2, byrow=TRUE))
dev.off()


########################################################################
## Figure 3 : plot IBIs for all populations
########################################################################
# get data for all populations but no maternal mortality
IB.subset <- subset(IB.melt, MaternalMortality=="N", select=c(age, value, Population))

# summarise data according to age and population
IB.avg <- ddply(IB.subset,
                .(age, Population),
                summarise,
                mean = mean(value),
                sd   = sd(value),
                se   = sd(value) / sqrt(length(value)),
                ci   = 1.96 * (sd(value) / sqrt(length(value))),
                N    = length(value))

pdf( paste(graph.path, "fig3.pdf", sep="/"), height=2, width=3.3)

# plot figure
ggplot(IB.avg, aes(age, mean, group=factor(Population))) +
  geom_line(aes(colour=Population)) +
  
  scale_x_continuous("Age (years)", limits=c(0, max.age), breaks=seq(0, max.age, by=10)) +
  coord_cartesian(xlim = c(0, max.age), ylim = c(0, max.ibi)) +
  scale_y_continuous("Interbirth interval (years)", limits=c(0, max.ibi)) +
  scale_colour_manual(values=cbPalette) +
  
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=graph_text_size)
    ,axis.title=element_text(size=graph_text_size)
  ) +
  
  # hide legend
  theme(legend.position="none") +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

dev.off()

# range of IBIs referenced in first paragraph of Results
range(subset(IB.avg, age > 20 & age < 40, select=c(mean)))
range(subset(IB.avg, age > 20, select=c(mean)))

subset(IB.avg, mean>3)  # when (in experiments with probabilistic AFB), do IBIs peak?


########################################################################
## Figure 4 : plot sibling competition for each population
########################################################################
# get list of populations in the dataset
pops <- levels(IB.melt$Population)

# subset IBI data
IB.subset <- subset(IB.melt, MaternalMortality=="N" & SiblingHelp=="N", select=c(age, value, Population, SiblingComp))

# clean up data
IB.subset$SiblingComp <- relevel(IB.subset$SiblingComp, "N")
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="N"] <- "None"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="L"] <- "Low"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="M"] <- "Medium"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="H"] <- "High"

plots <- list()
i <- 1  # loop counter

# loop populations and, for each, draw a graph of the four sib competition intensities
for (pop in pops)
{
  plots[[i]] <- ggplot(subset(IB.subset, Population==pop),
                       aes(age, value, group=SiblingComp, colour=SiblingComp)) +
    geom_line() +
    # labels
    scale_x_continuous("Age (years)", limits=c(0, max.age)) +
    coord_cartesian(xlim = c(0, max.age), ylim = c(0, max.ibi)) +
    scale_y_continuous("Interbirth interval (years)", limits=c(0, max.ibi)) +
    scale_colour_manual(values=cbPalette) +
    
    labs(title=pop) +
    # look and feel
    theme_bw() +
    #eliminates baground, gridlines, and chart border
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_blank()
      ,axis.text=element_text(size=graph_text_size)
      ,axis.title=element_text(size=graph_text_size)
    ) +
    
    # hide legend
    theme(legend.position="none") +
    
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black'))
  
  i <- i + 1
}

pdf( paste(graph.path, "fig4.pdf", sep="/"), height=7, width=7)
multiplot(plotlist=plots, cols=2, layout=matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE))
dev.off()


########################################################################
## calc difference in average IBI by level of sib comp
########################################################################
# subset IBI data for average human population and no sibling help
IB.subset <- subset(IB.melt, MaternalMortality=="N" & SiblingHelp=="N", select=c(age, value, Population, MaternalMortality, SiblingComp, SiblingHelp))

IB.subset$MaternalMortality <- relevel(IB.subset$MaternalMortality, "N")
IB.subset$SiblingComp       <- relevel(IB.subset$SiblingComp, "N")
IB.subset$SiblingHelp       <- relevel(IB.subset$SiblingHelp, "N")

# summarise data according to age and level of maternal mortality
require(plyr)
IB.avg <- ddply(IB.subset,
                .(Population, SiblingComp),
                summarise,
                mean = mean(value),
                median = median(value),
                sd   = sd(value),
                se   = sd(value) / sqrt(length(value)),
                ci   = 1.96 * (sd(value) / sqrt(length(value))),
                N    = length(value))

levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="N"] <- "None"
levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="L"] <- "Low"
levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="M"] <- "Medium"
levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="H"] <- "High"

# show ranges of medians by population
ddply(IB.avg, .(Population), summarise, range=range(median))

# difference in medians - for table S5
IB.avg[IB.avg$SiblingComp=="High",]$median - IB.avg[IB.avg$SiblingComp=="None",]$median


########################################################################
## calc difference in average IBI by level of sib help
########################################################################
# subset IBI data for average human population and no sibling help
IB.subset <- subset(IB.melt, 
                    MaternalMortality=="N", # & SiblingComp=="M", 
                    select=c(age, value, Population, MaternalMortality, SiblingComp, SiblingHelp))

IB.subset$MaternalMortality <- relevel(IB.subset$MaternalMortality, "N")
IB.subset$SiblingComp       <- relevel(IB.subset$SiblingComp, "N")
IB.subset$SiblingHelp       <- relevel(IB.subset$SiblingHelp, "N")

# summarise data according to age and level of maternal mortality
require(plyr)
IB.avg <- ddply(IB.subset,
                .(Population, SiblingComp, SiblingHelp),
                summarise,
                mean = mean(value),
                median = median(value),
                sd   = sd(value),
                se   = sd(value) / sqrt(length(value)),
                ci   = 1.96 * (sd(value) / sqrt(length(value))),
                N    = length(value))

levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="N"] <- "None"
levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="L"] <- "Low"
levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="M"] <- "Medium"
levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="H"] <- "High"

# show ranges of medians by population
sib.help.ranges <- ddply(IB.avg, .(Population, SiblingComp), summarise, range=max(median)-min(median))

# difference in medians - for table S5
IB.avg[IB.avg$SiblingHelp=="High",]$median - IB.avg[IB.avg$SiblingHelp=="None",]$median


########################################################################
## Find ages at first birth by population and level of sib comp
########################################################################
# subset IBI data
IB.subset <- subset(IB.melt, value > 0 & MaternalMortality=="N" & SiblingHelp=="N", select=c(age, value, Population, SiblingComp))

IB.subset$SiblingComp       <- relevel(IB.subset$SiblingComp, "N")
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="N"] <- "None"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="L"] <- "Low"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="M"] <- "Medium"
levels(IB.subset$SiblingComp)[levels(IB.subset$SiblingComp)=="H"] <- "High"

# summarise data according to age and level of maternal mortality
require(plyr)
AFBs <- ddply(IB.subset,
              .(Population, SiblingComp),
              summarise,
              AFB = min(age))


########################################################################
## Figure 5 : sibling help for one particular population
########################################################################
plot.fig5 <- function(population, plot.title="") {
  require(grid)
  
  # get data for specified population; no maternal mortality
  IB.subset <- subset(IB.melt, Population==population & MaternalMortality=="N", select=c(age, value, SiblingComp, SiblingHelp))
  
  # summarise data according to age and population
  IB.avg <- ddply(IB.subset,
                  .(age, SiblingComp, SiblingHelp),
                  summarise,
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)),
                  ci   = 1.96 * (sd(value) / sqrt(length(value))),
                  N    = length(value))
  
  # clean up data
  IB.avg$SiblingComp <- relevel(IB.avg$SiblingComp, "N")
  levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="N"] <- "None"
  levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="L"] <- "Low"
  levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="M"] <- "Medium"
  levels(IB.avg$SiblingComp)[levels(IB.avg$SiblingComp)=="H"] <- "High"
  # help
  IB.avg$SiblingHelp <- relevel(IB.avg$SiblingHelp, "N")
  levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="N"] <- "None"
  levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="L"] <- "Low"
  levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="M"] <- "Medium"
  levels(IB.avg$SiblingHelp)[levels(IB.avg$SiblingHelp)=="H"] <- "High"
  
  fig5 <- ggplot(IB.avg, aes(age, mean, group=factor(SiblingComp))) +
      geom_line(aes(colour=SiblingComp)) +
      
      scale_x_continuous("Age (years)", limits=c(0, max.age), breaks=seq(0, max.age, by=10)) +
      coord_cartesian(xlim = c(0, max.age), ylim = c(0, max.ibi)) +
      scale_y_continuous("Interbirth interval (years)", limits=c(0, max.ibi)) +
      scale_colour_manual(values=cbPalette) +
      
      facet_grid(. ~ SiblingHelp, labeller=label_both) +
      
      labs(title=plot.title) +
      theme(plot.title = element_text(hjust = -0.05, face="bold")) +
      
      theme_bw() +
      theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank()
        ,axis.text=element_text(size=graph_text_size)
        ,axis.title=element_text(size=graph_text_size)
        ,panel.margin = unit(1, "lines")
      ) +
      
      # hide legend
      theme(legend.position="none") +
      
      #draws x and y axis line
      theme(axis.line = element_line(color = 'black'))
  
  return(fig5)
}

fig5 <- plot.fig5("Sweden")
pdf( paste(graph.path, "fig5.pdf", sep="/"), height=2, width=7)
plot(fig5)
dev.off()
