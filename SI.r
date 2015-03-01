library(ggplot2)
library(plyr)
library(reshape)

########################################################################
## initialise stuff and import data
########################################################################
source("init.r")
source("import-ibi-data.r")


########################################################################
## Figure S1
########################################################################
# read optimal births
births.mnn <- read.csv( paste(data.path, "/Births-Gambia-MLN.out", sep="") , header=F)
# rename columns
names(births.mnn) <- c("Age", "Index", "Decision")

# read in details of family structures
Family.sizes         <- read.csv("./data/family sizes.csv", header=T, sep=",")
Family.sizes.ordered <- read.csv("./data/Family sizes - ordered.csv", header=T, sep=",")

# merge with family structure details
births.mnn <- merge(births.mnn, Family.sizes, by= intersect(names(births.mnn), names(Family.sizes)) )

# plot fig S1
ggplot(subset(births.mnn, Decision=="True"), aes(Age, Index), size=1, colour="black") +
  geom_point(size=1, colour="black") +
  geom_blank(data=subset(births.mnn, Decision=="False"), size=0, colour="white") +
  facet_grid(Size ~ ., scales = "fixed", space = "free") +  #, space = "free"
  scale_x_continuous(breaks = seq(15, max.age, by = 10)) +
  ylab("Family composition") +
  xlab("Age") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y=element_blank())


########################################################################
## Figure S2 : plot IBIs, split by maternal mortality
########################################################################
# get list of populations in the dataset
pops <- levels(IB.melt$Population)

IB.subset <- subset(IB.melt, SiblingComp=="N" & SiblingHelp=="N", select=c(age, value, Population, MaternalMortality))

# summarise data according to age and level of maternal mortality
IB.avg <- ddply(IB.subset,
                .(age, Population, MaternalMortality),
                summarise,
                mean = mean(value),
                sd   = sd(value),
                se   = sd(value) / sqrt(length(value)),
                ci   = 1.96 * (sd(value) / sqrt(length(value))),
                N    = length(value))


levels(IB.avg$MaternalMortality)[levels(IB.avg$MaternalMortality)=="N"] <- "None"
levels(IB.avg$MaternalMortality)[levels(IB.avg$MaternalMortality)=="L"] <- "Low"
levels(IB.avg$MaternalMortality)[levels(IB.avg$MaternalMortality)=="M"] <- "Medium"
levels(IB.avg$MaternalMortality)[levels(IB.avg$MaternalMortality)=="H"] <- "High"

none <- element_blank()

plots <- list()
i <- 1  # loop counter

# loop populations and, for each, draw a graph of the four sib competition intensities
for (pop in pops)
{
  plots[[i]] <- ggplot(subset(IB.avg, Population==pop),
                       aes(age, mean)) +
    geom_line(size=1.2) +
    facet_wrap(~ MaternalMortality, ncol=2) +
    scale_x_continuous("Age (years)", limits=c(0, max.age)) +
    coord_cartesian(xlim = c(0, max.age), ylim = c(0, max.ibi)) +
    scale_y_continuous("Interbirth interval (years)", limits=c(0, max.ibi))  +
    theme(legend.position = "none", panel.grid.major = none, panel.grid.minor = none) +
    labs(title=pop) +
    theme_bw()
  
  i <- i + 1
}

multiplot(plotlist=plots, cols=2, layout=matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE))


########################################################################
## age at which reproductive cessation occurs
########################################################################
min(subset(IB.avg, Population=="Gambia" & MaternalMortality=="Medium" & age>20 & mean==0.0)$age)
min(subset(IB.avg, Population=="Gambia" & MaternalMortality=="High" & age>20 & mean==0.0)$age)


########################################################################
## Figure S3 : sibling help for all populations
########################################################################
# get data for all populations but no maternal mortality
IB.subset <- subset(IB.melt, MaternalMortality=="N", select=c(age, value, Population, SiblingComp, SiblingHelp))

# summarise data according to age and population
IB.avg <- ddply(IB.subset,
                .(age, Population, SiblingComp, SiblingHelp),
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

pops <- levels(IB.melt$Population) # get list of populations in the dataset
plots <- list()
i <- 1  # loop counter

# loop populations and, for each, draw a graph of the four sib competition intensities
for (pop in pops)
{
  plots[[i]] <- ggplot(subset(IB.avg, Population==pop),
                       aes(age, mean, group=factor(SiblingComp))) +
    geom_line(aes(colour=SiblingComp)) +
    
    scale_x_continuous("Age (years)", limits=c(0, max.age), breaks=seq(0, max.age, by=10)) +
    coord_cartesian(xlim = c(0, max.age), ylim = c(0, max.ibi)) +
    scale_y_continuous("Interbirth interval (years)", limits=c(0, max.ibi)) +
    
    facet_grid(. ~ SiblingHelp, labeller=label_both) +
    
    labs(title=pop) +
    
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
  
  i <- i + 1
}

pdf( paste(graph.path, "figS3.pdf", sep="/"), height=14, width=7)
multiplot(plotlist=plots, cols=1, layout=matrix(c(1,2,3,4,5), nrow=5, byrow=TRUE))
dev.off()
