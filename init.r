library(ggplot2)
library(plyr)
library(reshape)
library(manipulate)


########################################################################
## parameters
########################################################################
data.dir <- "./data"

# pick which experiment to plot
exp <- "max age 50"
#exp <- "probabilistic AFB"
#exp <- "exponential maternal mortality"
#exp <- "sib help effects doubled"
#exp <- "sib help effects same as comp"


mortality <- "all mortality"
#mortality <- "child mortality only"
#mortality <- "adult mortality only"

data.path <- file.path(data.dir, exp, mortality)
graph.path <- file.path(data.path, "graphs")

graph_text_size <- 10
max.age <- 50
max.ibi <- 4

writeFiles <- F

# save current working directory
wd.current <- getwd()

# read in details of family structures
Family.sizes <- read.csv("./data/family sizes.csv", header=T, sep=",")

setwd(data.dir)

# make graphs folder and hide warnings (in case it already exists)
dir.create(graph.path, showWarnings=F)

# change back to original working directory
setwd(wd.current)

# colourblind-friendly palette from http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/#a-colorblind-friendly-palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


########################################################################
## functions
########################################################################
# for labelling facets in IBI and LB plots
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="SiblingHelp") { 
    value[value=="N"] <- "Help: N"
    value[value=="L"] <- "Help: L"
    value[value=="M"] <- "Help: M"
    value[value=="H"] <- "Help: H"
  } else if (var=="SiblingComp") {
    value[value=="N"] <- "Comp: N"
    value[value=="L"] <- "Comp: L"
    value[value=="M"] <- "Comp: M"
    value[value=="H"] <- "Comp: H"
  } else if (var=="MaternalMortality") {
    value[value=="N"] <- "None"
    value[value=="L"] <- "Low"
    value[value=="M"] <- "Medium"
    value[value=="H"] <- "High"
  }
  return(value)
}

##########
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
