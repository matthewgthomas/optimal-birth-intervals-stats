########################################################################
## Figure S4 : sibling help sensitivity comparison
########################################################################
# this stores the three plots
plots <- list()


###################################
## juvenile help as in main text ##
###################################
exp <- "max age 50"
mortality <- "all mortality"

data.path <- file.path(data.dir, exp, mortality)
graph.path <- file.path(data.path, "graphs")

source("init.r")
source("import-ibi-data.r")

plots[[1]] <- plot.fig5("Sweden", "(a)")


###########################
## juvenile help doubled ##
###########################
exp <- "sib help effects doubled"
mortality <- "all mortality"

data.path <- file.path(data.dir, exp, mortality)
graph.path <- file.path(data.path, "graphs")

source("init.r")
source("import-ibi-data.r")

plots[[2]] <- plot.fig5("Sweden", "(b)")


################################################
## juvenile help same as but opposite to comp ##
################################################
exp <- "sib help effects same as comp"
mortality <- "all mortality"

data.path <- file.path(data.dir, exp, mortality)
graph.path <- file.path(data.path, "graphs")

source("init.r")
source("import-ibi-data.r")

plots[[3]] <- plot.fig5("Sweden", "(c)")


#################
## do the plot ##
#################
multiplot(plotlist=plots, cols=1, layout=matrix(c(1,2,3), nrow=3, byrow=TRUE))
