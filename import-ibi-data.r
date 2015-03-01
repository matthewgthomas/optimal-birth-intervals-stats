library(reshape)

# save current working directory
#wd.current <- getwd()

#setwd(data.dir)

########################################################################
## initialise IB data frames
########################################################################
# set up data frames
ib.files <- list.files(data.path, pattern="IB.*csv", full.names=F)

IB.all <- data.frame(age=seq(15:90))


########################################################################
## IBI files
########################################################################
for (file in ib.files)
{
  filename <- paste(data.path, "/", file, sep="")
  
  # remove dashes from filename
  file <- gsub("\\-", ".", file)
  
  # get only param values for output filenames
  file.short <- substring(file, 4, nchar(file) - 4)  # "IB-" is 3 characters, ".out" is final 4
  
  #assign(Births, read.csv(filename, header=FALSE, sep=","))
  IB <- read.csv(filename, header=T, sep=",")
  
  # add to data frame of all IBIs
  IB.all <- merge(IB.all, IB, by=intersect(names(IB.all), names(IB)), all=T)
  # rename column to be current file's params
  names(IB.all)[names(IB.all)=="IBI"] <- file.short
  
  if (writeFiles)
  {
    # create jpg file
    jpeg( paste(graph.path, "/", "IB-", file.short, ".jpg", sep="") )
    
    # plot IBIs
    current.plot <- ggplot(IB, aes(age, IBI)) +
      geom_line() +
      scale_x_continuous(breaks = seq(15, 90, by = 5)) +
      scale_y_continuous(breaks = seq(0, 14, by=1), limits=c(0,14)) +
      theme_bw()
    
    print(current.plot)
    
    # close file
    dev.off()
  }
}


########################################################################
## process IBI data
########################################################################
# reshape data
IB.melt <- melt(IB.all, id=c("age"), variable_name="Mortality")

# remove NAs
IB.melt <- na.omit(IB.melt)

# split mortality column into population and mortality params
IB.melt <- cbind(IB.melt, colsplit(IB.melt$Mortality, "\\.", c("Population", "Params")))

# split Mortality column (e.g. "NNNN") into four separate columns
IB.melt <- cbind(IB.melt, colsplit(IB.melt$Params, "",
                                   c("MaternalMortality", "SiblingComp", "SiblingHelp")))

IB.melt$MaternalMortality <- factor(IB.melt$MaternalMortality, levels(IB.melt$MaternalMortality)[c(4,2,3,1)])
IB.melt$SiblingComp <- factor(IB.melt$SiblingComp, levels(IB.melt$SiblingComp)[c(4,2,3,1)])
IB.melt$SiblingHelp <- factor(IB.melt$SiblingHelp, levels(IB.melt$SiblingHelp)[c(4,2,3,1)])


########################################################################
## clean up
########################################################################
# remove temporary objects from environment
rm(list=c("ib.files", "file", "file.short", "filename", "IB"))
if (writeFiles) { rm("current.plot") }

# change back to original working directory
#setwd(wd.current)
