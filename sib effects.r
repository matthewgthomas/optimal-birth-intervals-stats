# set up sib comp functions
sib.comp <- function(comp, age)
{
  weight <- 0.0
  
  if (comp=="N")
  {
    weight <- 0.0
  }
  else if (comp=="L")
  {
    weight <- .5 - (age * .03333)
  }
  else if (comp=="M")
  {
    weight <- 1 - (age * .06666)
  }
  else if (comp=="H")
  {
    weight <- 1 - (age * 0.0357)
  }
  
  weight
}

# set up sib help functions
sib.help <- function(help, age)
{
  weight <- 0.0
  
  if (help=="N")
  {
    weight <- 0.0
  }
  else if (help=="L")
  {
    weight <- -0.1
  }
  else if (help=="M")
  {
    weight <- -.5 * (age - 10.0) / 5.0
  }
  else if (help=="H")
  {
    weight <- -1.0 * (age - 10.0) / 5.0
  }
  
  weight
}

# set up data frames
sibcomps <- data.frame(age=seq(0,15,by=1))
sibcomps$None   <- sib.comp("N", sibcomps$age)
sibcomps$Low    <- sib.comp("L", sibcomps$age)
sibcomps$Medium <- sib.comp("M", sibcomps$age)
sibcomps$High   <- sib.comp("H", sibcomps$age)

sibhelps <- data.frame(age=seq(10,15,by=1))
sibhelps$None   <- sib.help("N", sibhelps$age)
sibhelps$Low    <- sib.help("L", sibhelps$age)
sibhelps$Medium <- sib.help("M", sibhelps$age)
sibhelps$High   <- sib.help("H", sibhelps$age)

require(reshape)
sibcomps.melt <- melt(sibcomps, id=c("age"), variable_name="SiblingCompetition")
sibhelps.melt <- melt(sibhelps, id=c("age"), variable_name="SiblingHelp")

rm(list=c("sibcomps", "sibhelps", "sib.help", "sib.comp"))
