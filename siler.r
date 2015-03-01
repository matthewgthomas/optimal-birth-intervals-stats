siler <- read.csv("./data/siler.csv", header=T)

siler.melt <- melt(siler, id=c("age"), variable_name="Population")

siler.mean <- ddply(siler.melt,
                    .(age),
                    summarise,
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)),
                    ci   = 1.96 * (sd(value) / sqrt(length(value))),
                    N    = length(value))

#require(ggplot2)
#ggplot(siler.melt, aes(age, value, group=factor(Population))) +
#  geom_line(aes(linetype=Population, colour=Population), size=1.2) +
#  #geom_ribbon( aes(ymax=mean + sd, ymin=mean - sd), alpha=.3, colour=NA) +
#  scale_x_continuous("Age (years)", limits=c(0,90)) +
#  coord_cartesian(xlim = c(0, 90), ylim = c(0, 1)) +
#  scale_y_continuous("Probability of Survival", limits=c(0,1)) +
#  theme_bw()
#  #theme(axis.text=element_text(size = rel(2)))
