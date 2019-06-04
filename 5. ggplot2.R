#1 Use data in the birthwt dataset in the MASS library
library(MASS)
View(birthwt)
#1a Construct three density plots of birthweight (bwt) grouping by race
#in the same plotting pane
library(ggplot2)
f<- ggplot(birthwt, aes(x=bwt, fill=race))
f+ geom_density(color="red",alpha=.4)+theme_bw()+
  scale_fill_brewer(palette = "OrRd")

#*****Correct
# change race to a factor so that we can separate the density plots based on this factor
birthwt$race <- factor(birthwt$race)
# these are the four plotting steps: p1, p2, p3, p4
# ** Breaking plots down into these steps isn't necessary. You could just do a bunch
#   of ggplot(...) + ggtitle('...') + geom_point() etc. But this might be clearer.**
p1 <- ggplot(data = birthwt, aes(x = bwt))
# (use ?birthwt to see which columns correspond to birth weight and mother's race)
p2 <- p1 + ggtitle('Smoothed Birth Weight Density, by Mother\'s Race')
p3 <- p2 + geom_density(aes(group = race, fill = race), color = 'black', alpha = 0.4)
# (can experiment with the above color and alpha parameters to see what's clearest for you)
p4 <- p3 + theme_classic()
# need to "run" the final step of the graph to actually plot it, if using the step-by-step method:
p4
#*****

#1b Construct a multipane scatterplot of mother's weight (lwt) versus
#birthweigth with a separate pane/facet for smoking status.
p<- ggplot(data =birthwt, aes(x=lwt, y=bwt), color=smoke)
p+facet_grid(.~smoke)+geom_point()

#2 See a dataset 'Sika'
View(Sitka)

#2a Produce a plot of size (y) versus Time (x) for tree 1.
tree1<- subset(Sitka, tree==1)
p1<- ggplot(tree1, aes(x=Time, y= size))
p1+geom_line()

#2b Produce a plot of size (y) versus Time (x) for all 79 trees with a
#separate line for each tree. Use the color or each line to denote the
#condition (control or ozone-rich).
g2<-ggplot(data=Sitka, aes(x=Time, y=size,color=treat))
g2+geom_line (aes(group=tree))

#2c Same as previous graph in part (b) on facet based on control or
#ozone-rich. Based on this graph, does there appear to be a dierence
#in growth between the two conditions?
g2+geom_line(aes(group=tree))+facet_grid(.~treat)
#yes it does show the different in growth. 
#The control tree has more variance in growth than the ozone tree