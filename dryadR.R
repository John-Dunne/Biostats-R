
install.packages('ggplot2', dep = TRUE)

install.packages("tidyverse", dependencies = TRUE)

library(ggplot2)

#First we must read the data from the csv file into a dataframe.
dryad <- read.table("dryaddatasharing.csv", header=TRUE, sep=",")
#Now that we have a dataframe "dryad" we are free to start analyzing the data.
print(colnames(dryad))
#Sorrco isopods are an isopods species that was originally only found in a particular set of hot springs in New Mexico.
#Cannibalism is a natural behavior of these organisms and is considered a selective pressure. The data collected might shed some light on patterns associated with cannibalism in this species.

#What was the average partner body size z-score of each of the three populations and how did this track to attack incidents?

dryadplot <- ggplot(dryad, aes(x = Pop, y = Partner.BS.Standardized)) + geom_smooth()
dryadplotboxplot <- dryadplot+geom_boxplot()+ theme(legend.position="right")
print(dryadplotboxplot)

#What do the attacks look like for each one of these populations then?
#Bar graph showing count of recorded attacks between partners for each population
dryadplotbargraph <- ggplot(drya, aes(x = Pop, y = Attack))+ theme(legend.position="right")
# + theme adds a legend to the graph

#Try comparing Body vs Partner Body Size
#See if you can't color coat if there was an attack, or only compare sizes where there were attacks
#Could try using relative body size
dryadbodyVpartner <- ggplot(dryad, aes(x = Body.Size...Standardized, y = Partner.BS.Standardized))
dryadbodyVpartnercolor <- ggplot(dryad, aes(x = Body.Size...Standardized, y = Partner.BS.Standardized, col = Pop)) +
  geom_point(alpha = 0.4)


#Finally, see if you can compare length to likely hood of attacking "Long Boi Danger Scale"
