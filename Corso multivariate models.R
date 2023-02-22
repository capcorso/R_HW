#' title: HW4
#' author: Julianna 
#' output: html_documemnt
#' 
#' 
#' Import data 
library(vegan)
data(dune)
data(dune.env)
?dune

#'  1. Conduct an indirect ordination on the dune plant community. Specifically, visually examine a NMDS plot using the bray-curtis distance metric. Below is some code to help you develop a potential plot that emphasizes the role of the environmental variable “Moisture”. Describe how you interpret the graphic. What is the goal of creating such a plot? Does this analysis suggest any interesting findings with respect to the dune vegetation?
#' 
#' 
dune_mds <- metaMDS(dune,trace=0)
dune_mds

dune_fit <- envfit(dune_mds, dune.env)
dune_fit

plot(dune_mds)

#' code to help with plot from assignment
plot(dune_mds, type='n')
text(dune_mds, 'sp', cex=.5)
# generate vector of colors 
color_vect = rev(terrain.colors(6))[-1]
points(dune_mds, 'sites', pch=19, 
       col=color_vect[dune.env$Moisture])
legend('topright', paste("Moisture =", 1:5, sep=''), 
       col=color_vect, pch=19) 

#' treating moisture as a numeric variable instead of a factor with numerous centroids 

plot(dune_mds, type='n')
text(dune_mds, 'sp', cex=.5)
# generate vector of colors 
color_vect = rev(terrain.colors(6))[-1]
points(dune_mds, 'sites', pch=19, 
       col=color_vect[as.numeric(dune.env$Moisture)])
legend('topright', "Moisture", col=color_vect, pch=19, bty = "n",
       title = "Moisture", inset = c(0,1), x.intersp = 0.5, y.intersp = 0.5)

#' 
#' The scatter plot this code created is colored according to the moisture variable for a set of ecological samples. The plot is based on MDS coordinate, which are a way of visualizing the similarity or dissimilarity of samples based on multiple variable. 
#' The plot has a set of labeled poits, which correspond to the 'sp' varriable in the 'dune_mds' object. The labels are scalled down and plased near the points using the text function 
#' The color of each point is dtermined by the value of the moisture variable for the sample. Dark purple indicates low moisture while light yellow indicates high moisture. 
#' 
#' Within this plot there is a pattern in the distribution of samples based on their MDS coordinates and moisture levels. There are clustering so samples, but the relationship between the MDS coordinates and moisture levels is not abundently clear from this plot alone. Further analysis such as cluster analysis or regression, would be needed to fully explore the relationship between these variables. 

#' 2. Carry out a direct ordination using CCA in order to test any potential hypotheses that you developed after examining the MDS plot. Specifically, carry out a test of the entire model (i.e., including all constrained axes) and also carry out tests at the scale of individual explanatory variables you included in your model if you included more than one variable. Plot your results.
#' 
#' Test all enviornmental variables 
dune_cca <- cca(dune~ A1 + Moisture + Management + Use + Manure, data=dune.env)
plot(dune_cca)
#' WOah 
#' Loading points without scores 
plot(dune_cca, display= 'cn')
#' where scores are points and loadings are vectors 
plot(dune_cca, display= 'bp')

#' looking at data 
dune_cca 
#' So with all the paramiters in the data we get a value of 71%, but this is probably not very accurate as there are a lot of random variables that are being included. 
#' To find a more accurate calculation we should use the infamous rsquared 

dune_cca_adj <- RsquareAdj(dune_cca, nperm=5)

#' for more accurate reading run more then 5 times 
#' 
dune_cca_adj <- RsquareAdj(dune_cca, nperm=999)
dune_cca_adj

#' This shows us that it went from 71% to a mear 22%, this is supporting the fact that there are a lot of unhelpful variances 
#' 
#' Can run an anova 
anova(dune_cca)

#' Which can then be compared to anovas of each of the variables tested 
#' 
#' Terms
anova(dune_cca, by='terms')

#'Margin 
anova(dune_cca, by='margin')
#'Axis 
anova(dune_cca, by='axis')
#'Onedf 
anova(dune_cca, by='onedf')

#' with this information we can start to clean up the graph by taking out some of the variables. We see that Moisture and management and A1 have some significant values so lets keep that. 
dune_cca_2.0 <- cca(dune~ A1 + Moisture + Management, data=dune.env)
dune_cca_2.0 

#' lets take the R^2 of that new one 
#' 
dune_cca_2.0_adj <- RsquareAdj(dune_cca_2.0, nperm=999)
dune_cca_2.0_adj
anova(dune_cca_2.0)

#' A1 seems to have the smallest effect and then management less then moisture. So the data can be refined by removing A1 if desiered as well. 

anova(dune_cca_2.0, dune_cca)

plot(dune_cca_2.0)
#'  3. Do your two analyses agree with one another or complement one another or do these two analyses seem to be suggesting different take home messages? Which analysis do you find to be more useful?
#'  
#'  
#' I prefer the CDS plot, although the NMDS explains the dominate axes of variation and not just that of the enviornment as shown in CDS. However the stright forward correlation that the CDS provides is easier to understand and desipher in plain language then the NMDS. Both are interesting to explore for the data set reguardless. 









