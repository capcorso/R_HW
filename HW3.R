#' title: HW3
#' author: Julianna 
#' due date: 12, Feb 2023
#' output: html_documemnt

# input data 
trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')

# install needed packages 
# install anova and car 
install.packages('car') 
library(car)         


# Abies Plots 
plot(cover ~ elev, data = abies, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'Abies elevation vs. cover')

plot(cover ~ tci, data = abies, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'Abies tci vs. cover')

plot(cover ~ streamdist, data = abies, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'Abies stream distance vs. cover')

plot(cover ~ beers, data = abies, xlab = 'Beers',
     ylab = 'Cover', main= 'Abies beers vs. cover')

# Acer Plots 
plot(cover ~ elev, data = acer, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'Acer elevation vs. cover')

plot(cover ~ tci, data = acer, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'Acer tci vs. cover')

plot(cover ~ streamdist, data = acer, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'Acer stream distance vs. cover')

plot(cover ~ beers, data = acer, xlab = 'Beers',
     ylab = 'Cover', main= 'Acer beers vs. cover')
acer_plot <- lm(cover ~ ., data = acer)
summary(acer_plot)

# Restructure and subset 

    # we wish to model species cover across all sampled plots
    # create site x sp matrix for two species 
    sp_cov = with(trees, tapply(cover, list(plotID, spcode), 
                                function(x) round(mean(x))))
    sp_cov = ifelse(is.na(sp_cov), 0, sp_cov)
    sp_cov = data.frame(plotID = row.names(sp_cov), sp_cov)
    # create environmental matrix
    cols_to_select = c('elev', 'tci', 'streamdist', 'disturb', 'beers')
    env = aggregate(trees[ , cols_to_select], by = list(trees$plotID), 
                    function(x) x[1])
    names(env)[1] = 'plotID'
    # merge species and enviornmental matrices
    site_dat = merge(sp_cov, env, by='plotID')
    # subset species of interest
    abies = site_dat[ , c('ABIEFRA', cols_to_select)]
    acer  = site_dat[ , c('ACERRUB', cols_to_select)]
    names(abies)[1] = 'cover'
    names(acer)[1] = 'cover'


# 1. Carry out an exploratory analysis using the tree dataset. Metadata for the tree study can be found here. Specifically, I would like you to develop and compare models for species cover for a habitat generalist Acer rubrum (Red maple) and a habitat specialist Abies fraseri (Frasier fir). Because this dataset includes both continuous and discrete explanatory variables use the function Anova in the packages car as such

# Cover and elevation for Abies and Acer  
    # this doesnt show much in response to your question but I worked really hard to make it so i wanted to share : )


plot(cover ~ elev, data = trees, type = 'n', 
     xlab = 'elevation', ylab = 'cover')
points(cover ~ elev, data = trees, subset = spcode == "ABIEFRA",
       pch = 1, col = 'red')
lines(lowess(trees$elev[trees$spcode =='ABIEFRA'],
             trees$cover[trees$spcode == 'ABIEFRA']),
      lty = 1, col = 'red')
points(cover ~ elev, data = trees, subset = spcode == "ACERRUB",
       pch = 2, col = 'blue')
lines(lowess(trees$elev[trees$spcode =='ACERRUB'],
             trees$cover[trees$spcode =='ACERRUB']),
      lty = 2, col = 'blue')
legend('topleft', c('Abies', 'Acer'), col = c('red', 'blue'), 
       pch = c(1, 2), bty = 'n')

# Intercept only plot 

null_abies = lm(cover ~ 1, data = abies)
null_acer = lm(cover ~ 1, data = acer)     
null_abies
null_acer

mean(abies$cover)
mean(acer$cover)

plot(cover ~ 1, data = abies)
abline(null_abies, lwd = 2)
abline(h = mean(abies$cover), col = 'red', lty = 2, lwd = 2)

plot(cover ~ 1, data = acer)
abline(null_acer, lwd = 2)
abline(h = mean(acer$cover), col = 'red', lty = 2, lwd = 2)

# ggplot boxplot 
ggplot(data = abies) + 
  geom_boxplot(mapping = aes(x = disturb, y = cover)) +  
  labs(x = 'Disturbance', y = 'Cover', title = 'abies disturbance versus cover') 

ggplot(data = acer) + 
  geom_boxplot(mapping = aes(x = disturb, y = cover)) +  
  labs(x = 'Disturbance', y = 'Cover', title = 'acer disturbance versus cover') 

quantile(abies$cover[abies$disturb == 'VIRGIN'])
quantile(abies$cover[abies$disturb == 'CORPLOG'])
quantile(abies$cover[abies$disturb == 'SETTLE'])
quantile(abies$cover[abies$disturb == 'LT-SEL'])

quantile(acer$cover[acer$disturb == 'VIRGIN'])
quantile(acer$cover[acer$disturb == 'CORPLOG'])
quantile(acer$cover[acer$disturb == 'SETTLE'])
quantile(acer$cover[acer$disturb == 'LT-SEL'])

# Abies Plots 
plot(cover ~ elev, data = abies, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'Abies elevation vs. cover')

plot(cover ~ tci, data = abies, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'Abies tci vs. cover')

plot(cover ~ streamdist, data = abies, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'Abies stream distance vs. cover')

plot(cover ~ beers, data = abies, xlab = 'Beers',
     ylab = 'Cover', main= 'Abies beers vs. cover')

# Acer Plots 
plot(cover ~ elev, data = acer, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'Acer elevation vs. cover')

plot(cover ~ tci, data = acer, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'Acer tci vs. cover')

plot(cover ~ streamdist, data = acer, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'Acer stream distance vs. cover')

plot(cover ~ beers, data = acer, xlab = 'Beers',
     ylab = 'Cover', main= 'Acer beers vs. cover')
acer_plot <- lm(cover ~ ., data = acer)
summary(acer_plot)


# in order to see significance of individual variables for acer and abies, looking at statistics 

  # Abies 
    elev_indv_abies = lm(cover ~ elev, data = abies)
    tci_indv_abies = lm(cover ~ tci, data = abies)
    streamdist_indv_abies = lm(cover ~ streamdist, data = abies)
    disturb_indv_abies = lm(cover ~ disturb, data = abies)
    beers_indv_abies = lm(cover ~ beers, data = abies)
   # Summary of Abies individual statistics
    summary(elev_indv_abies)
    Anova(elev_indv_abies, type= 3)
    summary(tci_indv_abies)
    summary(streamdist_indv_abies)
    summary(disturb_indv_abies)
    summary(beers_indv_abies)

  # significant P vales 
      #for abies at elevation, streamdist, and disturb (specifically virgin)
    
    
  #Acer 
    elev_indv_acer = lm(cover ~ elev, data = acer)
    tci_indv_acer = lm(cover ~ tci, data = acer)
    streamdist_indv_acer = lm(cover ~ streamdist, data = acer)
    disturb_indv_acer = lm(cover ~ disturb, data = acer)
    beers_indv_acer = lm(cover ~ beers, data = acer)
  # Summary of Acer individual statistics
    summary(elev_indv_acer)
    summary(tci_indv_acer)
    summary(streamdist_indv_acer)
    summary(disturb_indv_acer)
    summary(beers_indv_acer)
   
  # significant P values 
      #for acer at elevation, stremdist, disturb ( virgin), and beers 
    
  #To look at Acer and Abies statistics condensed with all variables 
    all_vari_abies = lm(cover ~ elev + tci + streamdist + disturb + beers, data = abies)
    summary(all_vari_abies)
    
    all_vari_acer = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer)
    summary(all_vari_acer)
    
# From these statistics it can be said that beers effects Acer trees but not Abies. In addition, Tci has no significant effect on either type of trees.

    plot(cover ~ tci, data = acer, xlab = 'Topographic Coverage Index',
         ylab = 'Cover', main= 'Acer tci vs. cover')

    # Anova 
    acer_mod <- lm(cover ~ ., data = acer)
    summary(acer_mod)
    Anova(update(acer_mod, . ~ . -1), type = 3)
    
#working around outliers... Still figuring this out 
    # look at outliers in Tci vs. Cover plot for acer trees, then removing them from the plot.  
    identify(acer$cover ~ acer$tci, n = 2)
    acer_post = acer[-c(121, 318), ]
    plot(cover ~ tci, data= acer_post, xlab='tci', ylab= 'cover')
    
  # looking at before and after outlier data 
    all_vari_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer_post)
    summary(all_vari_acersubset)
    summary(all_vari_acer)
    # the difference in the before and after removal of the outliers was the p value of tci became much lower making tci more significant. It also improved the r^2 value. 
    
# the interactions of variables 
    full_mod_acersubset = update(all_mod_acersubset, ~ . + elev * disturb * tci * streamdist * beers)
    all_mod_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer_subset)
    anova(all_vari_acersubset, full_mod_acersubset)
    
    AIC(full_mod_acersubset)
    AIC(all_mod_acersubset)
    AIC(elev_interaction_mod_acersubset)
    
# Now Abies 
    identify(abies$cover ~ abies$tci, n=2)
    abies_post = abies[-c(121,33,109,56,53), ]
    dim(abies_post)
    all_vari_abiessubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = abies_subset)
    summary(all_vari_abiessubset)
    summary(all_vari_abies)
    
#taking out variables that are not significant to the data, 
#Using OLS 
    abies_run = lm(cover ~ elev + tci + streamdist + disturb + elev * tci + elev * streamdist + elev * disturb, data = abies_subset)
    acer_run = lm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers +  elev * disturb, data = acer_subset)
    summary(abies_run)
    summary(acer_run)
#GLM 
    abies_GLM = glm(cover ~ elev + tci + streamdist + disturb + elev * tci + elev * streamdist + elev * disturb, data = abies_subset, family = "poisson")
    acer_GLM = glm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers +  elev * disturb, data = acer_subset, family = "poisson")
    summary(abies_GLM)
    summary(acer_GLM)
#AIC 
    AIC(abies_run)
    AIC(abies_GLM)
    AIC(acer_run)
    AIC(acer_GLM)
    
#GLM of poission is seen in the data to be a better fit. This can be due to the fact the data is highly skewed data for elevation. When looking at the cover vs elevation data it is seen that a significant amount of the data is around the 0 value, until the elevation significantly increases. Creating a positive effect of elevation and cover for abies! 
    
# in non statistical terms it can be concluded that abies need a higher elevation and or altitude in order to grow. 
    
    



