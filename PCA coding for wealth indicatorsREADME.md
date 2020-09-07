# P4GES-WHP-Assest-household-data
WHP subset household asset data
#load library
library("psych")
library("polycor")
library("dplyr")
library("vegan")
library("ggplot2")
library("grid")
library("gridExtra")
library("reshape2")
library("MuMIn")
library("glmmADMB")
library(permute)
library(lattice)
library(magrittr)
library(tidyr)
library(scales)
library(gmnl)
library(mlogit)
library(msm)

#load data
library(readxl)
AssestSubset3 <- read_excel("C:/Users/katie/OneDrive/Desktop/DXX-4999 MSc Dissertation/SPSS/AssestSubset3.xlsx")
View(AssestSubset3)

#### Principal component analysis (PCA) with wealth (poverty) indicators
# code adapated from Poudyal 2018 https://github.com/mpoudyal/cepaper/blob/master/cepaper.Rmd

#for this PCA, 6 measures of local wealth were used 
#Number of rooms", House quality", "Livestock", "Food security","Access to light", 

#poverty indicator subset
poverty <- AssestSubset3[,c(5, 6, 17, 22, 29, 42)]

## names for variables
poverty.pca.set <- c("main_house_rm", 
                      "main_house_rf", 
                      "sum_of_livestock", 
                      "food_sec", 
                      "light_sufficiency", 
                      "mplayer_num")
poverty.pca.set.nice <- c("Number of rooms", 
                          "House quality", 
                          "Livestock units", 
                          "Food security", 
                          "Access to light", 
                          "Music Player")

poverty.cor <- with(poverty, hetcor(main_house_rm, 
                                    as.ordered(main_house_rf),
                                    sum_of_livestock, 
                                    food_sec, 
                                    as.ordered(light_sufficiency), 
                                    as.ordered(mplayer_num),
                                    ML=TRUE))

poverty.pca <- principal(r=poverty.cor$correlations, rotate="none", nfactors=2)
summary(poverty.pca)
poverty.pca

#Factor analysis with Call: principal(r = poverty.cor$correlations, nfactors = 2, rotate = "none")

#Test of the hypothesis that 2 factors are sufficient.
#The degrees of freedom for the model is 4  and the objective function was  0.56 

#The root mean square of the residuals (RMSA) is  0.15

#Results below
#Standardized loadings (pattern matrix) based upon correlation matrix
                               #PC1   PC2   h2   u2 com
#main_house_rm                 0.59 -0.46 0.55 0.45 1.9
#as.ordered.main_house_rf.     0.57 -0.53 0.60 0.40 2.0
#sum_of_livestock              0.51  0.01 0.26 0.74 1.0
#food_sec                      0.75  0.04 0.56 0.44 1.0
#as.ordered.light_sufficiency. 0.67  0.38 0.59 0.41 1.6
#as.ordered.mplayer_num.       0.39  0.72 0.66 0.34 1.5

                       #PC1  PC2
#SS loadings           2.08 1.15
#Proportion Var        0.35 0.19
#Cumulative Var        0.35 0.54
#Proportion Explained  0.64 0.36
#Cumulative Proportion 0.64 1.00

## Basic PCA plot 
plot(poverty.pca, labels=poverty.pca.set.nice)


## PCA prediction data and adding PC scores to 'poverty' data frame (basic plot)
poverty.pred.data <- poverty[, poverty.pca.set]
colnames(poverty.pred.data) <- rownames(poverty.pca$loadings)
poverty.pca$scores <- factor.scores(poverty.pred.data, poverty.pca)
rownames(poverty.pca$loadings) <- poverty.pca.set.nice
poverty <- cbind(poverty,  poverty.pca$scores$scores)


## New site names to match with manuscript text and with other plots
AssestSubset3 %>% pull(site)
head(site)

poverty$site2 <- factor(poverty$site, 
                        levels=c("Amporforo", "Zahamena", "Sahavazina", "Ampahitra"))
                
                     
poverty$v.symbol <- with(poverty, 
                         ifelse(site=="Ampahitra", 16,
                                ifelse(site=="Sahavazina", 15,
                                       ifelse(site=="Zahamena", 18, 17))))

poverty$v.col <- with(poverty, 
                      ifelse(site=="Ampahitra", "#C77CFF",
                             ifelse(site=="Sahavazina", "#00BFC4",
                                    ifelse(site=="Zahamena", "#7CAE00", "#F8766D"))))

require(dplyr)
poverty.nomis <- na.omit(poverty)
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]
hulls <- poverty.nomis %>% do(find_hull(.))
poverty.pca.loads <- data.frame(PC1=loadings(poverty.pca)[,1], PC2=loadings(poverty.pca)[,2], name=names(loadings(poverty.pca)[,1]))

## PCA biplot (Fig 2-a) 
require(ggplot2)
pcplot1 <- ggplot(poverty.pca.loads, aes(x=0, y=0, xend=PC1, yend=PC2, label=name)) +
  geom_text(aes(x=PC1, y=PC2, hjust="left")) +
  geom_segment(arrow=arrow(length=unit(0.2,"cm"))) + theme_bw() + xlab("Wealth axis 1") +
  annotate("text", x=0.025, y=0.65, label="a)", size=10) +
  ylab("Wealth axis 2") + xlim(0,0.85)

## view plot
pcplot1


## PCA scatter plot for all sites with convex hulls around scatter plot (Fig 2-b)
pcplot2 <-ggplot(poverty, aes(PC1, PC2)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.1)+
scale_colour_discrete(name=NULL, guide=guide_legend(keyheight=2, reverse = T)) +
  scale_shape_discrete(name=NULL, guide=guide_legend(keyheight=2, reverse = T)) +
  scale_fill_discrete(name=NULL, guide=guide_legend(keyheight=2, reverse = T)) +
  xlab("Wealth axis 1") +
  ylab(NULL) +
  annotate("text", x=-2, y=6, label="b)", size=10) +
  theme_bw()


#view plot
pcplot2

## Combining two plots to create a panel 
require(grid)
pcplot3 <- grid.draw(cbind(ggplotGrob(pcplot1), ggplotGrob(pcplot2), size="last"))
