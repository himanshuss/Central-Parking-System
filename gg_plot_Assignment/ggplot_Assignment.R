library(tidyverse)
library(ggalt)
library(GGally)
library(ggridges)

#import the dataset

data_iris <- iris
data_diamonds <- diamonds

setwd("/Users/himanshusahrawat/MBA projects/CPS/gg_plot_Assignment/")
data_names <- read.csv("names.csv", header = TRUE)
data_life <- read.csv("life_expectency.csv", header = TRUE)

head(diamonds,5)

view(data_iris)
head(data_iris,10)
glimpse(data_iris)


#Basic Scatter plot
ggplot(data_iris) +
  geom_point(aes(x = Sepal.Length , y = Sepal.Width),
             color = 'blue', size = 4, pch = 19) +
  
  ggtitle("Basic Scatter Plot", subtitle = "iris") +
  xlab("SEPAL LENGTH") +
  ylab("SEPAL WIDTH") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold.italic', 
                                   color = 'darkgreen',
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = 'bold', 
                                   color = 'blue',
                                   size = 10, angle = 45))

#########################


#Scatter Plot with feature differentiation by color & shape
ggplot(data_iris) +
  geom_point(aes(x = Sepal.Length , y = Sepal.Width,
                 color = Species, shape = Species), size = 4) +
  
  ggtitle("Scatter Plot with feature differentiation - 1") +
  xlab("SEPAL LENGTH") +
  ylab("SEPAL WIDTH") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold.italic', 
                                   color = 'darkgreen',
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = 'bold', 
                                   color = 'blue',
                                   size = 10, angle = 45))

#Scatter Plot with feature differentiation by size & transparency

ggplot(data_iris) +
  geom_point(aes(x = Sepal.Length , y = Sepal.Width,
                 size = Petal.Width, alpha = Petal.Length)) +
  
  ggtitle("Scatter Plot with feature differentiation - 2") +
  xlab("SEPAL LENGTH") +
  ylab("SEPAL WIDTH") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold.italic', 
                                   color = 'darkgreen',
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = 'bold', 
                                   color = 'blue',
                                   size = 10, angle = 45))


#Line Plot
#Basic line plot
head(names,10)

view(names)

ggplot(data_names %>% filter(name == 'Patricia')) +
  geom_line(aes(x = year, y = n), 
            lwd = 1.25, color = 'darkgreen') +
  
  ggtitle("Basic Line Plot", subtitle = "name - Patricia") +
  xlab("YEAR") +
  ylab("NUMBER") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Line Plot with feature differentiation by color & line type
ggplot(data_names) +
  geom_line(aes(x = year, y = n, 
                color = name, lty = name), lwd = 1.25) +
  
  ggtitle("Line Plot with feature differentiation") +
  xlab("YEAR") +
  ylab("NUMBER") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#BAR PLoT
#Basic BAR Plot
as.data.frame(table(data_diamonds$cut))
ggplot(as.data.frame(table(data_diamonds$cut))) +
  geom_bar(aes(x = Var1, y = Freq), 
           stat = 'identity', fill = 'darkgreen') +
  
  ggtitle("Basic Bar Plot") +
  xlab("cut") +
  ylab("count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Basic Bar Plot with polar transformation
as.data.frame(table(data_diamonds$cut))
ggplot(as.data.frame(table(data_diamonds$cut))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  
  ggtitle("Bar Plot with polar transformation") +
  xlab("cut") +
  ylab("count") +
  coord_polar("y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Basic Bar Plot- 2
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = cut),
           color = 'lightblue') +
  
  ggtitle("Basic Bar Plot - 2") +
  xlab("cut") +
  ylab("count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Stacked Bar Plot
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity), 
           color = 'lightblue') +
  
  ggtitle("Stacked Bar Plot") +
  xlab("cut") +
  ylab("count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Stacked Bar Plot in same height

ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity), position = 'fill') +
  
  ggtitle("Stacked Bar Plot (same height)") +
  xlab("cut") +
  ylab("count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Grouped Bar Plot
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity), position = 'dodge') +
  
  ggtitle("Grouped Bar Plot") +
  xlab("cut") +
  ylab("count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Stacked Bar Plot with polar transformation
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity)) +
  
  ggtitle("Stacked Polar (1)") +
  xlab("cut") +
  ylab("count") +
  
  coord_polar("y") +
  
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
#Stacked Bar Plot with polar transformation- 2
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity)) +
  
  ggtitle("Stacked Polar (2)") +
  xlab("cut") +
  ylab("count") +
  
  coord_polar() +
  
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Tiles Plot

as.data.frame(table(data_diamonds$cut, data_diamonds$color))
ggplot(as.data.frame(table(data_diamonds$cut, 
                           data_diamonds$color))) +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = Freq), 
            color = "yellow") +
  
  ggtitle("Tiles Plot") +
  xlab("cut") +
  ylab("COLOR") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Dumblble Plot
ggplot(data_life) +
  geom_dumbbell(aes(x = Y1967, xend = Y2007, 
                    y = country, group = country),
                colour = 'grey', size = 4,
                colour_x = 'green', colour_xend = 'blue') +
  
  ggtitle("Dumbbell Plot (Life Expenctancy 1967 ~ 2007)") +
  xlab('YEAR') +
  ylab('COUNTRY') +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Scatter Matrix Plot
#Basic Scatter Matrix Plot
ggpairs(data_iris[ , 1:4]) +
  theme_bw()

#Scatter Matrix Plot with feature differentiation
ggpairs(data_iris, aes(color = Species)) +
  theme_bw()

#Density Plot
ggplot(data_diamonds) +
  geom_density(aes(x = carat, 
                   fill = cut), alpha = 0.7)+
  
  ggtitle("Density Plot") +
  xlab("price") +
  ylab("Density") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


#Box plot
ggplot(data_diamonds) +
  geom_boxplot(aes(x = cut, y = carat, fill = cut)) +
  
  ggtitle("Basic Box Plot") +
  xlab("cut") +
  ylab("CARAT") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Grouped Box Plot
ggplot(data_diamonds) +
  geom_boxplot(aes(x = cut, y = carat, fill = clarity)) +
  ggtitle("Grouped Box Plot") +
  xlab("cut") +
  ylab("CARAT")+
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Box Plot with polar transformation
ggplot(data_diamonds) +
  geom_boxplot(aes(x = color, y = price, fill = color)) +
  
  ggtitle("Box Plot with polar transform") +
  coord_polar("y") +
  
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Violin Plot
ggplot(data_diamonds) +
  geom_violin(aes(x = cut , y = carat, fill = cut)) +
  
  ggtitle("Violin Plot") +
  xlab("cut") +
  ylab("Carat") +
  theme_bw() +
  
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Ridge Plot
ggplot(data_diamonds) +
  geom_density_ridges(aes(x = carat , y = cut, 
                          fill = clarity), alpha = 0.7) +
  ggtitle("Ridge Plot") +
  xlab("price") +
  ylab("cut") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="/Users/himanshusahrawat/MBA projects/CPS/gg_plot_Assignment/")
