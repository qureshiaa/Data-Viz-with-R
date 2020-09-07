library(tidyverse)

head(mtcars)

data.frame(mtcars)

# Creating a Bar Chart

bar_chart = qplot(mtcars$cyl,geom = 'bar',fill = I('red'),colour = I("black"),xlab = 'Cylinders',
                  ylab = 'Number of Vehicles',main = 'Cylinders in Vehicles 1974')

bar_chart

# Creating a Histogram

#  110 110  93 110 175 105 245  62  95 123 123 180 180 180 205 215 
#  230  66  52  65  97 150 150 245 175  66  91 113
#  264 175 335 109

histogram = qplot(mtcars$hp, geom = 'histogram',binwidth = 25,colour = I('green'),
                  xlim = c(50,400),xlab = 'HorsePower',ylab = 'Number of Cars')

histogram

# Creating a Scatter Chart

cylfactor = factor(mtcars$cyl)

scatter = qplot(mtcars$mpg,mtcars$wt,color = cylfactor,main = 'ScatterPlot',
                xlab = 'MPG of Vehicles',ylab = 'Weight of Vehicles') +
  geom_point(shape = 19) + labs(color = "Cylinders") + ggtitle("ScatterPlot")

scatter

# Line Plots and Regression

EUStockDF = data.frame(EuStockMarkets)

head(EUStockDF)

#     DAX    SMI    CAC   FTSE
# 1 1628.75 1678.1 1772.8 2443.6
# 2 1613.63 1688.5 1750.5 2460.2
# 3 1606.51 1678.6 1718.0 2448.2
# 4 1621.04 1684.1 1708.1 2470.4
# 5 1618.16 1686.6 1723.1 2484.7
# 6 1610.61 1671.6 1714.3 2466.8

line_plot = ggplot(EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=DAX)) +
  geom_line(size = 1,color = 'Dark Green') + labs(x = 'Time',y='Stocks')

line_plot

dax_smi_cac_plot = ggplot() + 
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=DAX), size = 1,color = 'Dark Green') +
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=SMI), size = 1,color = 'Dark Red') + 
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=CAC), size = 1,color = 'Black') +
  labs(x = 'Time', y='Stocks')
  
dax_smi_plot

dax_smi_cac_plot

all_stocks = ggplot() + 
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=DAX), size = 1,color = 'Dark Green') +
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=SMI), size = 1,color = 'Dark Red') + 
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=CAC), size = 1,color = 'Black') +
  geom_line(data=EUStockDF, aes(x=c(1:nrow(EUStockDF)),y=FTSE), size = 1,color = 'Purple') +
  labs(x = 'Time', y='Stocks')

all_stocks  
  
linear_reg = ggplot(mtcars, aes(x=mpg,y = wt)) + geom_point(shape = 19) + 
  geom_smooth(method = "lm",se=FALSE,color = "green")

#Linear Regression

linear_reg
  
linear_reg_ci = ggplot(mtcars, aes(x=mpg,y = wt)) + geom_point(shape = 19) + 
  geom_smooth(method = "lm",se=TRUE,color = "green")

linear_reg_ci

# Waffle Charts
  
library(waffle)  
  
library(ggplot2)

expenses = c(`Health ($43,212)`=43212,
             `Education ($113,412)`=113412,
             `Transportation ($20,231)` = 20231,
             `Entertainment ($28,145)` = 28145)

#IRkernal
waffle(expenses/1235, rows= 5,size = 0.3,
       colors = c('#c7d4b6',"#a3aabd",'#a0d0de','#97b5cf'),
       title = 'Imaginery Household Expenses Each Year',
       xlab = '1 Square = $934')

# Box Plots

set.seed(1234)

set_a = rnorm(200, mean = 1, sd=2)
set_b = rnorm(200, mean = 1, sd = 1)

df = data.frame(label = factor(rep(c("A","B"), each = 200)), value = c(set_a,set_b))

head(df)
tail(df)

ggplot(df, aes(x=label,y=value)) + geom_boxplot()

ggplotly()

summary(mtcars$mpg)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.40   15.43   19.20   20.09   22.80   33.90

summary(mtcars$cyl)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.000   4.000   6.000   6.188   8.000   8.000 

mtcars$mpg

mtcars$cyl

#cylinders are more of qualitative than a quantitative

new_df <- mtcars[,c("mpg","cyl")]

new_df

arrange(new_df, cyl,mpg)

qplot(factor(cyl),mpg,data=mtcars,geom = 'boxplot')


# Maps

library(leaflet)

map = leaflet()

map = leaflet() %>% addTiles()

map

# Times Square

map = leaflet() %>% addTiles() %>% addMarkers(lng = -73.9851, lat = 40.7589,popup = 'Times Square')

map

head(quakes)

quakes_df = leaflet(quakes) %>% addTiles() %>% addCircles(lng = quakes$long, lat = quakes$lat)

quakes_df

# Shiny

library(shiny)

runExample('01_hello')
