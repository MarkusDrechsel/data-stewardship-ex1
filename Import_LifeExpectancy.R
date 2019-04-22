###############################################################
#############          Data Stewardship        ################
##########          Markus Drechsel 1327557       #############
###############################################################

install.packages("tidyverse")
library(tidyverse)

###############################################################
###################           IMPORT        ###################
###############################################################

# Import raw data from excel worksheet and start transforming
LifeExpectancy <- read_excel("tps00150.sdmx.xlsx")
LEintermediate = subset(LifeExpectancy, select = c("indic_he", "geo", "TIME_PERIOD", "OBS_VALUE"))
LEfinalData <- LEintermediate %>% filter(geo == "AT")



###############################################################
###################           PLOT          ###################
###############################################################
install.packages("plotly")
library(plotly)

# extract data from LEfinalData set for the four main criteria
# HLEF = healthy live expectancy female
# LEF  = live expectancy female
# HLEM = healthy live expectancy male
# LEM  = live expectancy male

HLEF <- LEfinalData %>% filter(indic_he == "F_0_DFLE")
LEF <- (LEfinalData %>% filter(indic_he == "F_0_LE"))[1:11,]
HLEM <- LEfinalData %>% filter(indic_he == "M_0_DFLE")
LEM <- (LEfinalData %>% filter(indic_he == "M_0_LE"))[1:11,]
x <- HLEF$TIME_PERIOD

# LOAD BIRTH DATA for Linz
library(readr)
Gebur_2006 <- subset(read_delim("Gebur_2006.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2007 <- subset(read_delim("Gebur_2007.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2008 <- subset(read_delim("Gebur_2008.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2009 <- subset(read_delim("Gebur_2009.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2010 <- subset(read_delim("Gebur_2010.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2011 <- subset(read_delim("Gebur_2011.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2012 <- subset(read_delim("Gebur_2012.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2013 <- subset(read_delim("Gebur_2013.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2014 <- subset(read_delim("Gebur_2014.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2015 <- subset(read_delim("Gebur_2015.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))
library(readr)
Gebur_2016 <- subset(read_delim("Gebur_2016.csv", ";"), select = c("ehelich absolut", "unehelich absolut"))

births2006 <- sum(sum(Gebur_2006$`ehelich absolut`),sum(Gebur_2006$`unehelich absolut`))
births2007 <- sum(sum(Gebur_2007$`ehelich absolut`),sum(Gebur_2007$`unehelich absolut`))
births2008 <- sum(sum(Gebur_2008$`ehelich absolut`),sum(Gebur_2008$`unehelich absolut`))
births2009 <- sum(sum(Gebur_2009$`ehelich absolut`),sum(Gebur_2009$`unehelich absolut`))
births2010 <- sum(sum(Gebur_2010$`ehelich absolut`),sum(Gebur_2010$`unehelich absolut`))
births2011 <- sum(sum(Gebur_2011$`ehelich absolut`),sum(Gebur_2011$`unehelich absolut`))
births2012 <- sum(sum(Gebur_2012$`ehelich absolut`),sum(Gebur_2012$`unehelich absolut`))
births2013 <- sum(sum(Gebur_2013$`ehelich absolut`),sum(Gebur_2013$`unehelich absolut`))
births2014 <- sum(sum(Gebur_2014$`ehelich absolut`),sum(Gebur_2014$`unehelich absolut`))
births2015 <- sum(sum(Gebur_2015$`ehelich absolut`),sum(Gebur_2015$`unehelich absolut`))
births2016 <- sum(sum(Gebur_2016$`ehelich absolut`),sum(Gebur_2016$`unehelich absolut`))

birthsPerYear <- c(births2006, births2007, births2008, births2009, births2010, births2011, births2012, births2013, births2014, births2015, births2016)

# define frame
data <- data.frame(x, HLEF, LEF, HLEM, LEM, birthsPerYear)

#define second y-axis
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Births per year (Linz)"
)


# plot using plotly for R
p <- plot_ly(data, x = ~x) %>%
  add_trace(y = ~HLEF$OBS_VALUE, name = 'healthy life expectancy female',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~LEF$OBS_VALUE, name = 'life expectancy female',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~HLEM$OBS_VALUE, name = 'healthy life expectancy male',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~LEM$OBS_VALUE, name = 'life expectancy male',type = 'scatter', mode = 'lines') %>%
  add_trace(y = birthsPerYear, name = 'Births/Year Linz',type = 'scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Average (healthy) life expectancies by sex",
         xaxis = list(title = "Years"),
         yaxis = list(title = "Life expectancy (years)"),
         yaxis2 = ay )

p


#Scatterplot and regression line for variables HLEF and birthsPerYear
fit <- lm(birthsPerYear ~ HLEF$OBS_VALUE , data)
q <- plot_ly(x = ~HLEF$OBS_VALUE) %>% 
  add_markers(y = ~birthsPerYear, name = 'Correlation HLEF & birthsPerYear') %>% 
  add_lines(x = ~HLEF$OBS_VALUE, y = fitted(fit), name = 'regression line')%>%
  layout(title = "Correlation HLEF and birthsPerYear",
         xaxis = list(title = "Healthy life expectancy female"),
         yaxis = list(title = "Births per year in Linz"))
q

#Scatterplot and regression line for variables HLEM and birthsPerYear
fit <- lm(birthsPerYear ~ HLEM$OBS_VALUE , data)
r <- plot_ly(x = ~HLEM$OBS_VALUE) %>% 
  add_markers(y = ~birthsPerYear, name = 'Correlation HLEM & birthsPerYear') %>% 
  add_lines(x = ~HLEM$OBS_VALUE, y = fitted(fit), name = 'regression line')%>%
  layout(title = "Correlation HLEM and birthsPerYear",
         xaxis = list(title = "Healthy life expectancy male"),
         yaxis = list(title = "Births per year in Linz"))
r

#Scatterplot and regression line for variables LEM and birthsPerYear
fit <- lm(birthsPerYear ~ LEM$OBS_VALUE , data)
s <- plot_ly(x = ~LEM$OBS_VALUE) %>% 
  add_markers(y = ~birthsPerYear, name = 'Correlation LEM & birthsPerYear') %>% 
  add_lines(x = ~LEM$OBS_VALUE, y = fitted(fit), name = 'regression line')%>%
  layout(title = "Correlation LEM and birthsPerYear",
         xaxis = list(title = "Life expectancy male"),
         yaxis = list(title = "Births per year in Linz"))
s

#Scatterplot and regression line for variables LEF and birthsPerYear
fit <- lm(birthsPerYear ~ LEF$OBS_VALUE , data)
t <- plot_ly(x = ~LEF$OBS_VALUE) %>% 
  add_markers(y = ~birthsPerYear, name = 'Correlation LEF & birthsPerYear') %>% 
  add_lines(x = ~LEF$OBS_VALUE, y = fitted(fit), name = 'regression line')%>%
  layout(title = "Correlation LEF and birthsPerYear",
         xaxis = list(title = "Life expectancy female"),
         yaxis = list(title = "Births per year in Linz"))
t




###############################################################
###################           SAVE          ###################
###############################################################

# setup plotly API key with R session
Sys.setenv("plotly_username"="MarkusDrechsel")
Sys.setenv("plotly_api_key"="DhkBJiZMHJjqkCYzFD8H")

# save plot to plotly-workspace
chart_link = api_create(p, filename = "LifeExpectancies")
chart_link

chart_link2 = api_create(q, filename = "Correlation HLEF and birthsPerYear")
chart_link2

chart_link3 = api_create(r, filename = "Correlation HLEM and birthsPerYear")
chart_link3

chart_link4 = api_create(s, filename = "Correlation LEM and birthsPerYear")
chart_link4

chart_link5 = api_create(t, filename = "Correlation LEF and birthsPerYear")
chart_link5

write.csv(data, file = "MyData.csv")
