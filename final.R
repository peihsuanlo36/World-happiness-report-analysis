library(tidyverse)
library(ggplot2)

data <- read_csv("2019.csv")

str(data)

data$continent <- NA

data$continent[which(data$`Country or region` %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                     "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                     "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong", "Philippines",
                                                     "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                     "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                     "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                     "Cambodia", "Afghanistan", "Yemen", "Syria","Taiwan", "Laos"))] <- "Asia"

data$continent[which(data$`Country or region` %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                     "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                     "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                     "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                     "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                     "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                     "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                     "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                     "Bulgaria", "Albania", "Ukraine","Northern Cyprus", "North Macedonia"))] <- "Europe"

data$continent[which(data$`Country or region` %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                     "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                     "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                     "Haiti"))] <- "North America"

data$continent[which(data$`Country or region` %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                     "Colombia", "Ecuador", "Bolivia", "Peru",
                                                     "Paraguay", "Venezuela", "Trinidad & Tobago"))] <- "South America"

data$continent[which(data$`Country or region` %in% c("New Zealand", "Australia"))] <- "Oceania"

data$continent[which(is.na(data$continent))] <- "Africa"

data$incomelevel <- NA

data$incomelevel[which(data$`Country or region` %in% c("Andorra",
                                                      'Antigua and Barbuda',
                                                      'Argentina',
                                                      'Aruba',
                                                      'Australia',
                                                      'Austria',
                                                      'Bahamas',
                                                      'Bahrain',
                                                      'Barbados',
                                                      'Belgium',
                                                      'Bermuda',
                                                      'British Virgin Islands',
                                                      'Brunei',
                                                      'Canada',
                                                      'Cayman Islands',
                                                      'Channel Islands',
                                                      'Chile',
                                                      'Croatia',
                                                      'Curacao',
                                                      'Cyprus',
                                                      'Czech Republic',
                                                      'Denmark',
                                                      'Estonia',
                                                      'Faroe Islands',
                                                      'Finland',
                                                      'France',
                                                      'French Polynesia',
                                                      'Germany',
                                                      'Gibraltar',
                                                      'Greece',
                                                      'Greenland',
                                                      'Guam',
                                                      'Hong Kong',
                                                      'Hungary',
                                                      'Iceland',
                                                      'Ireland',
                                                      'Israel',
                                                      'Isle of Man',
                                                      'Italy',
                                                      'Japan',
                                                      'Kuwait',
                                                      'Latvia',
                                                      'Liechtenstein',
                                                      'Lithuania',
                                                      'Luxembourg',
                                                      'Macao',
                                                      'Malta',
                                                      'Monaco',
                                                      'Netherlands',
                                                      'New Caledonia',
                                                      'New Zealand',
                                                      'Northern Mariana Islands',
                                                      'Norway',
                                                      'Oman',
                                                      'Palau',
                                                      'Panama',
                                                      'Poland',
                                                      'Portugal',
                                                      'Puerto Rico',
                                                      'Qatar',
                                                      'Saint Kitts and Nevis',
                                                      'Saint Martin',
                                                      'San Marino',
                                                      'Saudi Arabia',
                                                      'Seychelles',
                                                      'Singapore',
                                                      'Sint Maarten',
                                                      'Slovakia',
                                                      'Slovenia',
                                                      'South Korea',
                                                      'Spain',
                                                      'Sweden',
                                                      'Switzerland',
                                                      'Taiwan',
                                                      'Trinidad and Tobago',
                                                      'Turks and Caicos Islands',
                                                      'United Arab Emirates',
                                                      'United Kingdom',
                                                      'United States',
                                                      'Uruguay',
                                                      'U.S. Virgin Islands'))] <- "highincome"

data$incomelevel[which(is.na(data$incomelevel))] <- "not highincome"


# 不會換column names

data <- data %>% select(`Overall rank`, `Country or region`, continent, everything())

data$continent <- as.factor(data$continent)

num.cor <- sapply(data, is.numeric)
cordata <- cor(data[, num.cor])
library(corrplot)
corrplot(cordata, method = "color")

continent_group <- data %>%
  select(-1) %>%
  group_by(continent) %>%
  summarise_at(vars(-`Country or region`), funs(mean(.,na.rm = TRUE)))

library(reshape2)
melt_continent <- melt(continent_group)

ggplot(melt_continent, aes(y = value, x = continent, color = continent, fill = continent)) +
  geom_bar( stat = "identity") +
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of variable between different continents",
       y = "average value")

## 分析各大洲 ##

library(corrgram)

### Africa ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "Africa"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of Africa")

### Asia ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "Asia"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of Asia")

### Europe ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "Europe"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of Europe")

### North America ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "North America"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of North America")

### South America ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "South America"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of South America")

### Oceania ###

corrgram(data %>% 
           select(-1) %>%
           filter(continent == "Oceania"), order = TRUE,
         upper.panel = panel.cor, main = "Mratix of Oceania")


#############################################################

continent_group <- data %>%
  select(-1) %>%
  group_by(continent) %>%
  summarise_at(vars(-`Country or region`), funs(mean(.,na.rm = TRUE)))


incomelevel_group <- data %>%
  select(-1) %>%
  group_by(incomelevel) %>%
  summarise_at(vars(-`Country or region`), funs(mean(.,na.rm = TRUE)))

library(reshape2)
melt_incomelevel <- melt(incomelevel_group)

ggplot(melt_incomelevel, aes(y = value, x = incomelevel, color = incomelevel, fill = incomelevel)) +
  geom_bar( stat = "identity") +
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of variable between different incomelevel",
       y = "average value")

######################################################
ggplot(data, aes(x = `GDP per capita`, y = Score)) + 
  geom_point(aes(color=incomelevel,), size = 3, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(data, aes(x = `Social support`, y = Score)) + 
  geom_point(aes(color=incomelevel,), size = 3, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(data, aes(x = `Healthy life expectancy`, y = Score)) + 
  geom_point(aes(color=incomelevel,), size = 3, alpha = 0.8) +  
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(data, aes(x = `Perceptions of corruption`, y = Score)) + 
  geom_point(aes(color=incomelevel,), size = 3, alpha = 0.8) +  
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(data, aes(x = `Freedom to make life choices`, y = Score)) + 
  geom_point(aes(color=incomelevel,), size = 3, alpha = 0.8) +  
  theme_bw() + labs(title = "Scatter plot with regression line")
#####################################################

### highincome ###

corrgram(data %>% 
           select(-3) %>%
           filter(incomelevel == "highincome"), 
         upper.panel = panel.cor, main = "Mratix of high income")

### not highincome ###

corrgram(data %>% 
           select(-3) %>%
           filter(incomelevel == "not highincome"),
         upper.panel = panel.cor, main = "Mratix of not high income")



### decision tree ###
library(resample)
pre_data <- data[,-c(1:3,5)]

pre_data$incomelevel <- as.factor(ifelse(pre_data$incomelevel == "highincome", 1, 0))

n <- nrow(pre_data)
train_num <- sample(1:n, size = (0.5)*n )
train <- pre_data[train_num,]
test <- pre_data[-train_num,]

library(rpart)
library(rpart.plot)
dtree <- rpart(formula = incomelevel~., data = train, method = "class")
pred <- predict(dtree, newdata = test, type = "class")
rpart.plot(dtree)
table(Real = test$incomelevel, Predict = pred)

library(randomForest)

data2 <- read.csv("2019.csv")
data2$incomelevel <- NA
data2$incomelevel[which(data$`Country or region` %in% c("Andorra",
                                                       'Antigua and Barbuda',
                                                       'Argentina',
                                                       'Aruba',
                                                       'Australia',
                                                       'Austria',
                                                       'Bahamas',
                                                       'Bahrain',
                                                       'Barbados',
                                                       'Belgium',
                                                       'Bermuda',
                                                       'British Virgin Islands',
                                                       'Brunei',
                                                       'Canada',
                                                       'Cayman Islands',
                                                       'Channel Islands',
                                                       'Chile',
                                                       'Croatia',
                                                       'Curacao',
                                                       'Cyprus',
                                                       'Czech Republic',
                                                       'Denmark',
                                                       'Estonia',
                                                       'Faroe Islands',
                                                       'Finland',
                                                       'France',
                                                       'French Polynesia',
                                                       'Germany',
                                                       'Gibraltar',
                                                       'Greece',
                                                       'Greenland',
                                                       'Guam',
                                                       'Hong Kong',
                                                       'Hungary',
                                                       'Iceland',
                                                       'Ireland',
                                                       'Israel',
                                                       'Isle of Man',
                                                       'Italy',
                                                       'Japan',
                                                       'Kuwait',
                                                       'Latvia',
                                                       'Liechtenstein',
                                                       'Lithuania',
                                                       'Luxembourg',
                                                       'Macao',
                                                       'Malta',
                                                       'Monaco',
                                                       'Netherlands',
                                                       'New Caledonia',
                                                       'New Zealand',
                                                       'Northern Mariana Islands',
                                                       'Norway',
                                                       'Oman',
                                                       'Palau',
                                                       'Panama',
                                                       'Poland',
                                                       'Portugal',
                                                       'Puerto Rico',
                                                       'Qatar',
                                                       'Saint Kitts and Nevis',
                                                       'Saint Martin',
                                                       'San Marino',
                                                       'Saudi Arabia',
                                                       'Seychelles',
                                                       'Singapore',
                                                       'Sint Maarten',
                                                       'Slovakia',
                                                       'Slovenia',
                                                       'South Korea',
                                                       'Spain',
                                                       'Sweden',
                                                       'Switzerland',
                                                       'Taiwan',
                                                       'Trinidad and Tobago',
                                                       'Turks and Caicos Islands',
                                                       'United Arab Emirates',
                                                       'United Kingdom',
                                                       'United States',
                                                       'Uruguay',
                                                       'U.S. Virgin Islands'))] <- "highincome"

data2$incomelevel[which(is.na(data2$incomelevel))] <- "not highincome"
data2$incomelevel <- as.factor(ifelse(data2$incomelevel == "highincome", 1, 0))
pre_data2 <-  data[,-c(1, 2, 3, 4)]

n <- nrow(pre_data2)
train_num <- sample(1:n, size = (0.5)*n )
train2 <- pre_data2[train_num,]
test2 <- pre_data2[-train_num,]








pre_data1 <- as.data.frame(pre_data)
train1 <- as.data.frame(pre_data1)
rf <- randomForest(train2$incomelevel~ ., data = train2, importance = TRUE, ntree = 100)

install.packages("systemfit",repos="http://R-Forge.R-project.org") 
library(systemfit)
