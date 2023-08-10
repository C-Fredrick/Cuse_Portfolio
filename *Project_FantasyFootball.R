library(tidyverse)
library(corrplot) #correlation matrix plot
library(factoextra)

setwd('/Users/christopherfredrick/CuseGradSchool')
df2016 <- read_csv('2016.csv')

FilterFunct <- function(df, position = '') {
  
  #Creating data frame filtered by position
   df <- df %>%
    filter(Pos == position, GS >= 8) %>% #Keep players who've started 8 games or more
    mutate(`FP/G` = FantasyPoints / G) %>% #Create new column Fantasy points per games started
    mutate(id = as.numeric(rownames(.))) %>%#Created id column because the cluster visualization requires two or more numeric variables
    select(c('id', 'Player', 'Tm', 'FP/G'))
   
   
  
}
FilterFunct2 <- function(df, position = '') {
  
  #Creating data frame filtered by position
  df <- df %>%
    filter(Pos == position, GS >= 8) %>% #Keep players who've started 8 games or more
    mutate(`FP/G` = FantasyPoints / G) %>% #Create new column Fantasy points per games started
    mutate(id = as.numeric(rownames(.))) %>%#Created id column because the cluster visualization requires two or more numeric variables
    group_by(Tm) %>% # Use this line and the next after getting WR df
    slice(which.max(`FP/G`)) %>% #Keeps higher value of duplicate
    select(c('id', 'Player', 'Tm', 'FP/G'))
  
  
  
}

PlotFunct <- function(df) {
  #Plot id vs Fantasy Points
  df %>%
    ggplot()+aes(x = id, y = `FP/G` )+geom_point()
}

################## Finding Tier Values  ###############################################

df2018 <- read_csv('2018.csv')
df2017 <- read_csv('2017.csv')
df2016 <- read_csv('2016.csv')
dfAll <- rbind(df2016, df2017, df2018)

#------------------WR Tiers --------------------------------------
WR <- FilterFunct(dfAll, 'WR')
PlotFunct(WR)

#Cluster and visualize
k <- kmeans(WR$`FP/G` , centers = 4, nstart = 25 )
fviz_cluster(k, geom = 'point', data = WR[c(1,4)])

#Placing WRs into tiers
WR <- cbind(WR, cluster = k$cluster)
WR %>%
  mutate(WRTiers = case_when(
    cluster == 2 ~ 'Tier 1',
    cluster == 4 ~ 'Tier 2',
    cluster == 3 ~ 'Tier 3',
    cluster == 1 ~ 'Tier 4'
  )) %>%
  select(-cluster, -id) -> WR

WR %>% # Getting the Tier ranges 
  group_by(WRTiers) %>%
  summarise(max(`FP/G`))
  

# Tier 1 values (16.3 +)
# Tier 2 values (12.2 - 16.2)
# Tier 3 values (8.19 - 12.1)
# Tier 4 values (0 - 8.18)

#-------------------- QB Tier Vals ---------------------------------


QB <- FilterFunct(dfAll, 'QB')
PlotFunct(QB)

#Cluster and visualize
k <- kmeans(QB$`FP/G` , centers = 4, nstart = 25 )
fviz_cluster(k, geom = 'point', data = QB[c(1,4)])

#Placing WRs into tiers
QB <- cbind(QB, cluster = k$cluster)
QB %>%
  mutate(QBTiers = case_when(
    cluster == 4 ~ 'Tier 1',
    cluster == 2 ~ 'Tier 2',
    cluster == 3 ~ 'Tier 3',
    cluster == 1 ~ 'Tier 4'
  )) %>%
  select(-cluster, -id) -> QB

QB %>% # Getting the Tier ranges 
  group_by(QBTiers) %>%
  summarise(max(`FP/G`))
# Tier 1 (19.3 +)
# Tier 2 (15.5 - 19.2)
# Tier 3 (11.8 - 15.4)
# Tier 4 (0 - 11.7)



#------------------ RB Tiers ------------------------------------
RB <- FilterFunct(dfAll, 'RB')
PlotFunct(RB)

#Cluster and visualize
k <- kmeans(RB$`FP/G` , centers = 4, nstart = 25 )
fviz_cluster(k, geom = 'point', data = RB[c(1,4)])

#Placing WRs into tiers
RB <- cbind(RB, cluster = k$cluster)
RB %>%
  mutate(RBTiers = case_when(
    cluster == 3 ~ 'Tier 1',
    cluster == 1 ~ 'Tier 2',
    cluster == 4 ~ 'Tier 3',
    cluster == 2 ~ 'Tier 4'
  )) %>%
  select(-cluster, -id) -> RB

RB %>% # Getting the Tier ranges 
  group_by(RBTiers) %>%
  summarise(max(`FP/G`))
# Tier 1 (18.5 +)
# Tier 2 (12.7 - 18.4)
# Tier 3 (5.42 - 12.6)
# Tier 4 (0 - 5.41)


#------------------------------- TE -------------------------------------
TE <- FilterFunct(dfAll, 'TE')
PlotFunct(TE)

#Cluster and visualize
k <- kmeans(TE$`FP/G` , centers = 4, nstart = 25 )
fviz_cluster(k, geom = 'point', data = TE[c(1,4)])

#Placing WRs into tiers
TE <- cbind(TE, cluster = k$cluster)
TE %>%
  mutate(TETiers = case_when(
    cluster == 2 ~ 'Tier 1',
    cluster == 1 ~ 'Tier 2',
    cluster == 3 ~ 'Tier 3',
    cluster == 4 ~ 'Tier 4'
  )) %>%
  select(-cluster, -id) -> TE

TE %>% # Getting the Tier ranges 
  group_by(TETiers) %>%
  summarise(max(`FP/G`))
# Tier 1 (11.7 +)
# Tier 2 (7.24 - 11.6)
# Tier 3 (3.34 - 7.23)
# Tier 4 (0 - 3.33)

#----------------- WR 2016 --------------------------------------

WR2016 <- FilterFunct(df2016, 'WR')
PlotFunct(WR2016)

#Discretize
WRTiers <- cut(WR2016$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
    labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WR2016 <- cbind(WR2016, WRTiers)
WR2016 <- select(WR2016, -id)

#------------------- QB 2016------------------------------------------

QB2016 <- FilterFunct2(df2016, 'QB')

#Discretize
QBTiers <- cut(QB2016$`FP/G`, breaks = c(0,11.7,15.4,19.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

QB2016 <- cbind(QB2016, QBTiers = QBTiers)
QB2016 <- select(QB2016, -id)

#Merging WR and QB data
New2016 <- merge(x = WR2016, y = QB2016, by = 'Tm', all.x = T ) #left join
New2016 <- na.omit(New2016)
New2016$QBTiers <- as.factor(New2016$QBTiers)
New2016$WRTiers <- as.factor(New2016$WRTiers)

#------------------ RB 2016-------------------------------------------

#Adding more positions to data
RB2016 <- FilterFunct2(df2016, 'RB')

#Discretize
RBTiers <- cut(RB2016$`FP/G`, breaks = c(0,5.41,12.6,18.4,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

RB2016 <- cbind(RB2016, RBTiers = RBTiers)
RB2016 <- select(RB2016, -id)


Merger <- function(df){
  New2016 <- merge(x = New2016, y = df, by = 'Tm', all.x = T ) #left join
  New2016 <- na.omit(New2016)
}

New2016 <- Merger(RB2016)
New2016$RBTiers <- as.factor(New2016$RBTiers)

#-------------------- TE 2016------------------------------------------
TE2016 <- FilterFunct2(df2016, 'TE')
TETiers <- cut(TE2016$`FP/G`, breaks = c(0,3.33,7.23,11.6,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

TE2016 <- cbind(TE2016, TETiers = TETiers)
TE2016 <- select(TE2016, -id)

New2016 <- Merger(TE2016)
New2016$TETiers <- as.factor(New2016$TETiers)

#---------------- WR Teammates --------------------------------

df2016 %>%
  filter(Pos == 'WR') %>%
  mutate(`FP/G` = FantasyPoints/G) %>%
  group_by(Tm) %>%
  slice_max(order_by = `FP/G`, n = 4) -> WRPerTeam

#Discretize
WRTiers <- cut(WRPerTeam$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))
  
WRPerTeam <- cbind(WRPerTeam, WRTiers = WRTiers)
WRPerTeam %>%
  select(Player, Tm, `FP/G`, WRTiers) -> WRPerTeam

#Placing other 3 WR in same row
otherWR <- data.frame()
for (i in seq(from=1, to=nrow(WRPerTeam), by=4)){
  a <- WRPerTeam[i,]
  b <- WRPerTeam[i+1,]
  c <- WRPerTeam[i+2,]
  d <- WRPerTeam[i+3,]
  e <- cbind(a,b,c,d)
  f <- cbind(b,a,c,d)
  g <- cbind(c,a,b,d)
  h <- cbind(d,a,b,c)
  otherWR <- rbind(otherWR,e,f,g,h)

 
}

NameCol <- c('Team', 'WRName', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers')
colnames(New2016) <- NameCol

names(otherWR)[1] <- 'Player.x'
New2016 <- merge(x = New2016, y = otherWR, by.x ='WRName', by.y = 'Player.x',
                 all.x = T)
New2016 <- na.omit(New2016)
NameCol <- c('WRName', 'Team', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers', 
             'WR1Team', 'WR1Points', 'WR1Tiers',
             'WR2Name', 'WR2Team', 'WR2Points', 'WR2Tiers',
             'WR3Name', 'WR3Team', 'WR3Points', 'WR3Tiers',
             'WR4Name', 'WR4Team', 'WR4Points', 'WR4Tiers'
             )
colnames(New2016) <- NameCol
New2016 <- select(New2016, c(-'WR1Team', -'WR1Points', -'WR1Tiers',
                             -'WR2Team', -'WR3Team', -'WR4Team'))


#----------------- WR 2017 --------------------------------------

WR2017 <- FilterFunct(df2017, 'WR')

#Discretize
WRTiers <- cut(WR2017$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WR2017 <- cbind(WR2017, WRTiers = WRTiers)
WR2017 <- select(WR2017, -id)

#------------------- QB 2017------------------------------------------

QB2017 <- FilterFunct2(df2017, 'QB')

#Discretize
QBTiers <- cut(QB2017$`FP/G`, breaks = c(0,11.7,15.4,19.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

QB2017 <- cbind(QB2017, QBTiers = QBTiers)
QB2017 <- select(QB2017, -id)

#Merging WR and QB data
New2017 <- merge(x = WR2017, y = QB2017, by = 'Tm', all.x = T ) #left join
New2017 <- na.omit(New2017)
New2017$QBTiers <- as.factor(New2017$QBTiers)
New2017$WRTiers <- as.factor(New2017$WRTiers)

#------------------ RB 2017-------------------------------------------

#Adding more positions to data
RB2017 <- FilterFunct2(df2017, 'RB')

#Discretize
RBTiers <- cut(RB2017$`FP/G`, breaks = c(0,5.41,12.6,18.4,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

RB2017 <- cbind(RB2017, RBTiers = RBTiers)
RB2017 <- select(RB2017, -id)


Merger <- function(df){
  New2017 <- merge(x = New2017, y = df, by = 'Tm', all.x = T ) #left join
  New2017 <- na.omit(New2017)
}

New2017 <- Merger(RB2017)
New2016$RBTiers <- as.factor(New2016$RBTiers)

#-------------------- TE 2017------------------------------------------
TE2017 <- FilterFunct2(df2017, 'TE')
TETiers <- cut(TE2017$`FP/G`, breaks = c(0,3.33,7.23,11.6,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

TE2017 <- cbind(TE2017, TETiers = TETiers)
TE2017 <- select(TE2017, -id)

New2017 <- Merger(TE2017)
New2017$TETiers <- as.factor(New2017$TETiers)

#---------------- WR Teammates 2017--------------------------------

df2017 %>%
  filter(Pos == 'WR') %>%
  mutate(`FP/G` = FantasyPoints/G) %>%
  group_by(Tm) %>%
  slice_max(order_by = `FP/G`, n = 4) -> WRPerTeam2017

#Discretize
WRTiers <- cut(WRPerTeam2017$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WRPerTeam2017 <- cbind(WRPerTeam2017, WRTiers = WRTiers)
WRPerTeam2017 %>%
  select(Player, Tm, `FP/G`, WRTiers) -> WRPerTeam2017

#Placing other 3 WR in same row
otherWR2017 <- data.frame()
for (i in seq(from=1, to=nrow(WRPerTeam2017), by=4)){
  a <- WRPerTeam2017[i,]
  b <- WRPerTeam2017[i+1,]
  c <- WRPerTeam2017[i+2,]
  d <- WRPerTeam2017[i+3,]
  e <- cbind(a,b,c,d)
  f <- cbind(b,a,c,d)
  g <- cbind(c,a,b,d)
  h <- cbind(d,a,b,c)
  otherWR2017 <- rbind(otherWR2017,e,f,g,h)
  
  
}

NameCol <- c('Team', 'WRName', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers')
colnames(New2017) <- NameCol

names(otherWR2017)[1] <- 'Player.x'
New2017 <- merge(x = New2017, y = otherWR, by.x ='WRName', by.y = 'Player.x',
                 all.x = T)
New2017 <- na.omit(New2017)
NameCol <- c( 'WRName','Team', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers', 
             'WR1Team', 'WR1Points', 'WR1Tiers',
             'WR2Name', 'WR2Team', 'WR2Points', 'WR2Tiers',
             'WR3Name', 'WR3Team', 'WR3Points', 'WR3Tiers',
             'WR4Name', 'WR4Team', 'WR4Points', 'WR4Tiers'
)
colnames(New2017) <- NameCol
New2017 <- select(New2017, c(-'WR1Team', -'WR1Points', -'WR1Tiers',
                             -'WR2Team', -'WR3Team', -'WR4Team'))
#----------------- WR 2018 --------------------------------------

WR2018 <- FilterFunct(df2018, 'WR')

#Discretize
WRTiers <- cut(WR2018$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WR2018 <- cbind(WR2018, WRTiers = WRTiers)
WR2018 <- select(WR2018, -id)

#------------------- QB 2018------------------------------------------

QB2018 <- FilterFunct2(df2018, 'QB')

#Discretize
QBTiers <- cut(QB2018$`FP/G`, breaks = c(0,11.7,15.4,19.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

QB2018 <- cbind(QB2018, QBTiers = QBTiers)
QB2018 <- select(QB2018, -id)

#Merging WR and QB data
New2018 <- merge(x = WR2018, y = QB2018, by = 'Tm', all.x = T ) #left join
New2018 <- na.omit(New2018)
New2018$QBTiers <- as.factor(New2018$QBTiers)
New2018$WRTiers <- as.factor(New2018$WRTiers)

#------------------ RB 2018-------------------------------------------

#Adding more positions to data
RB2018 <- FilterFunct2(df2018, 'RB')

#Discretize
RBTiers <- cut(RB2018$`FP/G`, breaks = c(0,5.41,12.6,18.4,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

RB2018 <- cbind(RB2018, RBTiers = RBTiers)
RB2018 <- select(RB2018, -id)


Merger <- function(df){
  New2018 <- merge(x = New2018, y = df, by = 'Tm', all.x = T ) #left join
  New2018 <- na.omit(New2018)
}

New2018 <- Merger(RB2018)
New2018$RBTiers <- as.factor(New2018$RBTiers)

#-------------------- TE 2018------------------------------------------
TE2018 <- FilterFunct2(df2018, 'TE')
TETiers <- cut(TE2018$`FP/G`, breaks = c(0,3.33,7.23,11.6,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

TE2018 <- cbind(TE2018, TETiers = TETiers)
TE2018 <- select(TE2018, -id)

New2018 <- Merger(TE2018)
New2018$TETiers <- as.factor(New2018$TETiers)

#---------------- WR Teammates 2018--------------------------------

df2018 %>%
  filter(Pos == 'WR') %>%
  mutate(`FP/G` = FantasyPoints/G) %>%
  group_by(Tm) %>%
  slice_max(order_by = `FP/G`, n = 4) -> WRPerTeam2018

#Discretize
WRTiers <- cut(WRPerTeam2018$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WRPerTeam2018 <- cbind(WRPerTeam2018, WRTiers = WRTiers)
WRPerTeam2018 %>%
  select(Player, Tm, `FP/G`, WRTiers) -> WRPerTeam2018

#Placing other 3 WR in same row
otherWR2018 <- data.frame()
for (i in seq(from=1, to=nrow(WRPerTeam2018), by=4)){
  a <- WRPerTeam2018[i,]
  b <- WRPerTeam2018[i+1,]
  c <- WRPerTeam2018[i+2,]
  d <- WRPerTeam2018[i+3,]
  e <- cbind(a,b,c,d)
  f <- cbind(b,a,c,d)
  g <- cbind(c,a,b,d)
  h <- cbind(d,a,b,c)
  otherWR2018 <- rbind(otherWR2018,e,f,g,h)
  
  
}

NameCol <- c('Team', 'WRName', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers')
colnames(New2018) <- NameCol

names(otherWR2018)[1] <- 'Player.x'
New2018 <- merge(x = New2018, y = otherWR, by.x ='WRName', by.y = 'Player.x',
                 all.x = T)
New2018 <- na.omit(New2018)
NameCol <- c('WRName','Team',  'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers', 
             'WR1Team', 'WR1Points', 'WR1Tiers',
             'WR2Name', 'WR2Team', 'WR2Points', 'WR2Tiers',
             'WR3Name', 'WR3Team', 'WR3Points', 'WR3Tiers',
             'WR4Name', 'WR4Team', 'WR4Points', 'WR4Tiers'
)
colnames(New2018) <- NameCol
New2018 <- select(New2018, c(-'WR1Team', -'WR1Points', -'WR1Tiers',
                             -'WR2Team', -'WR3Team', -'WR4Team'))
#-------------------Combine Years ----------------------------

Fantasy <- rbind(New2016,New2017,New2018)
summary(Fantasy)


Tiers <- select(Fantasy, c('WRName','WRTiers','QBTiers','RBTiers',
                           'TETiers','WR2Tiers','WR3Tiers','WR4Tiers'))

PointAvg <- select(Fantasy, c('WRTiers', 'QBPoints','RBPoints',
                              'TEPoints','WRPoints', 'WR2Points',
                              'WR3Points', 'WR4Points'))

############Creating 2019 Testing Data set############################

df2019 <- read_csv('2019.csv')

#----------------------WR 2019 ------------------------------------------
WR2019 <- FilterFunct(df2019, 'WR')

#Discretize
WRTiers <- cut(WR2019$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WR2019 <- cbind(WR2019, WRTiers = WRTiers)
WR2019 <- select(WR2019, -id)

#------------------- QB 2018------------------------------------------

QB2019 <- FilterFunct2(df2019, 'QB')

#Discretize
QBTiers <- cut(QB2019$`FP/G`, breaks = c(0,11.7,15.4,19.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

QB2019 <- cbind(QB2019, QBTiers = QBTiers)
QB2019 <- select(QB2019, -id)

#Merging WR and QB data
New2019 <- merge(x = WR2019, y = QB2019, by = 'Tm', all.x = T ) #left join
New2019 <- na.omit(New2019)
New2019$QBTiers <- as.factor(New2019$QBTiers)
New2019$WRTiers <- as.factor(New2019$WRTiers)

#------------------ RB 2018-------------------------------------------

#Adding more positions to data
RB2019 <- FilterFunct2(df2019, 'RB')

#Discretize
RBTiers <- cut(RB2019$`FP/G`, breaks = c(0,5.41,12.6,18.4,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

RB2019 <- cbind(RB2019, RBTiers = RBTiers)
RB2019 <- select(RB2019, -id)


Merger <- function(df){
  New2019 <- merge(x = New2019, y = df, by = 'Tm', all.x = T ) #left join
  New2019 <- na.omit(New2019)
}

New2019 <- Merger(RB2019)
New2019$RBTiers <- as.factor(New2019$RBTiers)

#-------------------- TE 2018------------------------------------------
TE2019 <- FilterFunct2(df2019, 'TE')
TETiers <- cut(TE2019$`FP/G`, breaks = c(0,3.33,7.23,11.6,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

TE2019 <- cbind(TE2019, TETiers = TETiers)
TE2019 <- select(TE2019, -id)

New2019 <- Merger(TE2019)
New2019$TETiers <- as.factor(New2019$TETiers)

#---------------- WR Teammates 2018--------------------------------

df2019 %>%
  filter(Pos == 'WR') %>%
  mutate(`FP/G` = FantasyPoints/G) %>%
  group_by(Tm) %>%
  slice_max(order_by = `FP/G`, n = 4) -> WRPerTeam2019

#Discretize
WRTiers <- cut(WRPerTeam2019$`FP/G`, breaks = c(0,8.18,12.1,16.2,Inf),
               labels = c('Tier 4', 'Tier 3', 'Tier 2', 'Tier 1'))

WRPerTeam2019 <- cbind(WRPerTeam2019, WRTiers = WRTiers)
WRPerTeam2019 %>%
  select(Player, Tm, `FP/G`, WRTiers) -> WRPerTeam2019

#Placing other 3 WR in same row
otherWR2019 <- data.frame()
for (i in seq(from=1, to=nrow(WRPerTeam2019), by=4)){
  a <- WRPerTeam2019[i,]
  b <- WRPerTeam2019[i+1,]
  c <- WRPerTeam2019[i+2,]
  d <- WRPerTeam2019[i+3,]
  e <- cbind(a,b,c,d)
  f <- cbind(b,a,c,d)
  g <- cbind(c,a,b,d)
  h <- cbind(d,a,b,c)
  otherWR2019 <- rbind(otherWR2019,e,f,g,h)
  
  
}

NameCol <- c('Team', 'WRName', 'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers')
colnames(New2019) <- NameCol

names(otherWR2019)[1] <- 'Player.x'
New2019 <- merge(x = New2019, y = otherWR, by.x ='WRName', by.y = 'Player.x',
                 all.x = T)
New2019 <- na.omit(New2019)
NameCol <- c('WRName','Team',  'WRPoints', 'WRTiers',
             'QBName', 'QBPoints', 'QBTiers',
             'RBName', 'RBPoints', 'RBTiers',
             'TEName', 'TEPoints', 'TETiers', 
             'WR1Team', 'WR1Points', 'WR1Tiers',
             'WR2Name', 'WR2Team', 'WR2Points', 'WR2Tiers',
             'WR3Name', 'WR3Team', 'WR3Points', 'WR3Tiers',
             'WR4Name', 'WR4Team', 'WR4Points', 'WR4Tiers'
)
colnames(New2019) <- NameCol
New2019 <- select(New2019, c(-'WR1Team', -'WR1Points', -'WR1Tiers',
                             -'WR2Team', -'WR3Team', -'WR4Team'))
#-------------------Combine Years ----------------------------

Fantasy <- rbind(New2016,New2017,New2018, New2019)
summary(Fantasy)


Tiers <- select(Fantasy, c('WRName','WRTiers','QBTiers','RBTiers',
                           'TETiers','WR2Tiers','WR3Tiers','WR4Tiers'))

PointAvg <- select(Fantasy, c('WRTiers', 'QBPoints','RBPoints',
                              'TEPoints','WRPoints', 'WR2Points',
                              'WR3Points', 'WR4Points'))




############## Models ########################
library(caret)
library(rpart)
library(e1071)

TrainIndex <- createDataPartition(Tiers$WRTiers, p=.7, list = F)
TrainSet <- Tiers[TrainIndex,]
TestSet <- Tiers[-TrainIndex,]
TestLabels <- TestSet$WRTiers
TestSet <- TestSet[-2]

#Naive Bayes
NB <- naiveBayes(WRTiers ~ ., data = TrainSet[-1], laplace = 1)
NBPred <- predict(NB, TestSet[-1], type = 'class')
confusionMatrix(NBPred, TestLabels)

#CV Naive Bayes Tiers
NB_CV <- train(WRTiers ~., data = TrainSet[-1], method = 'naive_bayes',
               trControl = trainControl(method = 'cv', number = 10, savePredictions = T))

NB_CVPred <- predict(NB_CV, TestSet[-1])
confusionMatrix(NB_CVPred, TestLabels)


#Random Forest
library(randomForest)
library(janitor)

rf <- randomForest(WRTiers~., data = TrainSet[-1], proximity = T)

rf_pred <- predict(rf, TestSet[-1])
confusionMatrix(rf_pred, TestLabels)


#-------------Fantasy Points Data ----------------------------------------
PointAvg <- select(PointAvg, -'WRPoints')
TrainIndex <- createDataPartition(PointAvg$WRTiers, p=.7, list = F)
TrainSet <- PointAvg[TrainIndex,]
TestSet <- PointAvg[-TrainIndex,]
TestLabels <- TestSet$WRTiers
TestSet <- TestSet[-1]


#SVM 
SVM <- svm(WRTiers ~ ., data = TrainSet, kernel = 'linear', cost = 1, scale = F)
SVMPred <- predict(SVM, TestSet)
confusionMatrix(SVMPred, TestLabels)

SVM <- svm(WRTiers ~ ., data = TrainSet, kernel = 'polynomial', cost = 10000, scale = F)
SVMPred <- predict(SVM, TestSet)
confusionMatrix(SVMPred, TestLabels)

#KNN
library(class)
KNN_Model <- knn(train = TrainSet[-1], test = TestSet, cl = TrainSet$WRTiers, k = 1, prob =F)
confusionMatrix(KNN_Model, TestLabels)















