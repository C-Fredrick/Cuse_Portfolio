#########This code does K-means cluster analyses using the Wahoo Customer data and performs some profile tabulations.


###########you need to first set the working directory for R to the directory of the source code and data

#setwd(" set it to the directory of the source code and data")

############setting the working directory can also be automated by the following
############ but it causes some funny restarting of R that you need to live through

setwd('/Users/christopherfredrick/CuseGradSChool/MAR 653')

#################

############This code allows you to conduct k-means cluster analyses, visualize elbow plots
############and conduct profile analyses for the wahoo segmentation data


###########We need the following libraries for this analyses, 
###########first install the packages either using the tools menu or the function, 
###########install.packages()

#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("dplyr")
#install.packages("lsa")
install.packages("gt")
#install.packages("scales")
install.packages('rlang')

library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)
library(lsa)
library(gt)
library(scales)


###########read the segmentation data. We need to first create a comma seperated file in excel, and read that comma separated file in R

segdat<-read.csv(file="wahoo_clustdat_jan23.csv")

############the "filter" command from the "dplyr" library selects only wahoo customers or "non-sample" in the data for our analyses. You can repeat the analysis
############for customers or non-customers by changing this line of code.
############the command below selects customers from the segmentation data

segdatc1<-segdat%>%filter(Q109_1=="non-sample")



#######the next few lines will process the data for segmentation. We need to first mean center the segmentation questions. 
#######here for each of the segmenation questions, we take the mean of those questions for each respondent. 
#######then we subtract each segmentation question from the corresponding respondent's mean. 
#######these mean centered questions control for the bias or noise created when some respondent provides higher or lower responses to all questions in general.

######## consider an example of using three questions q1, q2, and q3 for segmentation.
####### consider respondent 1, R1 answered 5, 4, and 5 for q1, q2, and q3 respectively.
####### we would first take the mean for R1 on q1, q2, and q3, which is 4.667
####### the mean centered value for q1, lets call this mc_q1 would be 5-4.667, which is .33.
####### similarly the mean centered values for q2 and q3 will be -.667, and .33.
####### the following lines of codes takes this logic and applies it to the wahoo segmentation data


###########select columns 57 through 79 as the segmentation questions and assign them to a new dataset called, "segment_dat".
segment_dat<-segdatc1[,57:79]


###########find the means of the segmentation questions for each respondent and assign it to the column named, "mean"

segment_dat$mean=rowMeans(segment_dat)

#######for each respondent compute the difference between the segmentation questions 

segment_mcdat<-data.matrix(segment_dat[,1:23]-segment_dat$mean)

####the next few lines will determine optimal number of clusters in the wahoo customer database
set.seed(123)

# Compute and plot the ratio of within sum of squares to between sum of squares. The following function automatically provides you the necessary data for the plot 

####compute a distance matrix of these columns using "get_dist". This function computes a matrix of distances with columns from the dataset as inputs.
######### For example, if we have 10 columns, the function will return a 10 x 10 matrix of distance, 
##########and the diagonal elements will have a distance of zero because they would represent the distance from a variable to the same variable.
##########We are using the manhattan method to compute distance. other methods include euclidean, cosine etc. Manhattan seems to perform well in our data.

gdist<-get_dist(segment_mcdat,method = "manhattan")

####use the following set of code to get the ratio of within to between cluster variances############
######the function takes the number of clusters as an input, then calculates k-means for the specified number of clusters######
#######the other options in kmeans include, the number of different random seeds (30 in our case) and the number of maximum iterations to try before giving up on convergence

# function to compute total within-cluster sum of square 
wss <- function(k) {
  km<-kmeans(gdist, k, nstart = 30, iter.max=500)
  
  km$tot.withinss/km$betweenss
  
 
 }

############set the number of clusters you want to try for the analyses
k.values <- 2:8

# extract the ratio of within to between cluster variance for 2-8 clusters using the function wss() that was defined before
wss_values <- map_dbl(k.values, wss)

##########the following gives you the elbow plot using the wss_values that was just computed

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-between clusters variance")

###repeat cluster analyses with the optimal number of clusters. We use the plot to determine the number of cluster where an elbow is detected.
kopt<-kmeans(gdist,centers=3,nstart=25)

###########attach cluster membership for each customer to the orginal dataset and write that file as a comma seperate document.

cluster<-kopt$cluster

segdatc=cbind(segdatc1,cluster)

write.csv(segdatc,file="wahoo_segments_clustmem_cust_jan23.csv")

############find means of segmentation and profile variables by cluster
#############in the code below, we are taking the means of each variable
################for each cluster group
############you can use these means to describe the segments
###########the characteristics or preferences of each segment (the means of segmentation variables)


################find the number of respondents in each cluster
meansbyseg<-table(segdatc$cluster)


#########the term "group_by" from the library "dplyr" provide the means by cluster. The term "summarise" with the "across" option computes the means for the specified questions.

l<-t(segdatc%>%group_by(cluster)%>%summarise(across(Q1_1:Q2_12, mean)))
l

#########remove the first row which just provides the cluster number from the output and add the rows which contain means of the segmentation variable to the table, "meansbyseg"

meansbyseg<-rbind(meansbyseg,l[-1,])
meansbyseg

######add column names to "meansbyseg"
cnames<-c("cluster1","cluster2","cluster3")

colnames(meansbyseg)=cnames

meansbyseg

##############the file rownames contains the names of the quesions used for segmentation. We will read these names into the file "rownames" and add it as a new column to "meansbyseg"


################attach the names of the variables#########################
rownames<-read.csv(file="rownames.csv")
meansbyseg<-cbind(meansbyseg,rownames)
meansbyseg

######creates a color palette for conditional formatting"
seg_palette <- col_numeric(c("#FEF0D9", "#990000"), domain = c(1, 6), alpha = 0.75)

##########the following lines creates a table using the function "gt" and includes a "title", "formats the numeric values to 2 decimal points" 
##########and "does conditional formatting where the variable values are shaded in different tones of red with darker shades for higher values"

gt(meansbyseg,rowname_col = "rownames") %>%
  tab_header(
    title = "Means of Questions by Cluster") %>%
  fmt_number(columns = c(cluster1,cluster2,cluster3),decimals = 2) %>% 
  data_color(columns = c(cluster1,cluster2,cluster3),colors = seg_palette)


############here you can find the distribution of age, income, and gender for each cluster. We use the "table" function here because the data below are all dummy or categorical variables.
############ The table function provides a frequency plot fo the data by cluster. All the profile summaries are stored in the table "profilesbyseg"



profileseg<-t(table(segdatc$cluster,segdatc$hqAge))
profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$hqSpend)))

profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$QS2)))

profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$hqRun)))
profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$hqCycle)))
profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$hqTri)))
profileseg<-rbind(profileseg,t(table(segdatc$cluster,segdatc$QS8)))

l<-t(segdatc%>%group_by(cluster)%>%summarise(across(QS6_1:QS6_9, mean)))
profileseg<-rbind(profileseg,l[-1,])

l<-t(segdatc%>%group_by(cluster)%>%summarise(across(QS7_1:QS7_9, mean)))
profileseg<-rbind(profileseg,l[-1,])


##############attach the column names and the names of the variables from the file "rownames_wahoo" #########################
cnames<-c("cluster1","cluster2","cluster3")

colnames(profileseg)=cnames

rownames<-read.csv(file="rownames_wahoo.csv")

profileseg<-cbind(profileseg,rownames)

profileseg[1:28,] -> top
profileseg[29:nrow(profileseg),] -> bottom
top %>%
  mutate(cluster1 = round(cluster1/144, 2), cluster2 = round(cluster2/276,2), cluster3 = round(cluster3/93,2)) -> top

profileseg <- rbind(top, bottom)


##########the following lines creates a table using the function "gt" and includes a "title", "formats the numeric values to 2 decimal points" 
gt(profileseg,rowname_col = "rownames") %>%
  tab_header(
    title = "Profile by Cluster") %>%
  fmt_number(columns = c(cluster1,cluster2,cluster3),decimals = 2)

segdatc %>%
  ggplot(aes(x =AGE, fill = as.factor(Class.)))+geom_histogram()+facet_wrap(~Class.)


 # Survey Question graphs

meansbyseg %>%
  mutate(leisure = cluster1, intermediate = cluster2, competitive = cluster3) %>%
  select(leisure, intermediate, competitive, rownames) -> graph_data


graph_data %>%
  filter(rownames(.) == 'Q1_10') %>%
  select(c(-'rownames'))-> Q1_10

Q1_10 <- as.data.frame(t(Q1_10))
as.da


library(tibble)
df <- tibble::rownames_to_column(Q1_10, 'Cluster')
df %>%
 # select(leisure, intermediate, competitive) %>%
  ggplot(aes(x = Cluster, y = Q1_10))+geom_col(aes(fill = Cluster), width = 0.7)+
  scale_fill_manual(values = c('#000000',
                    '#4D4D4D',
                    '#999999'))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  coord_flip()
