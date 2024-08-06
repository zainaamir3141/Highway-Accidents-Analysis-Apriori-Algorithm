library(ggplot2)
library(RColorBrewer)
getwd()
setwd("C:/BA")
HA <- read.csv("Highway accidents.csv", na.strings=c(" ","NA","N/A","Not Applicable"))
any(is.na(HA)) #any missing values? comes "TRUE"
colSums(is.na(HA)) #in what columns are the missing values in
HA2 <- subset(HA, Year == 2016 | Year == 2017) #creates a subset of 2016 & 2017
colnames(HA2)[1] <- "Event.ID"

tempHA <- HA2

prop.table (tempHA$Expected.Lost.Workdays..Total.)
prop.table(tempHA$Expected.Restricted.Workdays..Total.)
prop.table(tempHA$Actual.Lost.Workdays..Total.) 
prop.table(tempHA$Actual.Restricted.Workdays..Total.) 
#this shows that most data consist of zeros so this data is of no use to us except actual work days lost as we could categorize it to form useful analysis

#removing unimportant variables - 
tempHA <- tempHA[,-c(4,5,9,11,12,13,15,16,18,19,20,21,23,24,27,31,33,34,35,36)]
tempHA2 <- tempHA #for visualizations
tempHA2 [tempHA2 == ""] <- NA
tempHA4 <- tempHA #for clustering 


tempHA2 <- tempHA2[order(tempHA2$Event.ID),] #sorts data in terms of event ID

#to know the probability of accidents on a weekday and weekend 
tempHA2$Day.of.the.Week <- ifelse(tempHA2$Day.of.the.Week == "Mon" | tempHA2$Day.of.the.Week == "Tue" | tempHA2$Day.of.the.Week == "Wed" | tempHA2$Day.of.the.Week == "Thu" | tempHA2$Day.of.the.Week == "Fri", "Weekday", "Weekend")
tempHA2$Was.there.a.vehicle.involved.in.this.event. <- ifelse( tempHA2$Was.there.a.vehicle.involved.in.this.event. == "Yes", "Vehicle Involved", "Vehicle Not Involved")
tempHA2$Preventable <- ifelse(tempHA2$Preventable == "No", "Not Preventable", ifelse(tempHA2$Preventable == "Yes", "Preventable", "N/A"))
tempHA2$Was.A.Police.Report.Filed. <- ifelse(tempHA2$Was.A.Police.Report.Filed. == "Yes", "Filed", "Not Filed")
# for lighting
tempHA2$Lighting.Conditions <- ifelse(tempHA2$Lighting.Conditions == "100%", "Morning", ifelse(tempHA2$Lighting.Conditions == "Poor", "Evening", ifelse(tempHA2$Lighting.Conditions == "Non", "Night", "N/A")))

#changes lost days to numeric and then defines it so we can see what kind of accidents result in how much of a r
tempHA2$Actual.Lost.Workdays..Total. <- as.numeric(tempHA2$Actual.Lost.Workdays..Total.)
tempHA2$Actual.Lost.Workdays..Total. <- ordered(cut(tempHA2$Actual.Lost.Workdays..Total.,  c(0,4,8,70)), labels = c("0-4 Days", "4-8 Days", "Greater than 8 Days"))

tempHA$Was.A.Police.Report.Filed. <- as.factor(tempHA2$Was.A.Police.Report.Filed.)
tempHA2$Day.of.the.Week <- as.factor(tempHA2$Day.of.the.Week)
tempHA2$Event.Type <-as.factor(tempHA2$Event.Type)
tempHA2$Type <- as.factor(tempHA2$Type)
tempHA2$Was.there.a.vehicle.involved.in.this.event. <- as.factor(tempHA2$Was.there.a.vehicle.involved.in.this.event.)

tempHA3 <- tempHA2
tempHA3 <- tempHA3[,-c(1,6,8)] #for arules 
View(tempHA3)

#PART 3 
viz <- tempHA2
library(ggplot2)
#install.packages("RcolorBrewer")
library(RColorBrewer)
library(dplyr)
display.brewer.all(5)

viz$Year <- as.factor(viz$Year)

#to see count of accidents in 2016 and 2017 
ggplot(viz,aes(x= Year)) + geom_bar(fill = "dark blue") +theme_minimal() + 
  labs(title = "                                     Number of events in each year", x = "Year", y = "Count of accidents")


#comparison of number of distinct event types accross two years
ggplot(viz, aes(x= Event.Type)) + facet_grid(Year~ .) + geom_bar(fill = "dark blue") + coord_flip() +
  theme_minimal() + labs(title = "Comparison of Event types across 2 years", x="Event Type", y="Count of Accidents") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))  


#to see whether most accidents occur during weekday/weekend and during which time interval
ggplot(viz, aes(x= Day.of.the.Week)) + facet_grid(~Time.interval..6.hours.)+
  geom_bar(aes(fill= Event.Type)) +scale_fill_brewer(palette = "Dark2")+ labs(title="Distrubution of Events by Day of week and Time Interval", x="Day of the Week", y = "Count") + theme_bw()

#relationship between weather conditions and event type
viz2 <- subset(viz, Weather.Visibility == "Clear" |Weather.Visibility == "Dark" |Weather.Visibility == "Raining"|Weather.Visibility == "Storm"|Weather.Visibility == "Windy")
ggplot(viz2, aes(x= Weather.Visibility, fill=Event.Type)) + geom_bar()+
  labs(title="Association between Event Type and Weather across 2 years", x="Weather Conditions", y="Count") + theme_light() + facet_wrap(~Year)


#in how many accidents was a vehicle involved, a report filed
viz3 <- subset(viz, viz$Was.A.Police.Report.Filed. == "Filed" |viz$Was.A.Police.Report.Filed. == "Not Filed")
ggplot(viz3, aes(x=viz3$Event.Type, fill=viz3$Was.A.Police.Report.Filed.)) + facet_grid(viz3$Year~ viz3$Was.there.a.vehicle.involved.in.this.event.) +
  geom_bar() + theme_bw() + coord_flip() + labs(title= "Vehicle invovlement and Report", x = "Event Type")+theme(axis.text.x = element_text(angle=65, vjust=0.6))

#if a person was seriously injured, was a police report filed?
#knowing this would help understand the frequency of certain accidents 

ggplot(viz3,aes(x=Was.A.Police.Report.Filed.,y= Event.Subtype)) + geom_count()+ labs(x="Filing of Report", y="Subtype of Event") +
  theme_bw() 

#Distribution of Event subtypes  
A <- ggplot(viz, aes(x=viz$Year, y=Event.Subtype))+ labs(x="Year", y="Subtype of Events") +
  geom_count()
A

#Monthly distribution
B <- ggplot(viz, aes(x=factor(1), fill=Month)) +
  geom_bar(width = 1)+ facet_wrap(~viz$Year) +
  coord_polar("y") + scale_fill_brewer(palette = "Set3") + labs(x="",y="")
B

#from abov viz we see that majority of accidents are occuring in august-december, lets see if it has something to do with hazy weather during these months
ggplot(viz2, aes(x=Weather.Visibility, y=Month)) + geom_count() + facet_wrap(~viz2$Year) + labs(x="Weather")


#Part 4 - clustering
library(Rtsne)
library(cluster)
library(ggplot2)
library(dplyr)

#Clustering

#distancematrix
dis<-daisy(tempHA4[,-1],metric = "gower")
summary(dis)

dismat <- as.matrix(dis)



#Optimal num of clusters - using silhouette width and pam, k-means not working

silhou_wid <- c(NA)
for(i in 2:8){
  pam1 <- pam(dis,
              diss = TRUE,
              k = i)
  silhou_wid[i] <- pam1$silinfo$avg.width
}
plot(1:8, silhou_wid,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, silhou_wid)


#Using pam

pam1 <- pam(dis, diss = TRUE, k = 3)

tsne1 <- Rtsne(dis, is_distance = TRUE)
tsneD <- tsne1$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam1$clustering),
         name = tempHA4$Event.ID)
ggplot(aes(x = X, y = Y), data = tsneD) +
  geom_point(aes(color = cluster))


#Using k-means

set.seed(123)
KClusters<-kmeans(dismat, centers=3)
table(KClusters$cluster)


tsne2 <- Rtsne(dis, is_distance = TRUE)
tsneD2 <- tsne2$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(KClusters$cluster),
         name = tempHA4$Event.ID)

ggplot(aes(x = X, y = Y), data = tsneD2) +
  geom_point(aes(color = cluster))


#Clusters explained Via descriptives - kmeans results as it showed better clusters
pamR <- tempHA2 %>%
  dplyr::select(-Event.ID) %>%
  mutate(cluster = pam1$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pamR$the_summary


#PART 4: Arules

library(arules)
library(arulesViz)
library(htmlwidgets)



write.csv(tempHA3, file = "Accidentsrules.csv") #makes a new file and write previous cleaned data into it
Highway_Rules <-read.transactions("Accidentsrules.csv", sep = ",") #converts it into a sparse matrix 
class(Highway_Rules)

itemFrequencyPlot(Highway_Rules,support = 0.2,  col = brewer.pal(10,'Dark2'), main = "Item Frequency Plot") #20% or more support of which variables


rules <- apriori(Highway_Rules)
plot(rules, col = brewer.pal(10,'RdBu')) #tells us what support and confidence level to select so we choose 0.2 for support and 0.8 for confidence

rules <- apriori(Highway_Rules, parameter = list (support = 0.2, conf = 0.8)) 
rules


#instead of making subsets, we've targetted rules
#vehicle involvement:
Vrule1 <- apriori(Highway_Rules, parameter = list(support=0.05,conf=0.8), appearance = list (rhs = "Vehicle Not Involved", default = "lhs"))
Vrule1 <- sort (Vrule1, by = "lift")
inspect(Vrule1[1:10])

Vrule2 <- apriori(Highway_Rules, parameter = list(support=0.2,conf=0.8), appearance = list (rhs = "Vehicle Involved", default = "lhs"))
Vrule2 <- sort (Vrule2, by = "lift")
inspect(Vrule2[1:10])

#Weekday or Weekend 

Weekday <- apriori(Highway_Rules, parameter = list(support=0.1,conf=0.8), appearance = list (rhs = "Weekday", default = "lhs"))
Weekday <- sort (Weekday, by = "lift")
inspect(Weekday[1:10])
#challenge: out of bounds so difficult almost no rules of weekend so needed to use a lower support
Weekend <- apriori(Highway_Rules, parameter = list(support=0.001,conf=0.8), appearance = list (rhs = "Weekend", default = "lhs"))
Weekend <- sort (Weekend, by = "lift")
inspect(Weekend[1:10])

#morning and night
Lightingrule_1 <- apriori(Highway_Rules, parameter = list(support=0.1,conf=0.5), appearance = list (rhs = "Morning", default = "lhs"))
Lightingrule_1 <- sort (Lightingrule_1, by = "lift")
inspect(Lightingrule_1[1:10])
#issue that lower support gives weird output
Lightingrule_2 <- apriori(Highway_Rules, parameter = list(support=0.01,conf=0.8), appearance = list (rhs = "Night", default = "lhs"))
Lightingrule_2 <- sort (Lightingrule_2, by = "lift")
inspect(Lightingrule_2[1:10])

#event rules
EventRule1 <- apriori(Highway_Rules, parameter = list(support=0.2,conf=0.8, minlen = 2), appearance = list (rhs = "Construction & Maintenance", default = "lhs"))
EventRule1 <- sort (EventRule1, by = "lift")
inspect(EventRule1[1:10])

EventRule2 <- apriori(Highway_Rules, parameter = list(support=0.2,conf=0.8, minlen = 2), appearance = list (rhs = "Minor or low potential impact", default = "lhs"))
EventRule2 <- sort (EventRule2, by = "lift")
inspect(EventRule2[1:10])

EventRule3 <- apriori(Highway_Rules, parameter = list(support=0.2,conf=0.8, minlen = 2), appearance = list (rhs = "Incursion due to breakdown", default = "lhs"))
EventRule3 <- sort (EventRule3, by = "lift")
inspect(EventRule3[1:10])

EventRule4 <- apriori(Highway_Rules, parameter = list(support=0.2,conf=0.8, minlen= 2), appearance = list (rhs = "Near Miss", default = "lhs"))
EventRule4 <- sort (EventRule4, by = "lift")
inspect(EventRule4[1:10])

num <- length(which(tempHA3$Actual.Lost.Workdays..Total. == "0-4 Days"))
num #shows that there were 10 entries as such (all the other slots had less than 10 entries so we choose this)
#how many days lost as our data had the highest frequency of 0-4 days
LostDayRule <- apriori(Highway_Rules, parameter = list(support=0.001,conf=0.8), appearance = list (rhs = "0-4 Days", default = "lhs"))
LostDayRule <- sort (LostDayRule, by = "lift")
inspect(LostDayRule[1:10])

library(htmlwidgets)
library(arulesViz)

plot(Vrule1[1:10], method = "graph", engine = "htmlwidget") 
plot(Vrule2[1:10], method="graph", engine = "htmlwidget")
plot(Weekday[1:10], method="graph", engine = "htmlwidget")
plot(Weekend[1:10], method="graph", engine = "htmlwidget")
plot(Lightingrule_1[1:10], method = "graph", engine = "htmlwidget")
plot(Lightingrule_2[1:10], method="graph", engine = "htmlwidget")
plot(LostDayRule[1:10], method="graph", engine = "htmlwidget")

plot(EventRule1[1:10], method = "graph", engine = "htmlwidget") 
plot(EventRule2[1:10], method = "graph", engine = "htmlwidget") 
plot(EventRule3[1:10], method = "graph", engine = "htmlwidget") 
plot(EventRule4[1:10], method = "graph", engine = "htmlwidget")


