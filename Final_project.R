
install.packages("readr")
library(readr)

data <-read_csv("H:\\Tanjim Aiub\\Pdf\\semester 10\\Data Science\\Final Term\\Mall_Customers.csv")
data

dim(data) #dimension

str(data) #structure

sum(duplicated(data)) #duplicate_check

sum(is.na(data)) #NA_check

data$Gender <-as.factor(data$Gender) #vectorobject_to_factor

table(data$Gender) #shows_in_tabularform

gender <- table(data$Gender)
pie_chart <- round(gender/sum(gender)*100)
lbs <- paste(c("Female","Male")," ",pie_chart,"%",sep = " ")

install.packages("plotrix")
library(plotrix)
pie3D(gender,labels = lbs, main="Ratio of Female and Male", col = c("#268e8c","#7b3046"))

install.packages("magrittr")
library(magrittr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
data %>%
  group_by(Gender) %>%
  summarise(mean_spending = mean(`Spending Score (1-100)`)) %>%
  ggplot(aes(Gender, mean_spending, fill = Gender)) + geom_col() +
  ylab("Mean Spending Score") + theme_minimal() +
  theme(legend.position = "none", text = element_text(size=13 , color = "black")) +
  labs(tittle = "Average Spending Score by Gender") +
  xlab("") +
  scale_fill_manual(values = c("#268e8c","#7b3046"))

summary(data$Age)

data %>%
  ggplot(aes(x="", y=Age))+
  geom_boxplot(fill = "#7b3046")+
  xlab("")+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), text=element_text(size = 13, color="black"))+
  ylab("age")+
  labs(tittle="Description Analysis of Age")

data$Age <- cut(data$Age,
                breaks = c(15,25,35,45,55,65,100),
                labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                right = F,
                ordered_result = T)
table(data$Age)

data %>%
  group_by(Age) %>% 
  mutate(mean_spending = mean(`Spending Score (1-100)`)) %>% 
  ggplot(aes(Age, mean_spending, fill = Gender)) +
  geom_col() +
  ylab("Mean Spending Score") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 13, color = "black")) +
  labs(title = "Average Spending Score by Age")+
  xlab("Age") +
  scale_fill_manual(values = c("blue", "red"))

summary(data$`Annual Income (k$)`)

ggplot(data, aes(`Annual Income (k$)`)) +
  geom_density(fill="red", alpha = 0.1) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 13, color = "black")) +
  labs(title = "Distribution of Annual Income")+
  ylab("") +
  scale_x_continuous(breaks = seq(0,150, by = 30))

data %>% 
  ggplot(aes(`Annual Income (k$)`, `Spending Score (1-100)`)) +
  geom_point() +
  theme_minimal() +
  theme(text = element_text(size = 13, color = "black")) +
  labs(title = "Relation between Spending Score and Annual Income")+
  ylab("Spending Score")

summary(data$`Spending Score (1-100)`)

data %>% 
  ggplot(aes(x = "", y = `Spending Score (1-100)`)) +
  geom_boxplot(fill = "red") +
  xlab("") +
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 13, color = "black"))+
  ylab("Spending Score")+
  labs(title = "Descriptive Analysis of Spending Score")

ggplot(data, aes(`Spending Score (1-100)`)) +
  geom_density(fill="red", alpha = 0.1) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 13, color = "black")) +
  labs(title = "Distribution of Spending Score")+
  ylab("") +
  scale_x_continuous(breaks = seq(0,100, by = 20))

data_1 <- data[,-c(1,2,3)]
head(data_1)

data_1 <-scale(data_1)
data_1

wss <- vector("double", 10) #within cluster sum of square
for(i in 1:10){
  wss[i] <- sum(kmeans(data_1, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "within groups sum of squares")

set.seed(123)

km.out <- kmeans(data_1, centers = 5, nstart = 20)

km.out

ggplot(data, aes(x = `Annual Income (k$)` , y = `Spending Score (1-100)`))+
  geom_point(stat = "Identity", aes(color=as.factor(km.out$cluster)))+
  scale_color_discrete(name=" ", breaks=c("1","2","3","4","5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))+
    ggtitle("Mall Customer Segment",subtitle = "K-Means Clustering")

