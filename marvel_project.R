library(plotrix)
library(Hmisc)
library(stringr)
library(ggplot2)
library(dplyr)
library(data.table)
library(graphics)


getwd()
setw0d("d:\\fourth semester\\r programming\\r project")
getwd()
marvel<-read.csv("marvel_movies.csv")
marvel1<-read.csv("marvel_reviews.csv")


View(marvel)
View(marvel1)

is.data.frame(marvel)
is.data.frame(marvel1)

summary(marvel)
summary(marvel1)

dim(marvel)
dim(marvel1)

str(marvel)
str(marvel1)

names(marvel)
names(marvel1)

length(marvel)
length(marvel1)

head(marvel)
head(marvel1)

tail(marvel)
tail(marvel1)

marvel$Title

unique(marvel$Distributor)

ncol(marvel)
ncol(marvel1)

nrow(marvel)
nrow(marvel1)


#Number of movies produced by different distributors :
a<-aggregate(Title~Distributor,marvel,length)
a
ggplot(a,aes(x=Distributor,y=Title,fill=Distributor))+geom_bar(stat="identity")


#Names and budgets of movies having more than 2.00e+08 budget :
a1<-subset(marvel,Budget>2.00e+08,select = c(Title,Budget))
a1
ggplot(a1,aes(x=Title,y=Budget,fill=Title))+geom_bar(stat="identity")


#Movies according to their CinemaScore :
a2<-aggregate(Film~CinemaScore,marvel1,length)
a2
ggplot(a2,aes(x=CinemaScore,y=Film,fill=CinemaScore))+geom_bar(stat="identity")


#Movies according to their CinemaScore.1 :
a3<-aggregate(Film~CinemaScore.1,marvel1,length)
a3
ggplot(a3,aes(x="",y=CinemaScore.1,fill=CinemaScore.1))+geom_bar(stat="identity")+coord_polar("y")


#Movies where gross revenue > 500000000 in North America :
a4<-subset(marvel,NorthAmerica>300000000,select = c(Title,NorthAmerica))
a4
ggplot(a4,aes(x="",y=Title,fill=Title))+geom_bar(stat="identity")+coord_polar("y")


#Movies which are produced by "20th Century Fox" :
a5<-marvel[which(marvel$Distributor=="20th Century Fox"),"Title"]
a5


#What is the distribution of budget for movies :
ggplot(data=marvel, aes(x=Budget)) +
  geom_density(color="black") +
  geom_vline(xintercept = mean(marvel$Budget), color = "green", linetype = "dashed") +
  labs(title="Distribution of Budget for movies") +
  xlab(label="Budget") +
  ylab(label="Density")



#Relationship of Budget of the movie and its Gross Revenue(Worldwide) :
ggplot(marvel, aes(x=Budget, y=Worldwide)) +
  geom_point() +
  geom_smooth(aes(colour="Loess"), method="loess",size=.6, se=F) +
  geom_smooth(aes(colour="Linear Regression"), method="lm", se=F, size=.6) +
  xlab(label="Budget") +
  ylab(label="Gross Revenue(Worldwide)") +
  labs(title="Gross Revenue vs Budget") 


#Which movie has topped the revenue charts?
marvel$Title[marvel$Worldwide==max(marvel$Worldwide)]


#Which distributor has produced the most marvel movies?
table(marvel$Distributor)[table(marvel$Distributor)==max(table(marvel$Distributor))]


#Relationship between Rotten Tomatoes rating and the Metacritic rating of the movies :
marvel1[,c("Film","Rotten.tomatoesin.","Metacritic")]
ggplot(marvel1, aes(x=Metacritic, Rotten.Tomatoesin.)) +
  geom_point( size=.5) +
  geom_text_repel(label=marvel1$Film, size=3) +
  xlim(50,100) +
  ylim(50,100) +
  geom_line(colour="darkviolet", alpha=.6, method="loess", stat="smooth", size=1.3) +
  labs(title="Metacritic ratings vs Rotten Tomatoes ratings") +
  ylab(label="Rotten Tomatoes rating") +
  xlab(label="Metacriti rating")
ggplot(marvel1) +
  geom_bar(aes(x=reorder(Film, Rotten.Tomatoesin.), y=Rotten.Tomatoesin., fill="Rotten Tomatoes"), stat="identity", alpha=1) +
  geom_bar(aes(x=reorder(Film, Metacritic), y=Metacritic, fill="Metacritic"), stat="identity",alpha=1) +
  coord_flip() +
  scale_fill_discrete("") +
  labs(title="Ratings of movies") +
  ylab("Rating") +
  xlab("Movies")