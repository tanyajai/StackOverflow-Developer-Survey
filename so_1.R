####################################################################################
#pacakges

####################################################################################
#graph
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
library(GGally)
#install.packages("lattice")
library(lattice)

#map
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
#install.packages("ggmap")
library(ggmap)

#install.packages("sf")
library(sf)
#install.packages("leaps")
library(leaps)#subset
#install.packages("pracma")
library(pracma)#is.na
#install.packages("readr")
library(readr)#import description
#install.packages("splitstackshape")
library(splitstackshape)#cSplit
#install.packages("rgeos")
library(rgeos)

####################################################################################
#import datasets
schema <- read.csv("developer_survey_2019/survey_results_schema.csv", row.names=1)
#View(schema)
backup_schema <- schema

so_survey_result <- read_csv("developer_survey_2019/survey_results_public.csv")
#View(so_survey_result)
backup_so_survey_result <- so_survey_result

####################################################################################
#names of column of survey
names(so_survey_result)

####################################################################################
#checking NA values
len_survey <- length(so_survey_result)
na_col <- data.frame(col_name=character(),na_count=integer())

for (i in 1:len_survey){
  na_count <- sum(is.na(so_survey_result[i]))
  if (na_count > 0){
    #print(paste(names(so_survey_result[i])," have ",na_count," missing values"))
    col_na_name <- names(so_survey_result[i])
    temp_list <- list(col_name=col_na_name,na_count=na_count)
    na_col <- rbind(na_col,temp_list)
    na_col$col_name <- as.character(na_col$col_name)
  }
}

#so_survey_result[is.na(so_survey_result)] <- "Not Responded"

####################################################################################
#elminating columns
eliminated_variables<-c(11,13,16,18,20,21,24,26,28,30,31,34,35,40,42,43,64:77,80:82,84,85)
so_survey_result<-so_survey_result[-eliminated_variables]
names(so_survey_result)
#View(so_survey_result)

####################################################################################
#getting values that are sepreated by ; in a column

graph <- paste0("Language_", c(28:38), ".png", sep="")
compare <- paste0("current_desired", c(1:10), ".png", sep="")

for (ind in 28:38){
  col_name <- names(so_survey_result[ind])
  main_list <- c()
  main_list <- cSplit(so_survey_result,col_name,sep = ";",direction = "long")
  main_list <- main_list[[col_name]]
  frequency_list <- c()
  frequency_list <- as.data.frame(table(main_list))
  colnames(frequency_list)[1] <- "Languages" #have to make dynamic
  colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic
  
  # Draw plot
  
  #Language Vs Number of users
  g <- ggplot(data=frequency_list, aes(x=Languages , y=Number_of_Users)) + 
    geom_bar(stat="identity",width=.5, fill="blue") + 
    labs(title="Language Vs Number of users",
         caption="ProjectEAS506:SO") + ylab(col_name) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6))

  ggsave(filename = graph[(ind-27)], plot = g, height = 5, width = 5)
  
  if(ind%%2==0){
    frequency_list_1 <- c()
    frequency_list_1 <- frequency_list
    frequency_list_1[3] <- 'Current'
  }else{
    frequency_list_2 <- c()
    frequency_list_2 <- frequency_list
    frequency_list_2[3] <- 'Desired'
    frequency_list <- rbind(frequency_list_1,frequency_list_2)
    g <- ggplot(data = frequency_list, aes(x = Languages , y = Number_of_Users ,fill = V3)) + 
      geom_bar(stat="identity") +
      labs(title="languages now and desired",caption="ProjectEAS506:SO") + 
      theme(axis.text.x = element_text(angle=90, vjust=0.4))
    
    ggsave(filename = compare[(ind-27)], plot = g, height = 5, width = 5)
  }
}


####################################################################################
# Draw plot

#Main Branch
frequency_list <- as.data.frame(table(so_survey_result[,2]))
colnames(frequency_list)[1] <- "Role" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic

#Role Vs Number of users
g <- ggplot(data=frequency_list, aes(x=Role , y=Number_of_Users)) + 
  geom_bar(stat="identity",width=0.2, fill="DarkBlue") + 
        labs(title="Role Vs Number of users",
        caption="ProjectEAS506:SO") +
        theme(axis.text.x = element_text(angle=45, vjust=0.6))

ggsave(filename = 'role.png', plot = g, height = 5, width = 5)

####################################################################################
# Draw plot

#Hobby
frequency_list <- as.data.frame(table(so_survey_result[,3]))
colnames(frequency_list)[1] <- "Hobbyist" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic

#taking percentage
frequency_list[1,2] = ((frequency_list)[1,2]/((frequency_list)[1,2] + (frequency_list)[2,2]))*100
frequency_list[2,2] = 100 - (frequency_list)[1,2]

#Hobby Vs Number of users
g <- ggplot(data=frequency_list, aes(x=Hobbyist , y=Number_of_Users)) + 
  geom_bar(stat="identity",width=.1, fill="magenta") + 
  labs(title="Hobbyist Vs Number of users by percentage",
       caption="ProjectEAS506:SO") + ylab("Number of users by %") +
  theme(axis.text.x = element_text(angle=45, vjust=0.6))

ggsave(filename = 'hobby.png', plot = g, height = 5, width = 5)

####################################################################################
# Draw plot

#world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#Country
frequency_list <- as.data.frame(table(so_survey_result['Country']))
colnames(frequency_list)[1] <- "Country" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic
frequency_list <- as.data.frame(frequency_list)

#making list of number of programmer
programer_list <- c()

#inserting values in programer_list
for (country_ind in 1:length(world$name)){
  if(world$name[country_ind] %in% frequency_list[,1]){
    programer_list <- append(programer_list,
                        frequency_list[which(world$name[country_ind] == frequency_list[,1]),2]) 
  }
  else{
    programer_list <- append(programer_list,0)
  }
}

#plotting map
g <- ggplot(data = world) +
  geom_sf(aes(fill = programer_list)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggsave(filename = 'programmer.png', plot = g, height = 5, width = 5)

####################################################################################
# Draw plot

#EdLevel
frequency_list <- as.data.frame(table(so_survey_result['EdLevel']))
colnames(frequency_list)[1] <- "EdLevel" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic

#Language Vs Number of users
g <- ggplot(data=frequency_list, aes(x=EdLevel , y=Number_of_Users)) + 
  geom_bar(stat="identity",width=.5, fill="blue") + 
  labs(title="Language Vs Number of users",
       caption="ProjectEAS506:SO") + 
  theme(axis.text.x = element_text(angle=30, vjust=0.6))

ggsave(filename = 'EdLevel.png', plot = g, height = 5, width = 5)
####################################################################################
# Draw plot

#OrgSize
frequency_list <- as.data.frame(table(so_survey_result['OrgSize']))
colnames(frequency_list)[1] <- "OrgSize" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic

#OrgSize Vs Number of users
g <- ggplot(data=frequency_list, aes(x=OrgSize , y=Number_of_Users)) + 
  geom_bar(stat="identity",width=.5, fill="blue") + 
  labs(title="OrgSize Vs Number of users",
       caption="ProjectEAS506:SO") + 
  theme(axis.text.x = element_text(angle=30, vjust=0.6))

ggsave(filename = 'OrgSize.png', plot = g, height = 5, width = 5)

####################################################################################
# Draw plot

frequency_list <- as.data.frame(table(so_survey_result[
                                  (which(so_survey_result['Country']=='United States')),'JobSeek']))
colnames(frequency_list)[1] <- "JobSeek" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic
win.graph()
#Language Vs Number of users
ggplot(data=frequency_list, aes(x=JobSeek , y=Number_of_Users)) + 
  geom_bar(stat="identity",width=.5, fill="brown") + 
  labs(title="Job Seek Vs Number of users",
       caption="ProjectEAS506:SO") + ylab("Number of Users in United States")
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

#ggsave(filename = 'JobSeek.png', plot = g, height = 5, width = 5)

####################################################################################
# Draw plot

#Country
frequency_list <- as.data.frame(table(so_survey_result['Country']))
frequency_list[3] <- 0
colnames(frequency_list)[1] <- "Country" #have to make dynamic
colnames(frequency_list)[2] <- "Number_of_Users" #have to make dynamic
colnames(frequency_list)[3] <- "Salary" #have to make dynamic
frequency_list <- as.data.frame(frequency_list)

for (ind in 1:nrow(so_survey_result)){
  country_ind = which(so_survey_result$Country[ind] == frequency_list[,1])
  if(!is.na(so_survey_result$ConvertedComp[ind])){
    frequency_list[country_ind,3] = frequency_list[country_ind,3] + 
                                        so_survey_result$ConvertedComp[ind]
  }
  else{
    frequency_list[country_ind,2] = frequency_list[country_ind,2] - 1
  }
}

frequency_list$Salary = frequency_list$Salary/frequency_list$Number_of_Users

frequency_list <- frequency_list[complete.cases(frequency_list),]

frequency_list <- frequency_list[-which(frequency_list$Salary < 100000),]

#ConvertedComp Vs Country
g <- ggplot(data=frequency_list, aes(x=Country , y=Salary)) +
  geom_point() +
  labs(title="Country Vs Salary",
       caption="ProjectEAS506:SO") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.4))

ggsave(filename = 'Country_Salary.png', plot = g, height = 5, width = 5)

#######################################################################################
#################################################################################

employment_list<-as.data.frame(table(so_survey_result$Employment))
country_list<-as.data.frame(table(so_survey_result$Country))
listt<-c("Employed full-time","Employed part-time","Independent contractor", "freelancer, or self-employed","Not employed, and not looking for work","Not employed, but looking for work","Retired")
for(i in range(1:6))
{
  frequency_list<-as.data.frame(table(so_survey_result[(which(so_survey_result['Employment']=="Retired")),'Country']))                                                                                                                   
  colnames(frequency_list)[1] <- "Country_list" 
  colnames(frequency_list)[2] <- "Employment_Type" 
  frequency_list
  win.graph()
  plot(frequency_list$Country_list,frequency_list$Employment_Type, type="l", col="green", lwd=5, pch=15, xlab="Country", ylab="Number of Users Retired")
  lines(frequency_list$Country_list, frequency_list$Employment_Type, type="l", col="yellow", lwd=2, pch=19)
  title("Country vs Employment Type")
  legend(0,2.8,c("decay","growth"), lwd=c(5,2), col=c("green","red"), pch=c(15,19), y.intersp=1.5)
  
}
which.max(frequency_list$Var1)

####################################################################################
####################################################################################
### Word Cloud
####################################################################################
win.graph()
pal <-brewer.pal(10,"Dark2")
so_survey_result %>%
  anti_join(country_list) %>%
  count(country_list$Var1)
with(wordcloud(country_list$Var1,country_list$Freq,max.words = 100,colors = pal))

#######################################################################################
so_survey_result$LanguageWorkedWith
database_worked<-cSplit(so_survey_result,"LanguageWorkedWith",sep = ";",direction = "long")
database_worked <- database_worked[["LanguageWorkedWith"]]
database_list<-as.data.frame(table(database_worked))
win.graph()
so_survey_result %>%
  anti_join(database_list) %>%
  count(database_list$database_worked)
with(wordcloud(database_list$database_worked,database_list$Freq,colors = pal))

##########################################################################################
database_future<-cSplit(so_survey_result,"DatabaseDesireNextYear",sep = ";",direction = "long")
database_future <- database_future[["DatabaseDesireNextYear"]]
database_list2<-as.data.frame(table(database_future))
so_survey_result %>%
  anti_join(database_list2) %>%
  count(database_list2$database_future)
with(wordcloud(database_list2$database_future,database_list2$Freq))
#############################################################################################
#################################################################################
#############Bar PLots and Stacked Bar Plots
##############################################################################
graph <- paste0("Language_", c(29:39), ".png", sep="")
compare <- paste0("current_desired", c(1:11), ".png", sep="")

color <- c("blue","green","red","cyan","yellow","magenta","brown","violet","pink","purple","orange","black")
col_name_list<-c("Languages"," ","DataBase"," ","Platforms"," ","WebFramework"," ","Miscellaneous Technologies")
for (ind in 29:39){
  col_name <- names(so_survey_result[ind])
  main_list <- c()
  main_list <- cSplit(so_survey_result,col_name,sep = ";",direction = "long")
  main_list <- main_list[[col_name]]
  frequency_list <- c()
  frequency_list <- as.data.frame(table(main_list))
  colnames(frequency_list)[1] <- "Languages" 
  colnames(frequency_list)[2] <- "Number_of_Users" 
  
  # Draw plot
  
  #Language Vs Number of users
  g <- ggplot(data=frequency_list, aes(x=Languages , y=Number_of_Users)) + 
    geom_bar(stat="identity",width=.5, fill=color[ind-28]) + 
    labs(title=paste(col_name," Vs Number of Users", sep=""),
         caption="ProjectEAS506:SO") + xlab(col_name) + ylab("Number of Users") +
    theme(axis.text.x = element_text(angle=90, vjust=0.6))
  
  ggsave(filename = graph[(ind-27)], plot = g, height = 5, width = 5)
  
  if(ind%%2==1){
    frequency_list_1 <- c()
    frequency_list_1 <- frequency_list
    frequency_list_1[3] <- 'Current'
  }else{
    frequency_list_2 <- c()
    frequency_list_2 <- frequency_list
    frequency_list_2[3] <- 'Desired'
    frequency_list <- rbind(frequency_list_1,frequency_list_2)
    g <- ggplot(data = frequency_list, aes(x = Languages , y = Number_of_Users ,fill = V3)) + 
      geom_bar(stat="identity") + ylab("Number of Users") + xlab(paste(col_name_list[ind-29]," Now and Desired",sep =""))+
      labs(title=paste(col_name_list[ind-29],"Vs Number of Users",sep = ""),caption="ProjectEAS506:SO") + 
      theme(axis.text.x = element_text(angle=90, vjust=0.4))
    
    ggsave(filename = compare[(ind-28)], plot = g, height = 5, width = 5)
  }
}

##########################################################################
career <- as.data.frame(table(so_survey_result$CareerSat))
car1<-career[c(1,3,5),]
car1[3]<-1
car2<-career[c(2,4),]
car2[3]<-0
career_sat<-as.data.frame(rbind(car1,car2))

ggplot(data=career_sat,aes(x=V3,y=Freq,fill=Var1))+geom_bar(stat="identity")+
  labs(x = "Career Satisfaction Group", title = "Career Satisfaction") + 
  scale_x_discrete(labels = c("Positive", "Negative"))
##########################################################################
cjs <- so_survey_result[c("YearsCode", "YearsCodePro", "CareerSat")]
cjs <- cjs[complete.cases(cjs), ]
cjs$YearsCode <- as.character(cjs$YearsCode)
cjs$YearsCodePro <- as.character(cjs$YearsCodePro)
cjs$YearsCode[cjs$YearsCode == "Less than 1 year"] <- 1
cjs$YearsCodePro[cjs$YearsCodePro == "Less than 1 year"] <- 1
cjs$YearsCode[cjs$YearsCode == "More than 50 years"] <- 50
cjs$YearsCodePro[cjs$YearsCodePro == "More than 50 years"] <- 50
cjs$YearsCode <- as.integer(cjs$YearsCode)
cjs$YearsCodePro <- as.integer(cjs$YearsCodePro)
cjs$CareerSat <- as.factor(cjs$CareerSat)
#cjs$CareerSat <- factor(cjs$CareerSat("Very dissatisfied", 
#"Slightly dissatisfied", "Neutral", "Slightly satisfied", "Very satisfied"),
#levels = cjs$CareerSat("Very dissatisfied", 
#"Slightly dissatisfied", "Neutral", "Slightly satisfied", "Very satisfied"))

cjs$YearsCode = ifelse(cjs$YearsCode < cjs$YearsCodePro, cjs$YearsCode+cjs$YearsCodePro, cjs$YearsCode)

ggplot()+geom_jitter(data=cjs, aes(x=CareerSat, y= YearsCode),alpha=0.5,color="blue")+
  geom_jitter(data=cjs,aes(x=CareerSat, y=YearsCodePro),alpha=0.5,color="red")+
  labs(y = "YearsCode", x = "CareerSat", caption="ProjectEAS506:SO")+
  theme(axis.text.x = element_text(angle=90, vjust=0.4))

####################################################################################
#checking NA values
names(so_survey_result)
len_so <- length(so_survey_result)
na_col=c()
for(i in 1:len_so)
  
{
  na_count<-sum(is.na(so_survey_result[i,]))
  if(na_count>0){
    na_col<-so_survey_result[i]
  }
}
