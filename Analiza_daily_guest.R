library(dplyr)
library(tidyr)
library(tibble)
library(fivethirtyeight)

guests<-daily_show_guests
guests

#zamienianie na dwie kolumny
guests <- separate(guests, col = c("raw_guest_list"), into = c("imie", "nazwisko"), sep = " ", extra = "merge")

#zmiana na 3 kolumny - rozdzielenie daty
guests<-separate(guests, col = c("show"), into = c("year", "month", "day"), sep = "-", extra = "merge")
#zeby pozbyc sie jednego levela
guests$group[guests$group == "media"] <- "Media"
guests$year <-as.numeric(guests$year)
guests$month <-as.numeric(guests$month)
guests$day <-as.numeric(guests$day)
guests$group <-as.factor(guests$group)

#typeof(guests)
#guests
#write.csv(guests, file ="./guests.csv")
#write.csv(daily_show_guests, file ="./notProcessed.csv")

###########################################
#Wyznaczenie najbardziej popularnych osob/zespolow

#Policzenie ilosci wystapien poszczegolnych osob/zespolow i zmiana nazwy kolumny
namesOccurence <- daily_show_guests %>% group_by(raw_guest_list) %>% tally() %>% rename(Number_of_Occurences = n)
#Usuniecie dwoch pustych rekordow, chociaz to w sumie bez znaczenia dla tych danych
namesOccurence <- namesOccurence[3:nrow(namesOccurence),]
namesOccurence

#Wybierze 20 najbardziej znanych. W przypadku kilku os�b o takim samym wyniku zostana ne dodatkowo dobrane - w przypadku top_n(20,) wyswietli 24 osoby
mostFamous <- namesOccurence %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))

#mostFamous %>% print(n=Inf)
###########################################

#DODANIE BAR PLOTU POZIOMEGO
namesOccurence %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% 
ggplot(aes(x=raw_guest_list, y=Number_of_Occurences)) +
geom_bar(stat='identity') +
coord_flip()


#probowanie coś z płcią
guests$sex <- 1 * (str_sub(guests$imie, -1) == 'a')
guests$sex[guests$sex == 1] <- 'K'
guests$sex[guests$sex == 0] <- 'M'
guests$sex <- 1 * ((str_sub(guests$imie, -1) == 'a') | str_detect(guests$google_knowledge_occupation, 'actress') | str_detect(guests$google_knowledge_occupation, 'lady'))
summary(guests)
guests$sex[guests$sex > 0] <- 'K'
guests$sex[guests$sex == 0] <- 'M'
#trzeba by było jakiś df z imionami bo inaczej to ciezko bedzie
#mozna zrobić wykres dla pór roku np. najlepszy aktor czy coś
guests$season[guests$month %in% c(12,1,2)] <- 'winter'
guests$season[guests$month %in% c(6,7,8)] <- 'summer'
guests$season[guests$month %in% c(9,10,11)] <- 'autumn'
guests$season[guests$month %in% c(3,4,5)] <- 'spring'

#wykres group od seasonów
ggplot(guests, aes(x=group)) + geom_bar() + facet_wrap(~season) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +xlab("") + ylab("")



###########################################
#Special Events
specialEvents <- daily_show_guests
specialEvents
specialEvents <- specialEvents %>% filter(is.na(google_knowledge_occupation))
specialEvents <- specialEvents[specialEvents$raw_guest_list != "None" 
                               & specialEvents$raw_guest_list != "none"
                               & specialEvents$raw_guest_list != "no guest"
                               & specialEvents$raw_guest_list != "no Guest"
                               & specialEvents$raw_guest_list != "No guest"
                               & specialEvents$raw_guest_list != "No Guest", ]

specialEvents
###########################################



###########################################
#Most often used First Names
mostPopularFNames <- daily_show_guests
mostPopularFNames <- separate(mostPopularFNames, col = c("raw_guest_list"), into = c("fName"), sep = " ")
mostPopularFNames <- mostPopularFNames %>% group_by(fName) %>% tally() %>% rename(Number_of_Occurences = n)
#We consciously ignore the names of teams and institutions 
#They are included in the general list, but so rare that they will not appear in this list
mostPopularFNames <- mostPopularFNames %>% top_n(15, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))
mostPopularFNames
###########################################



###########################################
#Frequency of interviews
frequency <- guests %>% group_by(day) %>% tally()
#Delete 31st day of month, because of distortions on the chart
#Should be [1:28] if we want to be very objective
frequency <- frequency[1:30,]
frequency %>% print(n=Inf)
frequency %>% ggplot(aes(x=day, y=n)) + geom_bar(stat = 'identity')+geom_smooth()
#Conclusion: Most of the interviews are conducted in the middle of the month

#To samo mozna zrobic z uzyciem wlasnych funkcji (do przeksztalcenia miesiecy na kolejne dni w roku) dla pokazania czestotliwosci w przeciagu calego roku
###########################################



###########################################
#Year Frequency

guests[,"dayInYear"] <- NA
guests

for (i in guests$month)
{
  if(i == 1)
  {
    guests <- mutate(guests, dayInYear = day)
    #guests$dayInYear=guests$day
  }
  else if(i == 2)
  {
    guests <- mutate(guests, dayInYear = day + 31)
    #guests$dayInYear = guests$day+31
  }
  else if(i == 3)
  {
    #change to +28/29
    guests <- mutate(guests, dayInYear = day + 59)
    #guests$dayInYear = guests$day+59
  }
  else if(i == 4)
  {
    guests <- mutate(guests, dayInYear = day + 90)
    #guests$dayInYear = guests$day+90
  }
  else if(i == 5)
  {
    guests <- mutate(guests, dayInYear = day + 120)
    #guests$dayInYear = guests$day+120
  }
  else if(i == 6)
  {
    guests <- mutate(guests, dayInYear = day + 151)
    #guests$dayInYear = guests$day+151
  }
  else if(i == 7)
  {
    guests <- mutate(guests, dayInYear = day + 181)
    #guests$dayInYear = guests$day+181
  }
  else if(i == 8)
  {
    guests <- mutate(guests, dayInYear = day + 212)
    #guests$dayInYear = guests$day+212
  }
  else if(i == 9)
  {
    guests <- mutate(guests, dayInYear = day + 243)
    #guests$dayInYear = guests$day+243
  }
  else if(i == 10)
  {
    guests <- mutate(guests, dayInYear = day + 273)
    #guests$dayInYear = guests$day+273
  }
  else if(i == 11)
  {
    guests <- mutate(guests, dayInYear = day + 304)
    #guests$dayInYear = guests$day+304
  }
  else
  {
    guests <- mutate(guests, dayInYear = day + 334)
    #guests$dayInYear = guests$day+334
  }
}
guests
#guests <- mutate(guests, dayInYear = day + 31)
guests[guests$month == 2,]  
###########################################

