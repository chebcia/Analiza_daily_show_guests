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

