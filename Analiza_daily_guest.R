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

#Policzenie ilosci wystapien poszczegolnych osob/zespolow i zmiana nazwy kolumny
namesOccurence <- daily_show_guests %>% group_by(raw_guest_list) %>% tally() %>% rename(Number_of_Occurences = n)
#Usuniecie dwoch pustych rekordow, chociaz to w sumie bez znaczenia
namesOccurence <- namesOccurence[3:nrow(namesOccurence),]
namesOccurence
mostFamous <- namesOccurence %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))

#Dziala :D
#mostFamous %>% print(n=Inf)
#mostFamous %>% data.frame()

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


















