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


