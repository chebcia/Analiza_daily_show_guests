library(fivethirtyeight)
guests<-daily_show_guests
#zamienianie na dwie kolumny
guests <- separate(guests, col = c("raw_guest_list"), into = c("imie", "nazwisko"), sep = " ", extra = "merge")

#zmiana na 3 kolumny - rozdzielenie daty
guests<-separate(guests, col = c("show"), into = c("year", "month", "day"), sep = "-", extra = "merge")

guests$year <-as.numeric(guests$year)
guests$month <-as.numeric(guests$month)
guests$day <-as.numeric(guests$day)


guests$group <-as.factor(guests$group)
