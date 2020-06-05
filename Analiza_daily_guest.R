library(ggplot2)
#For ddply
library(plyr)
#plyr should be before dplyr
library(dplyr)
library(tidyr)
library(tibble)
library(fivethirtyeight)
library(tidyverse)
library(tidytext)
#Do as.yearmoon()
library(zoo)

guests<-daily_show_guests

#zmiana na 3 kolumny - rozdzielenie daty
guests<-separate(guests, col = c("show"), into = c("year", "month", "day"), sep = "-", extra = "merge")
#zeby pozbyc sie jednego levela
guests$group[guests$group == "media"] <- "Media"
guests$year <-as.numeric(guests$year)
guests$month <-as.numeric(guests$month)
guests$day <-as.numeric(guests$day)
guests$group <-as.factor(guests$group)


guests$season[guests$month %in% c(12,1,2)] <- 'winter'
guests$season[guests$month %in% c(6,7,8)] <- 'summer'
guests$season[guests$month %in% c(9,10,11)] <- 'autumn'
guests$season[guests$month %in% c(3,4,5)] <- 'spring'


###########################################
#Special Events
specialEvents <- daily_show_guests
specialEvents <- specialEvents %>% filter(is.na(google_knowledge_occupation))
specialEvents <- specialEvents[specialEvents$raw_guest_list != "None" 
                               & specialEvents$raw_guest_list != "none"
                               & specialEvents$raw_guest_list != "no guest"
                               & specialEvents$raw_guest_list != "no Guest"
                               & specialEvents$raw_guest_list != "No guest"
                               & specialEvents$raw_guest_list != "No Guest", ]
specialEvents <- specialEvents[,c(3,5)]
specialEvents

###########################################
#ANALIZA IMION

guests2<-guests %>% filter(!is.na(google_knowledge_occupation)) 
guests2 <- guests2[guests2$raw_guest_list != "(None)" 
                   & guests2$raw_guest_list != "none"
                   & guests2$raw_guest_list != "(no guest)"
                   & guests2$raw_guest_list != "none"
                   & guests2$raw_guest_list != "No guest"
                   & guests2$raw_guest_list != "None", ]


guests3 <- guests2 %>% filter(str_detect(guests2$raw_guest_list, ' & ') | str_detect(guests2$raw_guest_list, ' and ') | str_detect(guests2$raw_guest_list, ', ' ) | str_detect(guests2$raw_guest_list, 'Jr.' ) | str_detect(guests2$raw_guest_list, 'Dr. ' ) | str_detect(guests2$raw_guest_list, 'Rep.' ) | str_detect(guests2$raw_guest_list, 'Sen.' ) | str_detect(guests2$raw_guest_list, 'Senator' )| str_detect(guests2$google_knowledge_occupation, 'band' )| str_detect(guests2$raw_guest_list, 'Admiral' ) | str_detect(guests2$raw_guest_list, 'President' ))
guests2 <- guests2 %>% filter(!(str_detect(guests2$raw_guest_list, ' & ') | str_detect(guests2$raw_guest_list, ' and ') | str_detect(guests2$raw_guest_list, ', ' ) | str_detect(guests2$raw_guest_list, 'Jr.' ) | str_detect(guests2$raw_guest_list, 'Dr. ' ) | str_detect(guests2$raw_guest_list, 'Rep.' ) | str_detect(guests2$raw_guest_list, 'Sen.' ) | str_detect(guests2$raw_guest_list, 'Senator' )| str_detect(guests2$google_knowledge_occupation, 'band' )| str_detect(guests2$raw_guest_list, 'Admiral' ) | str_detect(guests2$raw_guest_list, 'President' )))


guests2 <- separate(guests2, col = c("raw_guest_list"), into = c("imie", "nazwisko"), sep = " ", extra = "merge")

guests4 <- guests2 %>% filter(is.na(nazwisko))
guests2 <- guests2 %>% filter(!(is.na(nazwisko)))

#dzielenie Guests3 na podtypy
# 1 opcja czyli TytuÅ‚ + imiÄ™ i nazwisko
tytul <- guests3 %>% filter(str_detect(guests3$raw_guest_list, 'Sen.' ) | str_detect(guests3$raw_guest_list, 'Senator' )| str_detect(guests3$raw_guest_list, 'Admiral' ) | str_detect(guests3$raw_guest_list, 'President' )| str_detect(guests3$raw_guest_list, 'Dr. ') | str_detect(guests3$raw_guest_list, 'Rep. '))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, 'Sen.' ) | str_detect(guests3$raw_guest_list, 'Senator' )| str_detect(guests3$raw_guest_list, 'Admiral' ) | str_detect(guests3$raw_guest_list, 'President' )| str_detect(guests3$raw_guest_list, 'Dr. ' ) | str_detect(guests3$raw_guest_list, 'Rep. ')))


tytul <- separate(tytul, col = c("raw_guest_list"), into = c("tytul", "imie", "nazwisko"), sep = " ", extra = "merge")
#Wykres ludzi z tytulami

tytul$tytul[tytul$tytul == "Sen."] <- "Senator"
#wykres ilosci tytulow

tytul %>% group_by(tytul) %>% summarise( ilosc = n()) %>% ggplot(aes(x=tytul, y= ilosc)) + geom_col() +xlab("Tytul") + ylab("Ilosc") +
   ggtitle("Ilosc tytulow")+
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 


Juniory <- guests3 %>% filter(str_detect(guests3$raw_guest_list, 'Jr.'  ))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, 'Jr.'  )))
Juniory <- separate(Juniory, col = c("raw_guest_list"), into = c("imie", "nazwisko", "koncowka"), sep = " ", extra = "merge")

Juniory %>% group_by(imie) %>% summarise( ilosc = n()) %>% ggplot(aes(x=imie, y= ilosc)) + geom_col() +xlab("Imie") + ylab("Ilosc") +
   ggtitle("Najpopularniejsze imiona wsrod Jr.")+
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 


Andy <- guests3 %>% filter(str_detect(guests3$raw_guest_list, ' & ' )   | str_detect(guests3$raw_guest_list, 'and' ))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, ' & ' )   | str_detect(guests3$raw_guest_list, 'and' )))



Andy%>% group_by(group) %>% summarise( ilosc = n()) %>% ggplot(aes(x=group, y= ilosc)) + geom_col() +xlab("Grupa") + ylab("Ilosc") +
   ggtitle("Z jakiej grupy byla wiecej niz jedna gwiazda")+
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))  +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Andy%>% group_by(year) %>% summarise( ilosc = n()) %>% ggplot(aes(x=year, y= ilosc)) + geom_col() +xlab("Rok") + ylab("Ilosc") +
   ggtitle("W ktorym roku byla wiecej niz jedna gwiazda")+
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))  + coord_flip()


Bandy<- guests3 %>% filter(str_detect(guests3$google_knowledge_occupation, 'band' ) )
guests3<- guests3 %>% filter(!(str_detect(guests3$google_knowledge_occupation, 'band' ) ))


Bandy%>% group_by(season) %>% summarise( ilosc = n()) %>% arrange(desc(ilosc)) %>%
   ggplot(aes(x=season, y= ilosc)) + geom_col()  +xlab("Pory roku") + ylab("Ilosc") +
   ggtitle("W ktorym sezonie bylo najwiecej zespolow")+
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))  +
   scale_y_continuous(breaks = seq(from = 0, to = 10,by =2 )) 


#Imiona zenskie
imionaz<- read_lines("data/female-names.txt")
#Imiona meskie
imionam<- read_lines("data/male-names.txt")

#tylko Ci co majÄ… imiÄ™ wiÄ™ksze niÅ¼ 2
imiona <- guests2  %>%  mutate(title_str = imie %>% str_replace_all("[^[:alnum:]]"," ")) %>% filter( str_length(title_str)>2)


#wybranie imion Å¼eÅ„skich
imionazenskie<- imiona %>%
   unnest_tokens(output = words, input = imie ) %>% 
   filter(words %in% imionaz, is.na(as.numeric(words)))

#wybranie imion mÄ™skich
imionameskie<- imiona %>%
   unnest_tokens(output = words, input = imie ) %>% 
   filter(words %in% imionam, is.na(as.numeric(words)))


###########################################
#Wyznaczenie najbardziej popularnych osob/zespolow

#Policzenie ilosci wystapien poszczegolnych osob/zespolow i zmiana nazwy kolumny
namesOccurencem <- imionameskie %>% group_by(title_str) %>% tally() %>% rename(Number_of_Occurences = n)
namesOccurencek <- imionazenskie %>% group_by(title_str) %>% tally() %>% rename(Number_of_Occurences = n)


#Wybierze 20 najbardziej znanych. W przypadku kilku osÃ³b o takim samym wyniku zostana ne dodatkowo dobrane - w przypadku top_n(20,) wyswietli 24 osoby
mostFamousm <- namesOccurencem %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))
mostFamousk <- namesOccurencek %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))
#mostFamous %>% print(n=Inf)
###########################################

#DODANIE BAR PLOTU POZIOMEGO
namesOccurencem %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% 
   ggplot(aes(x=title_str, y=Number_of_Occurences)) +
   geom_bar(stat='identity') +
   coord_flip()+
   ggtitle("Najczesciej wystepujace imiona meskie")+
   xlab("Imiona") + ylab("Ilosc wystapien") +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))

namesOccurencek %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% 
   ggplot(aes(x=title_str, y=Number_of_Occurences)) +
   geom_bar(stat='identity') +
   coord_flip()+
   ggtitle("Najczesciej wystepujace imiona meskie")+
   xlab("Imiona") + ylab("Ilosc wystapien")+
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))


imionazenskie %>% group_by(group) %>% summarise(ilosc = n()) %>% ggplot(aes(x=group, y=ilosc)) + geom_col() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Ilosc")+
   ylab("Ilosc")  +  ggtitle("Udzial danej grupy wsrod kobiet") + theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = )) + scale_y_continuous(breaks = seq(from = 0, to = 600,by =40 )) 


imionameskie %>% group_by(group) %>% summarise(ilosc = n()) %>% ggplot(aes(x=group, y=ilosc)) + geom_col() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Ilosc")+
   ylab("Ilosc") +  ggtitle("Udzial danej grupy wsrod mezczyzn") + theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = )) + scale_y_continuous(breaks = seq(from = 0, to = 600,by =40 )) 


############################################################################################

#wykres group od seasonow
ggplot(imionazenskie, aes(x=group)) + geom_bar() + 
   facet_wrap(~season) + ggtitle("Ilosc wywiadow w poszczegolnych porach roku wsrod kobiet") + 
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
   xlab("") + ylab("Ilosc")

ggplot(imionameskie, aes(x=group)) + geom_bar() +
   facet_wrap(~season) + ggtitle("Ilosc wywiadow w poszczegolnych porach roku wsrod mezczyzn") +
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
   xlab("") + ylab("Ilosc")


#wykres rÃ³Å¼nych grup na przedziale lat
df <- guests %>% group_by(group, year) %>% summarise( ilosc = n()) 
#usuniÄ™cie grupy NA
df <- na.omit(df) 

ggplot(df, aes(fill=group, x=year, y=ilosc)) + 
   geom_bar(position="stack", stat="identity") + 
   scale_x_continuous(breaks = c(1999,2000,2001,2002,2003,2004,
                                 2005,2006,2007,2008,2009,2010,2011,
                                 2012,2013,2014,2015)) + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
   ggtitle("Wystepowanie roznych grup na przedziale lat")+
   xlab("Rok")+
   ylab("Ilosc") + 
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5,  face= 'bold', margin = ))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
   theme(axis.title.y= element_text( face="bold")) 


###########################################
#Frequency of interviews
frequency <- guests %>% group_by(day) %>% tally()
#Delete 31st day of month, because of distortions on the chart
#Should be [1:28] if we want to be very objective
frequency <- frequency[1:30,]
frequency %>% ggplot(aes(x=day, y=n)) + geom_bar(stat = 'identity')+geom_smooth()+
   ggtitle("Ilosc wywiadow na przestrzeni miesiaca")+
   xlab("Dzien miesiaca")+
   ylab("Calkowita ilosc wywiadow") + 
   theme_test() +
   theme(plot.title = element_text(size = 15, hjust=0.5,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))
###########################################


###########################################
#Amount of interviews in each day of a week
interviewDate <- daily_show_guests
interviewDate <- interviewDate %>% add_column(guests$month, guests$day)

#dplyr::rename z powodu konfliktu z plyr
interviewDate <- dplyr::rename(interviewDate, month=`guests$month`, day=`guests$day`, date=`show`)

interviewDate <- interviewDate[,c(1,3,6,7)]

#interviewDate <- interviewDate[interviewDate$year >= 2005,]

#Niepotrzebne, bo domyslnie jest w dobrym formacie
#interviewDate$show <- as.Date(interviewDate$month)
interviewDate$textMonth <- as.yearmon(interviewDate$date)
interviewDate$textMonthF <- factor(interviewDate$textMonth)
interviewDate$dayInYear <- as.Date(interviewDate$date,format='%Y-%m-%d')
interviewDate$dayInYear <- lubridate::yday(interviewDate$dayInYear)
interviewDate$weekDay <- weekdays(interviewDate$date)
interviewDate$weekNumber <- strftime(interviewDate$date, format = "%V")

interviewDate <- ddply(interviewDate,.(textMonthF),transform)

topDay <- interviewDate %>% group_by(weekDay) %>% tally()
topDay
orderDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Sunday")

topDay <- topDay[match(orderDays, topDay$weekDay),]
topDay
###########################################



###########################################
#Amount of interviews in each moth
monthlyFrequency <- interviewDate %>% group_by(month) %>% tally()
monthlyFrequency %>% ggplot(aes(x=month, y=n)) + geom_bar(stat = 'identity') + 
   ggtitle("Czestotliwosc wywiadow w poszczegolnych miesiacach") + 
   xlab("Miesiac")+
   ylab("Calkowita ilosc wywiadow") + 
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))
###########################################


###########################################
#Amount of interviews in each year
yearFrequency <- interviewDate %>% group_by(year) %>% tally()
yearFrequency %>% ggplot(aes(x=year, y=n)) + geom_bar(stat = 'identity') + 
   ggtitle("Czestotliwosc wywiadow w poszczegolnych latach") + 
   xlab("Rok")+
   ylab("Calkowita ilosc wywiadow")+
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))
###########################################



###########################################
#Amount of interviews during the year 
allFrequency <- interviewDate %>% group_by(dayInYear) %>% tally()
# allFrequency %>% ggplot(aes(x=dayInYear, y=n)) + geom_area() + 
#    ggtitle("Czestotliwosc wywiadow na przestrzeni roku") + 
#    theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))

spline.d <- as.data.frame(spline(allFrequency$dayInYear, allFrequency$n))
allFrequency %>% ggplot(aes(x=dayInYear, y=n)) + geom_point() + geom_line(data = spline.d, aes(x = x, y = y)) + 
   ggtitle("Czestotliwosc wywiadow na przestrzeni roku") + 
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   xlab("Numer dnia w roku")+
   ylab("Calkowita ilosc wywiadow")+
   theme(plot.title = element_text(size = 15, hjust=0.5, face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))
###########################################


