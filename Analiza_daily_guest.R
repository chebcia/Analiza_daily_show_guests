library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(fivethirtyeight)
library(tidyverse)
guests<-daily_show_guests
guests

#zmiana na 3 kolumny - rozdzielenie daty
guests<-separate(guests, col = c("show"), into = c("year", "month", "day"), sep = "-", extra = "merge")
#zeby pozbyc sie jednego levela
guests$group[guests$group == "media"] <- "Media"
guests$year <-as.numeric(guests$year)
guests$month <-as.numeric(guests$month)
guests$day <-as.numeric(guests$day)
guests$group <-as.factor(guests$group)




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
# 1 opcja czyli Tytuł + imię i nazwisko
tytul <- guests3 %>% filter(str_detect(guests3$raw_guest_list, 'Sen.' ) | str_detect(guests3$raw_guest_list, 'Senator' )| str_detect(guests3$raw_guest_list, 'Admiral' ) | str_detect(guests3$raw_guest_list, 'President' )| str_detect(guests3$raw_guest_list, 'Dr. ') | str_detect(guests3$raw_guest_list, 'Rep. '))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, 'Sen.' ) | str_detect(guests3$raw_guest_list, 'Senator' )| str_detect(guests3$raw_guest_list, 'Admiral' ) | str_detect(guests3$raw_guest_list, 'President' )| str_detect(guests3$raw_guest_list, 'Dr. ' ) | str_detect(guests3$raw_guest_list, 'Rep. ')))


tytul <- separate(tytul, col = c("raw_guest_list"), into = c("tytuł", "imie", "nazwisko"), sep = " ", extra = "merge")
#Wykres ludzi z tytułami 

tytul$tytuł[tytul$tytuł == "Sen."] <- "Senator"
#wykres ilosci tytułów


tytul %>% group_by(tytuł) %>% summarise( ilosc = n()) %>% ggplot(aes(x=tytuł, y= ilosc)) + geom_col() +xlab("Tytuł") + ylab("Ilość") +
   ggtitle("Ilość tytułów")+
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 


Juniory <- guests3 %>% filter(str_detect(guests3$raw_guest_list, 'Jr.'  ))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, 'Jr.'  )))
Juniory <- separate(Juniory, col = c("raw_guest_list"), into = c("imie", "nazwisko", "koncówka"), sep = " ", extra = "merge")

Juniory %>% group_by(imie) %>% summarise( ilosc = n()) %>% ggplot(aes(x=imie, y= ilosc)) + geom_col() +xlab("Imię") + ylab("Ilość") +
   ggtitle("Najpopularniejsze imiona wsród Jr.")+
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 


Andy <- guests3 %>% filter(str_detect(guests3$raw_guest_list, ' & ' )   | str_detect(guests3$raw_guest_list, 'and' ))
guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, ' & ' )   | str_detect(guests3$raw_guest_list, 'and' )))

#wszedzie mozna dodac scale_x i zrobić to ładniej

Andy%>% group_by(group) %>% summarise( ilosc = n()) %>% ggplot(aes(x=group, y= ilosc)) + geom_col() +xlab("Grupa") + ylab("Ilość") +
   ggtitle("Z jakiej grupy było więcej niż jedna gwiazda")+
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))  +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Andy%>% group_by(year) %>% summarise( ilosc = n()) %>% ggplot(aes(x=year, y= ilosc)) + geom_col() +xlab("Grupa") + ylab("Ilość") +
   ggtitle("W którym roku była więcej niż jedna gwiazda")+
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold"))  + coord_flip()


Bandy<- guests3 %>% filter(str_detect(guests3$google_knowledge_occupation, 'band' ) )

guests3 <- guests3 %>% filter(!(str_detect(guests3$raw_guest_list, 'Sen.' ) | str_detect(guests3$raw_guest_list, 'Senator' )| str_detect(guests3$raw_guest_list, 'Admiral' ) | str_detect(guests3$raw_guest_list, 'President' )| str_detect(guests3$raw_guest_list, 'Dr. ' ) | str_detect(guests3$raw_guest_list, 'Rep. ')))


#imiona zenskie
imionaz<- read_lines("data/first-names.txt")
imionam<- read_lines("data/male-names.txt")

#tylko Ci co mają imię większe niż 2
imiona <- guests2  %>%  mutate(title_str = imie %>% str_replace_all("[^[:alnum:]]"," ")) %>% filter( str_length(title_str)>2)
          
          
#wybranie imion żeńskich
imionazenskie<- imiona %>%
   unnest_tokens(output = words, input = imie ) %>% 
   filter(words %in% imionaz, is.na(as.numeric(words)))

#wybranie imion męskich
imionameskie<- imiona %>%
   unnest_tokens(output = words, input = imie ) %>% 
   filter(words %in% imionam, is.na(as.numeric(words)))


###########################################
#Wyznaczenie najbardziej popularnych osob/zespolow

#Policzenie ilosci wystapien poszczegolnych osob/zespolow i zmiana nazwy kolumny
namesOccurencem <- imionameskie %>% group_by(title_str) %>% tally() %>% rename(Number_of_Occurences = n)
namesOccurencek <- imionazenskie %>% group_by(title_str) %>% tally() %>% rename(Number_of_Occurences = n)
#Usuniecie dwoch pustych rekordow, chociaz to w sumie bez znaczenia dla tych danych


#Wybierze 20 najbardziej znanych. W przypadku kilku osób o takim samym wyniku zostana ne dodatkowo dobrane - w przypadku top_n(20,) wyswietli 24 osoby
mostFamousm <- namesOccurencem %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))
mostFamousk <- namesOccurencek %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences))
#mostFamous %>% print(n=Inf)
###########################################

#DODANIE BAR PLOTU POZIOMEGO
namesOccurencem %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% 
   ggplot(aes(x=title_str, y=Number_of_Occurences)) +
   geom_bar(stat='identity') +
   coord_flip()

namesOccurencek %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% 
   ggplot(aes(x=title_str, y=Number_of_Occurences)) +
   geom_bar(stat='identity') +
   coord_flip()




############################################################################################
#seasons
guests$season[guests$month %in% c(12,1,2)] <- 'winter'
guests$season[guests$month %in% c(6,7,8)] <- 'summer'
guests$season[guests$month %in% c(9,10,11)] <- 'autumn'
guests$season[guests$month %in% c(3,4,5)] <- 'spring'

#wykres group od seasonÃ³w
ggplot(imionazenskie, aes(x=group)) + geom_bar() + facet_wrap(~season) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +xlab("") + ylab("")
ggplot(imionameskie, aes(x=group)) + geom_bar() + facet_wrap(~season) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +xlab("") + ylab("")





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
#wykres różnych grup na przedziale lat
df <- guests %>% group_by(group, year) %>% summarise( ilosc = n()) 
#usunięcie grupy NA
df <- na.omit(df) 

ggplot(df, aes(fill=group, x=year, y=ilosc)) + 
   geom_bar(position="stack", stat="identity") + 
   scale_x_continuous(breaks = c(1999,2000,2001,2002,2003,2004,
                                 2005,2006,2007,2008,2009,2010,2011,
                                 2012,2013,2014,2015)) + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
   ggtitle("Występowanie różnych grup na przedziale lat")+
   xlab("Rok")+
   ylab("Ilość") + 
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 



Bandy%>% group_by(season) %>% summarise( ilosc = n()) %>% ggplot(aes(x=season, y= ilosc)) + geom_col()  +xlab("Pory roku") + ylab("Ilość") +
   ggtitle("W którym sezonie było najwięcej zespołów")+
   theme_test() +
   theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
   theme(axis.title.x = element_text( face="bold"))+
   theme(axis.title.y= element_text( face="bold")) 






