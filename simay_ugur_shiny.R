
library(shiny)

library(magrittr) # pipe operations
library(lubridate) # date operations
library(tidyverse) # ggplot2, tidyr, dplyr...
library(gridExtra) # multiple grid-based plots on a page
library(ggplot2)
library(RColorBrewer)
library(coronavirus)#
library(tidyverse)
library(tidycovid19) 
library(yaml)
library(scales)
library(dplyr)
library("shinythemes")
library("tinytex")

data1.confirmed <-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = T)
data1.deaths <-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header= T)
data1.recovered <-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv" ,header = T)

n.col <- ncol(data1.confirmed)
## get dates from column names
dates <- names(data1.confirmed)[5:n.col] %>% substr(2,8) %>% lubridate:: mdy()


min.date <- min(dates)
max.date <- max(dates)
min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y') %>% paste('UTC')

veriler.onaylanan <- data1.confirmed %>% select(-c(Province.State, Lat, Long)) 
veriler.onaylanan%<>% dplyr:: rename(ulke=Country.Region)
veriler.onaylanan %<>% gather(key=tarih, value=sayi, -ulke)
veriler.onaylanan %<>% mutate(tarih = tarih %>% substr(2,8) %>% lubridate:: mdy())


veriler.vefat <- data1.deaths %>% select(-c(Province.State, Lat, Long)) 
veriler.vefat %<>% dplyr:: rename(ulke=Country.Region)
veriler.vefat %<>% gather(key=tarih, value=sayi, -ulke)
veriler.vefat %<>% mutate(tarih = tarih %>% substr(2,8) %>% lubridate:: mdy())


veriler.iyilesen <- data1.recovered %>% select(-c(Province.State, Lat, Long)) 
veriler.iyilesen %<>% dplyr:: rename(ulke=Country.Region)
veriler.iyilesen %<>% gather(key=tarih, value=sayi, -ulke)
veriler.iyilesen %<>% mutate(tarih = tarih %>% substr(2,8) %>% lubridate:: mdy())


veriler.onaylanan %<>% group_by(ulke, tarih) %>% 
    summarise(sayi=sum(sayi, na.rm=T)) %>% as.data.frame()
veriler.vefat %<>% group_by(ulke, tarih) %>% 
    summarise(sayi=sum(sayi, na.rm=T)) %>% as.data.frame()
veriler.iyilesen %<>% group_by(ulke, tarih) %>% 
    summarise(sayi=sum(sayi, na.rm=T)) %>% as.data.frame() 

veriler.onaylanan %<>% dplyr:: rename(olgu_sayi=sayi)
veriler.vefat %<>% dplyr:: rename(vefat_sayi=sayi)
veriler.iyilesen %<>% dplyr:: rename(iyilesen_sayi=sayi)


veriler <- veriler.onaylanan%>% merge(veriler.vefat, all=T) %>% merge(veriler.iyilesen, all=T)


data.dunya <- veriler %>% group_by(tarih) %>%
    summarise(ulke='dunya',
              olgu_sayi= sum(olgu_sayi, na.rm=T),
              vefat_sayi = sum(vefat_sayi, na.rm=T),
              iyilesen_sayi = sum(iyilesen_sayi, na.rm=T))


veriler<-veriler%<>% rbind(data.dunya)

veriler %<>% mutate(aktif.olgu = olgu_sayi - vefat_sayi - iyilesen_sayi)


veriler %<>% arrange(ulke,tarih)


n <- nrow(veriler)
day1 <- min(veriler$tarih)
veriler %<>% 
    mutate(yeni.olgu = ifelse(tarih == day1, NA, olgu_sayi - lag(olgu_sayi,n=1)),
           yeni.vefat =ifelse(tarih == day1, NA, vefat_sayi - lag(vefat_sayi, n=1)),
           yeni.iyilesen = ifelse(tarih == day1, NA, iyilesen_sayi - lag(iyilesen_sayi, n=1)))




veriler%<>% mutate(yeni.olgu = ifelse(yeni.olgu < 0, 0, yeni.olgu),
                   yeni.vefat = ifelse(yeni.vefat < 0, 0, yeni.vefat),
                   yeni.iyilesen = ifelse(yeni.iyilesen < 0, 0, yeni.iyilesen))


veriler %<>% mutate(ust.mortalite.oran = (100 * vefat_sayi/ (vefat_sayi + iyilesen_sayi)) %>%
                        round(1))

veriler %<>% mutate(alt.mortalite.oran = (100 * vefat_sayi / olgu_sayi) %>% round(1))

veriler%<>% mutate(gunluk.oran = (100 * yeni.vefat / (yeni.vefat + yeni.iyilesen )) %>%
                       round(1))

veriler$gunluk.oran=ifelse(veriler$gunluk.oran=="NaN",0,veriler$gunluk.oran)
veriler$ust.mortalite.oran=ifelse(veriler$ust.mortalite.oran=="NaN",0,veriler$ust.mortalite.oran)
veriler$alt.mortalite.oran=ifelse(veriler$alt.mortalite.oran=="NaN",0,veriler$alt.mortalite.oran)


data.long <- veriler %>%
    select(c(ulke, tarih, olgu_sayi, aktif.olgu, iyilesen_sayi, vefat_sayi))
rates.long <- veriler %>%
    select(c(ulke, tarih, ust.mortalite.oran, alt.mortalite.oran,gunluk.oran ))

data.long<- data.long %>%
    gather(key=type, value=count, -c(ulke, tarih))
rates.long<- rates.long %>%
    # mutate(country=factor(country, levels=top.countries)) %>%
    gather(key=type, value=count, -c(ulke, tarih))


data.long %<>% mutate(type=recode_factor(type, olgu_sayi='Olgu',
                                         aktif.olgu='Aktif',
                                         iyilesen_sayi='iyilesen' , vefat_sayi='Vefat'))

rates.long %<>% mutate(type=recode_factor(type,gunluk.oran='gunluk.mortalite', alt.mortalite.oran='alt.mortalite',
                                          ust.mortalite='ust.mortalite'))


dunya.uzun <- data.long %>% filter(ulke == 'dunya')

dunya.alan.graph <- dunya.uzun %>% filter(type != 'Olgu') %>%
    ggplot(aes(x=tarih, y=count)) +
    geom_area(aes(fill=type), alpha=0.5) +
    labs(title=paste0('Dünya Capinda Vaka Sayisi (günlük ölceğg) - ',max.date.txt)) +
    scale_fill_manual(values=c( 'green', 'black','blue')) +
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=7),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'),
          legend.text=element_text(size=9),
          axis.text=element_text(size=9),
          axis.text.x=element_text(angle=45, hjust=1))


dunya.cizgi.graph <- dunya.uzun %>%
    ggplot(aes(x=tarih, y=count)) +
    geom_line(aes(color= type)) +
    labs(title=paste0('Dünya Capinda Vaka Sayilari (Log Olcek)', max.date.txt)) +
    scale_color_manual(values=c('purple', 'red', 'green', 'black'))+
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=7),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'),
          legend.text=element_text(size=9),
          axis.text=element_text(size=9),
          axis.text.x=element_text(angle=45, hjust=1)) +
    scale_y_continuous(trans='log10',
                       labels = trans_format("log10", math_format(10^.x)),
                       breaks = c(1000,10000,100000,1e05))

son.tarih <- max(dunya.uzun$tarih)
turkiye.long <- veriler %>% filter(ulke == 'Turkey', tarih>"2020-03-10")

turkiye.long <- data.long %>% filter(ulke == 'Turkey')

turkiye.alan.graph <- turkiye.long %>% filter(type != 'Olgu') %>%
    ggplot(aes(x=tarih, y=count)) +
    geom_area(aes(fill=type), alpha=0.5) +
    labs(title=paste0("Turkiye'de Durum")) +
    scale_fill_manual(values=c('blue', 'green', 'black')) +
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'),
          legend.text=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1))

turkiye.cizgi.graph <- turkiye.long %>%  filter(type != 'Olgu')%>%
    ggplot(aes(x=tarih, y=count)) +
    geom_line(aes(color=type)) +
    labs(title=paste0("Turkiye'de Durum (log ölcek)")) +
    scale_color_manual(values=c('blue', 'green', 'red')) +
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'),
          legend.text=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    geom_hline(aes(yintercept = max(turkiye.long$count)),
               size=1,color="red")+
    scale_y_continuous(trans='log10',
                       labels = trans_format("log10", math_format(10^.x)),
                       breaks = c(1000,10000,100000,1e05))


data.dunya <- veriler %>% filter(ulke == 'dunya')
n <- nrow(data.dunya)

aktif.olgu.graph <- ggplot(data.dunya, aes(x=tarih, y=aktif.olgu)) +
    geom_smooth( method = 'loess' )+
    geom_point() + 
    xlab('') + ylab('Sayi') + labs(title='Aktif Olgu') +
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    scale_y_continuous()

gunluk.oran.graph <- ggplot(data.dunya, aes(x=tarih, y=yeni.olgu)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Sayi') + labs(title='Gunluk Yeni Olgu') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

olum.graph <- ggplot(data.dunya, aes(x=tarih, y=vefat_sayi)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Sayi') + labs(title='Vefat Eden Vaka Grafigi') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

iyilesen.graph <- ggplot(data.dunya, aes(x=tarih, y=iyilesen_sayi)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Sayi') + labs(title='Iyilesen Vaka Grafigi') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

yeni.olum.graph <- ggplot(data.dunya, aes(x=tarih, y=yeni.vefat)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Sayi') + labs(title='Yeni Vefat Eden Vaka Grafigi') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

yeni.iyilesen.graph <- ggplot(data.dunya, aes(x=tarih, y=yeni.iyilesen)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Sayi') + labs(title='Yeni Iyilesen Vaka Grafigi') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

mortalite.dunya <- ggplot(data.dunya, aes(x=tarih)) +
    geom_line(aes(y=ust.mortalite.oran, colour='Ust Mortalite Orani')) +
    geom_line(aes(y=alt.mortalite.oran, colour='Alt Mortalite Orani')) +
    geom_line(aes(y=gunluk.oran, colour='Gunluk')) +
    xlab('') + ylab('Olum Orani (%)') + labs(title='Dunya Geneli Oranlar') +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=45, hjust=1))


y.max <- data.dunya[n-(14:0), ] %>% select(ust.mortalite.oran, alt.mortalite.oran, gunluk.oran) %>% max()

mortalite.dunya.2 <- ggplot(data.dunya[n-(14:0),], aes(x=tarih)) +
    geom_line(aes(y=ust.mortalite.oran, colour='Ust Mortalite Oran')) +
    geom_line(aes(y=alt.mortalite.oran, colour='Alt Mortalite Oran')) +
    geom_line(aes(y=gunluk.oran, colour='Gunluk')) +
    xlab('') + ylab('Olum Orani(%)') + labs(title='Dunyada Son 2 hafta Yuzdeleri') +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=45, hjust=1)) +
    ylim(c(0, y.max))



data.turkiye <- veriler %>% filter(ulke == 'Turkey')

mortalite.turkey <- ggplot(data.turkiye, aes(x=tarih)) +
    geom_line(aes(y=ust.mortalite.oran, colour='Üst Mortalite Oran')) +
    geom_line(aes(y=alt.mortalite.oran, colour='Alt Mortalite Oran')) +
    geom_line(aes(y=gunluk.oran, colour='Günlük')) +
    xlab('') + ylab('Ölüm oranı(%)') + labs(title='Türkiye Yüzdeleri') +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=90, hjust=0,vjust=0.5))+
    scale_x_date(breaks = scales::date_breaks("week"),
                 labels = scales::date_format("%d %b"))

y.max <- data.turkiye[n-(14:0), ] %>% select(ust.mortalite.oran, alt.mortalite.oran, gunluk.oran) %>% max()
mortalite.turkey2 <- ggplot(data.turkiye[n-(14:0),], aes(x=tarih)) +
    geom_line(aes(y=ust.mortalite.oran, colour='Üst Mortalite Oran')) +
    geom_line(aes(y=alt.mortalite.oran, colour='Alt Mortalite Oran')) +
    geom_line(aes(y=gunluk.oran, colour='Günlük')) +
    xlab('') + ylab('Ölüm oranı(%)') + labs(title="Türkiye'deki Son 2 hafta Yüzdeleri") +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=45, hjust=1)) +
    ylim(c(0, y.max))+
    scale_x_date(breaks = scales::date_breaks("day"),
                 labels = scales::date_format("%d %B"))

data.latest.all <- veriler %>% filter(tarih == max(tarih)) %>%
    select(ulke, tarih, olgu_sayi, yeni.olgu,
           aktif.olgu,iyilesen_sayi,vefat_sayi, yeni.vefat,olum.oranı=alt.mortalite.oran) %>%
    mutate(ranking = dense_rank(desc(olgu_sayi)))


k <- 15
top.countries <- data.latest.all %>% filter(ranking <= k + 1) %>%
    arrange(ranking) %>% pull(ulke) %>% as.character()
top.countries %>% setdiff('dunya') %>% print()


data.latest <- data.latest.all %>% filter(!is.na(ulke)) %>%
    mutate(ulke=ifelse(ranking <= k + 1, as.character(ulke), 'Others')) %>%
    mutate(ulke=ulke %>% factor(levels=c(top.countries, 'Others')))

data.latest %<>% group_by(ulke) %>%
    summarise(olgu_sayi=sum(olgu_sayi), yeni.olgu=sum(yeni.olgu),
              aktif.olgu=sum(aktif.olgu),
              iyilesen_sayi=sum(iyilesen_sayi), vefat_sayi=sum(vefat_sayi),
              yeni.vefat=sum(yeni.vefat)) %>%
    mutate(olum.oranı=(100 * vefat_sayi/olgu_sayi) %>% round(1))
data.latest %<>% select(c(ulke, olgu_sayi, vefat_sayi, olum.oranı,
                          yeni.olgu,yeni.vefat,  aktif.olgu))

data.latest.long <- data.latest %>% filter(ulke!='dunya') %>%
    gather(key=type, value=count, -ulke)

data.latest.long <- data.latest.long %>% mutate(type=recode_factor(type,olgu_sayi='Toplam Onaylanan Vakalar',
                                                                     olum.oranı='Toplam Olüm Orani (%)',
                                                                     yeni.olgu='Yeni Onaylanan Vakalar (bir gün öncesine göre kiyaslanir)',
                                                                     yeni.vefat='Günlük Vefat Sayisi (bir gün öncesine göre kiyaslanir)',
                                                                     aktif.olgu='Güncel Covid-19 Vakalari'))



 bar1<- data.latest.long %>% filter(type=="Toplam Onaylanan Vakalar") %>% 
    ggplot(aes(x=ulke, y=count, fill=ulke, group=ulke)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=count, y=count), size=3, vjust=0) +
    xlab('') + ylab('') +
    labs(title=paste0('En cok Vakaların Görüldüğü 15 Ülke - ', max.date.txt)) +
    scale_fill_discrete(name='Ulke', labels=aes(count)) +
    theme(legend.title=element_blank(),
          legend.position='none',
          plot.title=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    facet_wrap(~type, ncol=1, scales='free_y') 

 bar2<- data.latest.long %>% filter(type=="Toplam Olüm Orani (%)") %>% 
    ggplot(aes(x=ulke, y=count, fill=ulke, group=ulke)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=count, y=count), size=3, vjust=0) +
    xlab('') + ylab('') +
    labs(title=paste0('En cok Vakaların Görüldüğü 15 Ülke - ', max.date.txt)) +
    scale_fill_discrete(name='Ulke', labels=aes(count)) +
    theme(legend.title=element_blank(),
          legend.position='none',
          plot.title=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    facet_wrap(~type, ncol=1, scales='free_y') 


 bar3<- data.latest.long %>% filter(type=="Yeni Onaylanan Vakalar (bir gün öncesine göre kiyaslanir)") %>% 
    ggplot(aes(x=ulke, y=count, fill=ulke, group=ulke)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=count, y=count), size=3, vjust=0) +
    xlab('') + ylab('') +
    labs(title=paste0('En cok Vakaların Görüldüğü 15 Ülke - ', max.date.txt)) +
    scale_fill_discrete(name='Ulke', labels=aes(count)) +
    theme(legend.title=element_blank(),
          legend.position='none',
          plot.title=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    facet_wrap(~type, ncol=1, scales='free_y') 

 bar4<- data.latest.long %>% filter(type=="Günlük Vefat Sayisi (bir gün öncesine göre kiyaslanir)") %>% 
    ggplot(aes(x=ulke, y=count, fill=ulke, group=ulke)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=count, y=count), size=3, vjust=0) +
    xlab('') + ylab('') +
    labs(title=paste0('En cok Vakaların Görüldüğü 15 Ülke - ', max.date.txt)) +
    scale_fill_discrete(name='Ulke', labels=aes(count)) +
    theme(legend.title=element_blank(),
          legend.position='none',
          plot.title=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    facet_wrap(~type, ncol=1, scales='free_y') 

 bar5<- data.latest.long %>% filter(type=="Güncel Covid-19 Vakalari") %>% 
    ggplot(aes(x=ulke, y=count, fill=ulke, group=ulke)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=count, y=count), size=3, vjust=0) +
    xlab('') + ylab('') +
    labs(title=paste0('En cok Vakaların Görüldüğü 15 Ülke - ', max.date.txt)) +
    scale_fill_discrete(name='Ulke', labels=aes(count)) +
    theme(legend.title=element_blank(),
          legend.position='none',
          plot.title=element_text(size=8),
          axis.text=element_text(size=8),
          axis.text.x=element_text(angle=45, hjust=1)) +
    facet_wrap(~type, ncol=1, scales='free_y') 

 
 df <- download_merged_data(cached = TRUE, silent = TRUE)
 
 g_tur<-df %>% 
     filter(iso3c == "TUR") %>%
     rename(tarih=date)%>%
     mutate(
         yeni_vakalar = confirmed - lag(confirmed),
         ave_new_cases = rollmean(yeni_vakalar, 7, na.pad=TRUE, align="right")
     ) %>%
     filter(!is.na(yeni_vakalar), !is.na(ave_new_cases)) %>%
     ggplot(aes(x = tarih)) +
     geom_bar(aes(y = yeni_vakalar), stat = "identity", fill = "mistyrose3") +
     geom_line(aes(y = ave_new_cases), color ="mediumorchid4",size=1) +
     labs(title="Türkiye") +
     theme_minimal()
 
 
 g_usa<-df %>% 
     filter(iso3c == "USA") %>%
     rename(tarih=date)%>%
     mutate(
         yeni_vakalar = confirmed - lag(confirmed),
         ave_new_cases = rollmean(yeni_vakalar, 7, na.pad=TRUE, align="right")
     ) %>%
     filter(!is.na(yeni_vakalar), !is.na(ave_new_cases)) %>%
     ggplot(aes(x = tarih)) +
     geom_bar(aes(y = yeni_vakalar), stat = "identity", fill = "lightsteelblue") +
     geom_line(aes(y = ave_new_cases), color ="mediumorchid4",size=1) +
     labs(title="Amerika") +
     theme_minimal()
 
 
 g_bra<-df %>% 
     filter(iso3c == "BRA") %>%
     rename(tarih=date)%>%
     mutate(
         yeni_vakalar = confirmed - lag(confirmed),
         ave_new_cases = rollmean(yeni_vakalar, 7, na.pad=TRUE, align="right")
     ) %>%
     filter(!is.na(yeni_vakalar), !is.na(ave_new_cases)) %>%
     ggplot(aes(x = tarih)) +
     geom_bar(aes(y = yeni_vakalar), stat = "identity", fill = "palevioletred") +
     geom_line(aes(y = ave_new_cases), color ="mediumorchid4",size=1) +
     labs(title="Brezilya") +
     theme_minimal()
 
 g_chn<-df %>% 
     filter(iso3c == "CHN") %>%
     rename(tarih=date)%>%
     mutate(
         yeni_vakalar = confirmed - lag(confirmed),
         ave_new_cases = rollmean(yeni_vakalar, 7, na.pad=TRUE, align="right")
     ) %>%
     filter(!is.na(yeni_vakalar), !is.na(ave_new_cases)) %>%
     ggplot(aes(x = tarih)) +
     geom_bar(aes(y = yeni_vakalar), stat = "identity", fill = "cadetblue4") +
     geom_line(aes(y = ave_new_cases), color ="mediumorchid4",size=1.2) +
     labs(title="Çin") +
     theme_minimal()
 

# SHİNY



ui <- fluidPage(

    # Application title
    titlePanel("Covid - 19 Veri Seti Görselleştirmesi"),

    theme = shinytheme("simplex"),
        mainPanel(
            
            tabsetPanel(type = "tab", 
                        
                        tabPanel(title=" En Çok Vakanın Görüldüğü 15 Ülke",
                                 
                                 radioButtons(inputId = "radioBar",
                                              label = "Tercih Edilen Grafiği Seçiniz!",
                                              choices = c("Toplam Onaylanan Vakaların Grafiği"= 1, 
                                                          "Toplam Olüm Orani (%) Grafiği"= 2,
                                                          "Yeni Onaylanan Vakalar Grafiği"= 3, 
                                                          "Günlük Vefat Sayısı Grafiği"= 4,
                                                          "Güncel Covid-19 Vakalarinin Grafiği"= 5),
                                              selected = 1), plotOutput("bar")),
                        
                        
                        tabPanel(title="Seçilen Dört Ülke İçin Dağılım Grafikleri",
                                 
                                 radioButtons(inputId = "radioDagilim",
                                              label = "Tercih Edilen Grafiği Seçiniz!",
                                              choices = c("Türkiye için Dağılım Grafiği"= 1, 
                                                          "Amerika için Dağılım Grafiği"= 2,
                                                          "Brezilya için Dağılım Grafiği"= 3, 
                                                          "Çin için Dağılım Grafiğii"= 4),
                                              selected = 1), plotOutput("dagilim")),
                        
                      
                        
                        
                        
                        tabPanel(title = "Dünya Grafikleri", 
                                 radioButtons(inputId = "radioDunya", 
                                              label="Dünya için Tercih Edilen Grafiği Seçiniz!", 
                                              choices = c("Dünya'daki Vakalar için Çizgi Grafiği"= 1, 
                                                          "Dünya'daki Vakalar için Alan Grafiği"= 2,
                                                          "Dünya'daki Aktif Vaka Sayısı Grafiği"= 3, 
                                                          "Dünya'daki Vefat Eden Vaka Sayısı Grafiği"= 4,
                                                          "Dünya'daki Yeni İyileşen Vaka Sayısı Grafiği"= 5, 
                                                          "Dünya'daki Yeni Vefat Eden Sayısı Grafiği"= 6,
                                                          "Dünya'daki İyileşen Vaka Sayısı Grafiği"= 7),
                                              selected = 1), plotOutput("dunya")),
                        
                        tabPanel(title="Dünya Ölüm Oranları Grafikleri",
                                 radioButtons(inputId = "radioDunyaOran",
                                              label = "Dünya için Tercih Edilen Oran Grafiğini Seçiniz!",
                                              choices = c("Günlük Ölüm Oranı Grafiği"= 1,
                                                          "Ölüm Oranları Grafiği" =2,
                                                          "Ölüm Oranları Grafiği (Son İki Hafta)"=3),
                                              selected = 3), plotOutput("dunyaOran")),
                        
                        tabPanel(title = "Türkiye Grafikleri", 
                                 radioButtons(inputId = "radioTurkey", 
                                              label="Türkiye için Tercih Edilen Grafiği Seçiniz!", 
                                              choices = c("Türkiye'deki Vakalar için Alan Grafiği"= 1, 
                                                          "Türkiye'deki Vakalar için Çizgi Grafiği"= 2,
                                                          "Türkiye'deki Ölüm Oranları Grafiği"= 3, 
                                                          "Türkiye'deki Ölüm Oranları Grafiği (Son İki Hafta)"= 4),
                                              selected = 4), plotOutput("turkey"))
                        
                        )
            
            
            
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$dagilim = renderPlot({
        
        switch (input$radioDagilim, "1" = g_tur, "2" = g_usa,
                "3" = g_bra, "4" = g_chn)
    })
    
    output$bar = renderPlot({
        
        switch (input$radioBar, "1" = bar1, "2" = bar2,
                "3" = bar3, "4" = bar4,
                "5" = bar5)
    })
    
    
    output$dunya = renderPlot({
        
        switch (input$radioDunya, "1" = dunya.cizgi.graph, "2" = dunya.alan.graph,
                "3" = aktif.olgu.graph, "4" = olum.graph,
                "5" = yeni.iyilesen.graph, "6" = yeni.olum.graph,
                "7" = iyilesen.graph)
    })
    
    
    output$dunyaOran = renderPlot({
        
        switch (input$radioDunyaOran, "1" = gunluk.oran.graph, "2" = mortalite.dunya,
                "3" = mortalite.dunya.2)
    })
    
    
    output$turkey = renderPlot({
        
        switch (input$radioTurkey, "1" = turkiye.alan.graph, "2" = turkiye.cizgi.graph,
                "3" = mortalite.turkey, "4" = mortalite.turkey2)
    })
    
    output$both = renderPlot({
        switch (input$radioUlkeler, "1"= pgraph)
    })  
    
}
# Run the application 
shinyApp(ui = ui, server = server)
