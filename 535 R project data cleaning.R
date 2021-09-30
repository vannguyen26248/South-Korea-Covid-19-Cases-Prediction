library(dplyr)
library(ggplot2)
library(raster)
library(ggrepel)
library(zoo)
#library(xts)
setwd("C:/Users/lyh_0/OneDrive/010spring 21 All HW/535 Project")

Time <- read.csv("C:/Users/lyh_0/Desktop/535 data/Time.csv")
TimeProvince <- read.csv("C:/Users/lyh_0/Desktop/535 data/TimeProvince.csv")
Weather <- read.csv("C:/Users/lyh_0/Desktop/535 data/Weather.csv")
SearchTrend <- read.csv("C:/Users/lyh_0/Desktop/535 data/SearchTrend.csv")
Policy<- read.csv("C:/Users/lyh_0/Desktop/535 data/Policy.csv")
TimeAge <- read.csv("C:/Users/lyh_0/Desktop/535 data/TimeAge.csv")
TimeGender <- read.csv("C:/Users/lyh_0/Desktop/535 data/TimeGender.csv")

WeatherRanged=Weather[23680:26271,]
SearchTrendranged=SearchTrend[1481:1642,]
#step 1 data visualization
TimeProvince$latitude=ifelse(TimeProvince$province =="Seoul", 37.566953, 
                         ifelse(TimeProvince$province =="Busan", 35.179884, 
                         ifelse(TimeProvince$province =="Gwangju", 35.160467,
                         ifelse(TimeProvince$province =="Incheon", 37.456188,
                         ifelse(TimeProvince$province =="Daejeon", 36.350621,
                         ifelse(TimeProvince$province =="Ulsan", 35.539797,
                         ifelse(TimeProvince$province =="Sejong", 36.480132,
                         ifelse(TimeProvince$province =="Gyeonggi-do", 37.275119,
                         ifelse(TimeProvince$province =="Gangwon-do", 37.885369,
                         ifelse(TimeProvince$province =="Chungcheongbuk-do", 36.63568,
                         ifelse(TimeProvince$province =="Chungcheongnam-do", 36.658976,
                         ifelse(TimeProvince$province =="Jeollabuk-do", 35.820308,
                         ifelse(TimeProvince$province =="Jeollanam-do", 34.816095,
                         ifelse(TimeProvince$province =="Gyeongsangbuk-do", 36.576032,
                         ifelse(TimeProvince$province =="Gyeongsangnam-do", 35.238294,
                         ifelse(TimeProvince$province =="Jeju-do", 33.488936,35.87215))))))))))))))))
# the last else is Daegu

TimeProvince$longitude=ifelse(TimeProvince$province =="Seoul", 126.977977, 
                      ifelse(TimeProvince$province =="Busan", 129.074796, 
                      ifelse(TimeProvince$province =="Gwangju", 126.851392,
                      ifelse(TimeProvince$province =="Incheon", 126.70592,
                      ifelse(TimeProvince$province =="Daejeon", 127.415597,
                      ifelse(TimeProvince$province =="Ulsan", 129.311538,
                      ifelse(TimeProvince$province =="Sejong", 127.289021,
                      ifelse(TimeProvince$province =="Gyeonggi-do", 127.009466,
                      ifelse(TimeProvince$province =="Gangwon-do", 127.729868,
                      ifelse(TimeProvince$province =="Chungcheongbuk-do", 127.491384,
                      ifelse(TimeProvince$province =="Chungcheongnam-do", 126.673318,
                      ifelse(TimeProvince$province =="Jeollabuk-do", 127.108791,
                      ifelse(TimeProvince$province =="Jeollanam-do", 126.463021,
                      ifelse(TimeProvince$province =="Gyeongsangbuk-do", 128.505599,
                      ifelse(TimeProvince$province =="Gyeongsangnam-do", 128.692397,
                      ifelse(TimeProvince$province =="Jeju-do", 126.500423,128.601783))))))))))))))))

# the last else is Daegu

# Total confirmed
TimeProvince$t=as.numeric(as.Date(TimeProvince$date))-18280


TimeProWea=merge(TimeProvince, Weather)
TimeProWeaSea=merge(TimeProWea,SearchTrend)
#This data is consisting 15 provinces with t=162 (total 2430 obs), 
#we remove out  Sejong and Gyeongsangbuk-do because they contain missing data.
TimeProWeaSeaF=filter(TimeProWeaSea,province!="Chungcheongbuk-do") 
QT=filter(TimeProWeaSeaF,province=="Busan")
QQQ=diff(QT$confirmed)
prvovname=c(as.character(TimeProWeaSeaF$province[1:15]))
Pdatanew=as.data.frame(NULL)
for(i in 1:15){
  Pdata=filter(TimeProWeaSeaF,province==prvovname[i])
  Pdata$newcases=matrix(c(0,as.matrix(diff(Pdata$confirmed))),162,1)
  Pdatanew=rbind(Pdatanew,Pdata)
}
TimeProWeaSeadiffF=Pdatanew

Time=Time[-163,]
Time$newcases=matrix(c(0,as.matrix(diff(Time$confirmed))),162,1)
TimeSea=merge(Time,SearchTrend)



# train province seoul test near by Gyeonggi-do and Incheon
# Will take seoul frist, find the time lack of weather and tune for lack.

seouldata=dplyr::filter(TimeProWeaSeadiffF,TimeProWeaSeadiffF$province=="Seoul")
seoulweather=Weather %>% dplyr::filter(Weather$province=="Seoul")
seoulweather14lag=seoulweather[1467:1628,]
seouldata$avg_temp14lag=seoulweather14lag$avg_temp
seouldata$min_temp14lag=seoulweather14lag$min_temp
seouldata$max_temp14lag=seoulweather14lag$max_temp
seouldata$precipitation14lag=seoulweather14lag$precipitation
seouldata$max_wind_speed14lag=seoulweather14lag$max_wind_speed
seouldata$avg_relative_humidity14lag=seoulweather14lag$avg_relative_humidity

#USE MA7 to smooth the new cases
newcases7dayMA=c(rep(NA,6),zoo::rollmean(seouldata$newcases,7))
seouldata$newcases7dayMA=newcases7dayMA

#USE MA14 to smooth the new cases
newcases14dayMA=c(rep(NA,13),zoo::rollmean(seouldata$newcases,14))
seouldata$newcases14dayMA=newcases14dayMA


tempset=dplyr::arrange(Weather,province) %>% dplyr::filter(province=="Seoul")
Policy=dplyr::arrange(Policy,type,start_date,end_date)
#Hand build policy dataset- count the data by type of policy 
clearedpolicy=as.data.frame(tempset$date[1462:1642])
clearedpolicy$Alertcum=0
clearedpolicy$Alertcum[3]=1
clearedpolicy$Alertcum[20]=2
clearedpolicy$Alertcum[28]=3
clearedpolicy$Alertcum[54]=4
#
clearedpolicy$Alertnew=0
clearedpolicy$Alertnew[3]=1
clearedpolicy$Alertnew[20]=1
clearedpolicy$Alertnew[28]=1
clearedpolicy$Alertnew[54]=1
#
clearedpolicy$Immigrationcum=0
clearedpolicy$Immigrationcum[35]=1
clearedpolicy$Immigrationcum[43]=3
clearedpolicy$Immigrationcum[69]=4
clearedpolicy$Immigrationcum[72]=6
clearedpolicy$Immigrationcum[75]=11
clearedpolicy$Immigrationcum[76]=12
clearedpolicy$Immigrationcum[79]=13
clearedpolicy$Immigrationcum[92]=14
clearedpolicy$Immigrationcum[104]=15
#
clearedpolicy$Immigrationnew=0
clearedpolicy$Immigrationnew[35]=1
clearedpolicy$Immigrationnew[43]=2
clearedpolicy$Immigrationnew[69]=1
clearedpolicy$Immigrationnew[72]=2
clearedpolicy$Immigrationnew[75]=5
clearedpolicy$Immigrationnew[76]=1
clearedpolicy$Immigrationnew[79]=1
clearedpolicy$Immigrationnew[92]=1
clearedpolicy$Immigrationnew[104]=1
#
clearedpolicy$Healthcum=0
clearedpolicy$Healthcum[35]=1
clearedpolicy$Healthcum[43]=2
clearedpolicy$Healthcum[58]=5
clearedpolicy$Healthcum[64]=6 #2020-03-04
clearedpolicy$Healthcum[69]=7
clearedpolicy$Healthcum[73]=8
clearedpolicy$Healthcum[149]=9 #2020-05-28
clearedpolicy$Healthcum[166]=8 #2020-06-14 end -1
#
clearedpolicy$Healthnew=0
clearedpolicy$Healthnew[35]=1
clearedpolicy$Healthnew[43]=1
clearedpolicy$Healthnew[58]=3
clearedpolicy$Healthnew[64]=1 #2020-03-04
clearedpolicy$Healthnew[69]=1
clearedpolicy$Healthnew[73]=1
clearedpolicy$Healthnew[149]=1 #2020-05-28
clearedpolicy$Healthnew[166]=-1 #2020-06-14 end -1
#
clearedpolicy$Administrativecum=0
clearedpolicy$Administrativecum[129]=1
clearedpolicy$Administrativecum[137]=2
clearedpolicy$Administrativecum[142]=3
clearedpolicy$Administrativecum[155]=2 # end
clearedpolicy$Administrativecum[159]=1 #end
#
clearedpolicy$Administrativenew=0
clearedpolicy$Administrativenew[129]=1
clearedpolicy$Administrativenew[137]=1
clearedpolicy$Administrativenew[142]=1
clearedpolicy$Administrativenew[155]=-1 # end
clearedpolicy$Administrativenew[159]=-1 #end
#
clearedpolicy$Socialcum=0
clearedpolicy$Socialcum[57]=1
clearedpolicy$Socialcum[111]=0.5
#
clearedpolicy$Socialnew=0
clearedpolicy$Socialnew[57]=1
clearedpolicy$Socialnew[111]=-0.5
#
clearedpolicy$Technologycum=0
clearedpolicy$Technologycum[20]=1
clearedpolicy$Technologycum[43]=2
clearedpolicy$Technologycum[67]=3
clearedpolicy$Technologycum[68]=4 
clearedpolicy$Technologycum[118]=5 
clearedpolicy$Technologycum[162]=6 
#
clearedpolicy$Technologynew=0
clearedpolicy$Technologynew[20]=1
clearedpolicy$Technologynew[43]=1
clearedpolicy$Technologynew[67]=1
clearedpolicy$Technologynew[68]=1 
clearedpolicy$Technologynew[118]=1 
clearedpolicy$Technologynew[162]=1
#
clearedpolicy$Transformationcum=0
clearedpolicy$Transformationcum[147]=1
clearedpolicy$Transformationcum[148]=2
clearedpolicy$Transformationcum[149]=3
clearedpolicy$Transformationcum[163]=2
#
clearedpolicy$Transformationnew=0
clearedpolicy$Transformationnew[147]=1
clearedpolicy$Transformationnew[148]=1
clearedpolicy$Transformationnew[149]=1
clearedpolicy$Transformationnew[163]=-1
#
clearedpolicy$Educationcum=0
clearedpolicy$Educationcum[62]=5
clearedpolicy$Educationcum[97]=0
clearedpolicy$Educationcum[100]=2
clearedpolicy$Educationcum[107]=8
clearedpolicy$Educationcum[111]=10
clearedpolicy$Educationcum[141]=9
clearedpolicy$Educationcum[148]=6
clearedpolicy$Educationcum[155]=2
clearedpolicy$Educationcum[160]=0

#
clearedpolicy$Educationnew=0
clearedpolicy$Educationnew[62]=5
clearedpolicy$Educationnew[97]=-5
clearedpolicy$Educationnew[100]=2
clearedpolicy$Educationnew[107]=6
clearedpolicy$Educationnew[111]=4
clearedpolicy$Educationnew[141]=-1
clearedpolicy$Educationnew[148]=-3
clearedpolicy$Educationnew[155]=-1
clearedpolicy$Educationnew[160]=-2
#
colnames(clearedpolicy)[1]="date"
newseouldata=merge(seouldata,clearedpolicy)
clearedpolicy14lag=clearedpolicy[6:167,]
colnames(clearedpolicy14lag)=paste(colnames(clearedpolicy14lag),"14lag",sep ="")
finalseouldata=cbind(newseouldata,clearedpolicy14lag)

#DATAset for Gyeonggi-do

Gyeonggidodata=dplyr::filter(TimeProWeaSeadiffF,TimeProWeaSeadiffF$province=="Gyeonggi-do")
Gyeonggidoweather=Weather %>% dplyr::filter(Weather$province=="Gyeonggi-do")
Gyeonggidoweather14lag=Gyeonggidoweather[1467:1628,]
Gyeonggidodata$avg_temp14lag=Gyeonggidoweather14lag$avg_temp
Gyeonggidodata$min_temp14lag=Gyeonggidoweather14lag$min_temp
Gyeonggidodata$max_temp14lag=Gyeonggidoweather14lag$max_temp
Gyeonggidodata$precipitation14lag=Gyeonggidoweather14lag$precipitation
Gyeonggidodata$max_wind_speed14lag=Gyeonggidoweather14lag$max_wind_speed
Gyeonggidodata$avg_relative_humidity14lag=Gyeonggidoweather14lag$avg_relative_humidity

#USE MA7 to smooth the new cases
newcases7dayMAGyeonggidodata=c(rep(NA,6),zoo::rollmean(Gyeonggidodata$newcases,7))
Gyeonggidodata$newcases7dayMA=newcases7dayMAGyeonggidodata

#USE MA14 to smooth the new cases
newcases14dayMAGyeonggidodata=c(rep(NA,13),zoo::rollmean(Gyeonggidodata$newcases,14))
Gyeonggidodata$newcases14dayMA=newcases14dayMAGyeonggidodata
# add policy to dataset
newGyeonggidodata=merge(Gyeonggidodata,clearedpolicy)
finalGyeonggidodata=cbind(newGyeonggidodata,clearedpolicy14lag)

#####DATAset for Incheon
Incheondata=dplyr::filter(TimeProWeaSeadiffF,TimeProWeaSeadiffF$province=="Incheon")
Incheonweather=Weather %>% dplyr::filter(Weather$province=="Incheon")
Incheonweather14lag=Incheonweather[1467:1628,]
Incheondata$avg_temp14lag=Incheonweather14lag$avg_temp
Incheondata$min_temp14lag=Incheonweather14lag$min_temp
Incheondata$max_temp14lag=Incheonweather14lag$max_temp
Incheondata$precipitation14lag=Incheonweather14lag$precipitation
Incheondata$max_wind_speed14lag=Incheonweather14lag$max_wind_speed
Incheondata$avg_relative_humidity14lag=Incheonweather14lag$avg_relative_humidity

#USE MA7 to smooth the new cases
newcases7dayMAIncheondata=c(rep(NA,6),zoo::rollmean(Incheondata$newcases,7))
Incheondata$newcases7dayMA=newcases7dayMAIncheondata

#USE MA14 to smooth the new cases
newcases14dayMAIncheondata=c(rep(NA,13),zoo::rollmean(Incheondata$newcases,14))
Incheondata$newcases14dayMA=newcases14dayMAIncheondata
# add policy to dataset
newIncheondata=merge(Incheondata,clearedpolicy)
finalIncheondata=cbind(newIncheondata,clearedpolicy14lag)


#save final data set as csv
setwd("C:/Users/lyh_0/OneDrive/010spring 21 All HW")
write.csv(finalseouldata, file = "535Seouldata.csv", row.names = FALSE)
write.csv(finalGyeonggidodata, file = "535Gyeonggidodata.csv", row.names = FALSE)
write.csv(finalIncheondata, file = "535Incheondata.csv", row.names = FALSE)

# new added
finalseouldata$totalpolicy14lag=rowSums(finalseouldata[,seq(49,63,2)])
