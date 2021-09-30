source("C:/Users/lyh_0/OneDrive/010spring 21 All HW/535 Project/535 R project data cleaning.r")


skorea <- getData("GADM", country = "South Korea", level = 2)
skorea <- fortify(skorea)

#Noted: t=1 means 1/20/2020 to t=163 means 6/30/2020
# Confirmed by time


TimeProvinceChange=TimeProvince[TimeProvince$t==1,]
P1=ggplot() +  
  geom_map(data = skorea, map = skorea, aes(x = long, y = lat, map_id = id, group = group), fill = NA, colour = "black") + 
  geom_point(data = TimeProvinceChange, aes(x = longitude, y = latitude, size = confirmed,fill =factor(province),colour = factor(province)), alpha = .5) + 
  scale_size(range = c(1, 30))+ggtitle(paste("Cumulative confirmed COVID-19 cases_Time=", 1, sep = ""))+ 
  geom_text(data=TimeProvinceChange,aes(x = longitude, y = latitude,label=province))+
  theme(legend.key.size = unit(0.3, 'cm'),legend.position = "none")

TimeProvinceChange=TimeProvince[TimeProvince$t==50,]
P2=ggplot() +  
  geom_map(data = skorea, map = skorea, aes(x = long, y = lat, map_id = id, group = group), fill = NA, colour = "black") + 
  geom_point(data = TimeProvinceChange, aes(x = longitude, y = latitude, size = confirmed,fill =factor(province),colour = factor(province)), alpha = .5) + 
  scale_size(range = c(1, 30))+ggtitle(paste("Cumulative confirmed COVID-19 cases_Time=", 50, sep = ""))+ 
  geom_text(data=TimeProvinceChange,aes(x = longitude, y = latitude,label=province))+
  theme(legend.key.size = unit(0.3, 'cm'),legend.position = "none")


TimeProvinceChange=TimeProvince[TimeProvince$t==100,]
P3=ggplot() +  
  geom_map(data = skorea, map = skorea, aes(x = long, y = lat, map_id = id, group = group), fill = NA, colour = "black") + 
  geom_point(data = TimeProvinceChange, aes(x = longitude, y = latitude, size = confirmed,fill =factor(province),colour = factor(province)), alpha = .5) + 
  scale_size(range = c(1, 30))+ggtitle(paste("Cumulative confirmed COVID-19 cases_Time=", 100, sep = ""))+ 
  geom_text(data=TimeProvinceChange,aes(x = longitude, y = latitude,label=province))+
  theme(legend.key.size = unit(0.3, 'cm'),legend.position = "none")

TimeProvinceChange=TimeProvince[TimeProvince$t==162,]
P4=ggplot() +  
  geom_map(data = skorea, map = skorea, aes(x = long, y = lat, map_id = id, group = group), fill = NA, colour = "black") + 
  geom_point(data = TimeProvinceChange, aes(x = longitude, y = latitude, size = confirmed,fill =factor(province),colour = factor(province)), alpha = .5) + 
  scale_size(range = c(1, 30))+ggtitle(paste("Cumulative confirmed COVID-19 cases_Time=", 162, sep = ""))+ 
  geom_text(data=TimeProvinceChange,aes(x = longitude, y = latitude,label=province))+
  theme(legend.key.size = unit(0.3, 'cm'),legend.position = "none")



# Cumulative confirmed COVID-19 cases by SK 

P5=ggplot() +
  geom_line(data = Time, aes(x = date, y = confirmed,group=1))+
  labs(x = "Date", y = "Cumulative confirmed")+
  ggtitle("Cumulative confirmed COVID-19 cases in SK")+
  theme(plot.title = element_text(hjust = 0.5))

# Cumulative confirmed COVID-19 cases by Province 
P6=ggplot() +
  geom_line(data = TimeProvince, aes(x = date, y = confirmed,group=province,colour=province))+
  labs(x = "Date", y = "Cumulative confirmed")+
  ggtitle("Cumulative confirmed COVID-19 cases by Province ")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggrepel::geom_text_repel(data=TimeProvinceChange,aes(x = date, y = confirmed,label=province))


# new confirmed COVID-19 cases by Province 
P7=ggplot() +
  geom_line(data = TimeProWeaSeadiffF, aes(x = date, y = newcases,group=province,colour=province))+
  labs(x = "Date", y = "New confirmed")+
  ggtitle("New confirmed COVID-19 cases by Province ")+
  theme(plot.title = element_text(hjust = 0.5))

# new confirmed COVID-19 cases in SK with search trend coronavirus
P8=ggplot() +
  geom_line(data = TimeSea, aes(x = date, y = newcases,group=1))+
  geom_line(data = TimeSea, aes(x = date, y = coronavirus*10,group=1,color="coronavirus"))+
  labs(x = "Date", y = "New confirmed")+
  ggtitle("coronavirus")+
  theme(plot.title = element_text(hjust = 0.5))

# new confirmed COVID-19 cases in SK with search trend cold
P9=ggplot() +
  geom_line(data = TimeSea, aes(x = date, y = newcases,group=1))+
  geom_line(data = TimeSea, aes(x = date, y = cold*120,group=1,color="cold"))+
  labs(x = "Date", y = "New confirmed")+
  ggtitle("cold")+
  theme(plot.title = element_text(hjust = 0.5))

# new confirmed COVID-19 cases in SK with search trend pneumonia
P10=ggplot() +
  geom_line(data = TimeSea, aes(x = date, y = newcases,group=1))+
  geom_line(data = TimeSea, aes(x = date, y = pneumonia*120,group=1,color="pneumonia"))+
  labs(x = "Date", y = "New confirmed")+
  ggtitle("pneumonia")+
  theme(plot.title = element_text(hjust = 0.5))

# new confirmed COVID-19 cases in SK with search trend flu
P11=ggplot() +
  geom_line(data = TimeSea, aes(x = date, y = newcases,group=1))+
  geom_line(data = TimeSea, aes(x = date, y = flu*120,group=1,color="flu"))+
  labs(x = "Date", y = "New confirmed")+
  ggtitle("flu")+
  theme(plot.title = element_text(hjust = 0.5))


# Seoul new confirmed COVID-19 cases vs date
P12=ggplot() +
  geom_line(data = seouldata, aes(x = t, y = newcases,color = "newcases",group=1))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases")+
  theme(plot.title = element_text(hjust = 0.5))



# Seoul new confirmed COVID-19 cases vs raw weather 
P13=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = max_temp,group=1,color = "max_temp"))+
  geom_line(data = seouldata, aes(x = date, y = avg_relative_humidity,group=1,color = "avg_relative_humidity"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases vs raw weather  ")+
  theme(plot.title = element_text(hjust = 0.5))


# Seoul new confirmed COVID-19 cases vs raw weather max_temp
P14=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = max_temp,group=1,color = "max_temp"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("max_temp ")+
  theme(plot.title = element_text(hjust = 0.5))

# Seoul new confirmed COVID-19 cases vs raw weather 
P15=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = avg_relative_humidity-40,group=1,color = "avg_relative_humidity"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("avg_relative_humidity ")+
  theme(plot.title = element_text(hjust = 0.5))

# Seoul new confirmed COVID-19 cases vs lagged max_temp weather 
P16=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = max_temp14lag,group=1,color = "max_temp14lag"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("max_temp14lag ")+
  theme(plot.title = element_text(hjust = 0.5))
# Seoul new confirmed COVID-19 cases vs lagged avg_relative_humidity weather 
P17=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = avg_relative_humidity14lag-40,group=1,color = "avg_relative_humidity14lag"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("avg_relative_humidity14lag ")+
  theme(plot.title = element_text(hjust = 0.5))


#USE MA7 to smooth the new cases
newcases7dayMA=c(rep(NA,6),zoo::rollmean(seouldata$newcases,7))
seouldata$newcases7dayMA=newcases7dayMA

#USE MA14 to smooth the new cases
newcases14dayMA=c(rep(NA,13),zoo::rollmean(seouldata$newcases,14))
seouldata$newcases14dayMA=newcases14dayMA

# Seoul new confirmed COVID-19 cases vs smoothed new confirmed COVID-19 cases 
P18=ggplot() +
  geom_line(data = seouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = seouldata, aes(x = date, y = newcases7dayMA,group=1,color = "newcases7dayMA"))+
  geom_line(data = seouldata, aes(x = date, y = newcases14dayMA,group=1,color = "newcases14dayMA"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases vs smoothed ")+
  theme(plot.title = element_text(hjust = 0.5))


# Seoul new confirmed COVID-19 cases vs Educationnew
P19=ggplot() +
  geom_line(data = finalseouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = finalseouldata, aes(x = date, y = Educationnew,group=1,color = "Educationnew"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases vs Educationnew ")+
  theme(plot.title = element_text(hjust = 0.5))

# Seoul new confirmed COVID-19 cases vs lagged Educationnew14lag
P20=ggplot() +
  geom_line(data = finalseouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = finalseouldata, aes(x = date, y = Educationnew14lag,group=1,color = "Educationnew14lag"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases vs Educationnew14lag ")+
  theme(plot.title = element_text(hjust = 0.5))

# Seoul new confirmed COVID-19 cases vs lagged totalpolicy14lag
finalseouldata$totalpolicy14lag=rowSums(finalseouldata[,48:63])
P21=ggplot() +
  geom_line(data = finalseouldata, aes(x = date, y = newcases,color = "newcases",group=1))+
  geom_line(data = finalseouldata, aes(x = date, y = totalpolicy14lag,group=1,color = "new_totalpolicy14lag"))+
  labs(x = "Date", y = "New confirmed",color = "Legend")+
  ggtitle("Seoul new confirmed COVID-19 cases vs new totalpolicy14lag ")+
  theme(plot.title = element_text(hjust = 0.5))
