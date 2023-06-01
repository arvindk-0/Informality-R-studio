library("readxl")
library(tidyverse)
library(ggplot2)
library(plyr)
library(gridExtra)
library(dplyr)


sdg831 = SDG_8_3_1_
spec(sdg831)


##NONAGRICULTURE
A1 = filter(sdg831, classif1.label == "Economic activity (Agriculture, Non-Agriculture): Non-agriculture")
A1India = filter(A1, ref_area.label == "India" )
A1Pak = filter(A1, ref_area.label == "Pakistan" )
A1Thai = filter(A1, ref_area.label == "Thailand" )
A1Viet = filter(A1, ref_area.label == "Viet Nam" )
ggplot(data = A1India, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("INDIA")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A1Pak, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("PAKISTAN")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A1Thai, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("THAILAND")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A1Viet, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("VIETNAM")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point() 



##AGRICULTURE
A2 = filter(sdg831, classif1.label == "Economic activity (Agriculture, Non-Agriculture): Agriculture")
A2India = filter(A2, ref_area.label == "India" )
A2Bang = filter(A2, ref_area.label == "Bangladesh" )
A2Pak = filter(A2, ref_area.label == "Pakistan")
A2Thai = filter(A2, ref_area.label == "Thailand")
A2Viet = filter(A2, ref_area.label == "Viet Nam")

ggplot(data = A2India, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("INDIA")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A2Pak, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("PAKISTAN")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A2Thai, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("THAILAND")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()
ggplot(data = A2Viet, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  ggtitle("VIETNAM")+
  xlab("Year") + ylab("Percentage of Agricultural Informality")+
  geom_point()

##TOTAL
A3 = filter(sdg831, classif1.label == "Economic activity (Agriculture, Non-Agriculture): Total")
A3India = filter(A3, ref_area.label == "India" )
A3Thai = filter(A3, ref_area.label == "Thailand" )
A3Viet = filter(A3, ref_area.label == "Viet Nam" )
ggplot(data = A3India, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  geom_point()
ggplot(data = A3Thai, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  geom_point()
ggplot(data = A3Viet, mapping = aes (x = time, y= obs_value, colour = sex.label,shape = sex.label)) +
  geom_point()



##World region comparison
##fliters are: Total informalitty, sex = total 
A4 = filter(sdg831, classif1.label == "Economic activity (Agriculture, Non-Agriculture): Total")
A5 = filter(A4, sex.label == "Sex: Total" )
A5india = filter(A5, ref_area.label == "India" ) ##lower middle income
A5Sweden = filter(A5, ref_area.label == "Sweden" ) ##high income
A5Thai = filter(A5, ref_area.label == "Thailand" ) ##upper middle income
A5Afg = filter(A5, ref_area.label == "Afghanistan" ) ##lower income
ggplot(data = A5india, mapping = aes (x = time, y= obs_value)) +
  ggtitle("INDIA (Lower middle income)")+
  xlab("Year") + ylab("Percentage of Total Informality")+
  geom_point()
ggplot(data = A5Sweden, mapping = aes (x = time, y= obs_value)) +
  ggtitle("Sweden (High income)")+
  xlab("Year") + ylab("Percentage of Total Informality")+
  geom_point()
ggplot(data = A5Thai, mapping = aes (x = time, y= obs_value)) +
  ggtitle("Thailand (Upper Middle income)")+
  xlab("Year") + ylab("Percentage of Total Informality")+
  geom_point()
ggplot(data = A5Afg, mapping = aes (x = time, y= obs_value)) +
  ggtitle("Afghanistan (Lower income)")+
  xlab("Year") + ylab("Percentage of Total Informality")+
  geom_point()



'
ggplot( data = sdg831, mapping = aes(x= ref_area.label, y=obs_value, colour = classif1.label)) +
geom_point()


op = filter(sdg_final_Sheet1, sex.label == "Sex: Female")
ggplot( data = op, mapping = aes(x= ref_area.label, y=obs_value, colour = classif1.label, fill = time)) +
  geom_point()


p1 = filter(sdg831, ref_area.label == "India")

p12 = filter(p1, sex.label == "Sex: Female")

ggplot( data = p12, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()

p2 = filter(SDG_8_3_1_, ref_area.label=="Sweden")
p22 = filter(p2, sex.label == "Sex: Female")
ggplot( data = p22, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()


p3 = filter(SDG_8_3_1_, ref_area.label=="Thailand")
p32 = filter(p3, sex.label == "Sex: Female")
ggplot( data = p32, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()

p4 = filter(SDG_8_3_1_, ref_area.label=="Afghanistan")
p42 = filter(p4, sex.label == "Sex: Female")
ggplot( data = p42, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()


p5 = filter(SDG_8_3_1_, ref_area.label=="Sri Lanka")
p52 = filter(p5, sex.label == "Sex: Female")
ggplot( data = p52, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()


p6 = filter(SDG_8_3_1_, ref_area.label=="Pakistan")
p62 = filter(p5, sex.label == "Sex: Female")
ggplot( data = p62, mapping = aes(x= time, y=obs_value, colour = classif1.label)) +
  geom_point()
