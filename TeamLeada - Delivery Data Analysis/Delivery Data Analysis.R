delivery_data <- read.csv("D:/My Folder/R/TeamLeada - Delivery Data Analysis/delivery_data.csv", stringsAsFactors=FALSE)

attach(delivery_data)
#Creating transactions by zip codes

summary(pickup_zipcode)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  94020   94110   94110   94110   94120   95400     855 

zip_code<- as.data.frame(table(pickup_zipcode))
sum(zip_code$Freq)
# 59341
# There are 855 NA's. Hence, 59341 + 855 = 60196

max(zip_code$Freq)
#8679

which(zip_code == 8679, arr.ind=TRUE)
#      row col
# [1,]  10   2

#Removing zip codes with total count of deliveries less than 200

table(zip_code$Freq > 200)
# FALSE  TRUE 
# 11    23 

zip_code <- zip_code[zip_code$Freq > 200 ,  ] 


#Frequency transactions by dollar amount


# rm(new_data)
freq_data <- as.data.frame(zip_code$pickup_zipcode)
freq_data [, 2] <- 0
freq_data [, 3] <- 0
freq_data [, 4] <- 0

colnames (freq_data)[1] <- "Zip_Code"
colnames (freq_data)[2] <- "Less_Than_60"
colnames (freq_data)[3] <- "Btw_60_120"
colnames (freq_data)[4] <- "Above_120"

#zip<-zip_code[, 1]

#delivery_data[is.na(delivery_data)] <- 1
#zip_code[is.na(zip_code)] <- 1

#k=1
#for(i in 1:nrow(delivery_data))
#{
#  for(j in 1:nrow(zip_code))
#  {
#    if(delivery_data$pickup_zipcode[i] == zip_code$pickup_zipcode[j])
#    {
#      new_data[k] <- delivery_data[i, ]
#      k<-k+1
#      break
#    }
#  }
#}
#new_data <- subset(delivery_data, delivery_data$pickup_zipcode == zip)

#new_data <- subset(delivery_data, pickup_zipcode == zip_code$pickup_zipcode)

install.packages("foreach")
library(foreach)

x<- freq_data$Zip_Code

count=0

for(i in 1:nrow(delivery_data))
{
  if(pickup_zipcode[i] %in% zip_code$pickup_zipcode )
  {
   new_data[count,] <-  delivery_data[i, ] 
   count<-count+1
  }
}
#x<- unique(new_data$pickup_zipcode)

#count=0
#for(i in 1:nrow(new_data))
#if(new_data$pickup_zipcode[i] == 1)
#{
#  count=count+1
  
#}

#Filling up the columns in freq_data from newdata

library(psych)
describe(new_data$pickup_zipcode)


#  vars    n     mean   sd median  trimmed  mad   min   max range skew kurtosis   se
# 1    1 2557 94112.97 8.68  94110 94112.14 7.41 94102 94158    56 1.36     3.75 0.17

#Sorting the array according to the zip code

new_data<-new_data[order(new_data$pickup_zipcode), ]

freq_table <- as.data.frame(table(new_data$pickup_zipcode))
freq_table <- freq_table[-1]

count = 1;
for(i in 1: nrow(freq_data))
{
  current_zip <- freq_table[i,]
  for(j in 1:current_zip)
  {
      if(new_data$purchase_price[count] < 60)
        freq_data$Less_Than_60[i] <- freq_data$Less_Than_60[i] + 1
  
      else if(new_data$purchase_price[count] > 60 && new_data$purchase_price[count] < 120)
        freq_data$Btw_60_120[i] <- freq_data$Btw_60_120[i] + 1
  
      else
        freq_data$Above_120[i]<-freq_data$Above_120[i] +1
      count <- count +1
  }
  
  
}
