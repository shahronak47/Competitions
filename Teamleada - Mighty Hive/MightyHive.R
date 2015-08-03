reservation <- read.csv("D:/My Folder/R/Teamleada - Mighty Hive/Reservation_Data_Seed.csv", stringsAsFactors=FALSE)

abandoned <- read.csv("D:/My Folder/R/Teamleada - Mighty Hive/Abandoned_Data_Seed.csv", stringsAsFactors=FALSE)

#Check test label data in reservation

table(reservation$Test_Control)
# control    test 
#  2086   18728 

table(abandoned$Test_Control)
# control    test 
# 4176    4266 

#Caller_ID seems to be uniquely generated for every call but lets confirm it

count <- 0

foreach(i= 1: nrow(abandoned)) %do%
{
  foreach(j=1: nrow(reservation)) %do%
 {
  if(abandoned$Caller_ID[i] == reservation$Caller_ID[j])
  {
    count <- count + 1
  }
 }
}


#Lets check the email address, so that we get to know how many of them are repeat callers
#count <- 0
#foreach(i= 1: nrow(abandoned)) %do%
#{
#  foreach(j=1: nrow(reservation)) %do%
#{
#  if(abandoned$Email[i] == reservation$Email[j])
#  {
#    count <- count + 1
#  }
#}
#}

myfile <- abandoned[order(abandoned$Email), ]
table (abandoned$Email == "")
# FALSE  TRUE 
# 1030  7412 
#There are 7412 values which are blank so we cant use email address

table (abandoned$First_Name == "")
#FALSE  TRUE 
#8252   190

table (abandoned$Incoming_Phone == "")
#FALSE  TRUE 
#7262  1180

table (abandoned$Contact_Phone == "")

#FALSE  TRUE 
#8403    39 

#foreach(i=1:nrow(abandoned))
#{
#  if(reservation$Caller_ID[79] == abandoned$Caller_ID[i])
#  {
#    print("Success")
#    break
#  }
#  else 
#    print("Failure")
#}



#Lets separate the date from the timestamp

abandoned$Date <- substring(abandoned$Session,1,10)
#myfile <- abandoned$Date


#myfile1 <-  gsub('[.]', '-', myfile)
#The dates are in format 28.3.2014 changing it to 28-3-2014

abandoned$Date <- gsub('[.]', '-', abandoned$Date)

#Date is still a character class, changing it to date class

abandoned$Date <- as.Date(abandoned$Date)



reservation$Date <- substring(reservation$Session,1,10)

reservation$Date <- gsub('[.]', '-', reservation$Date)

reservation$Date <- as.Date(reservation$Date)

summary(abandoned$Date)
#  Min.       1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2014-01-06" "2014-01-13" "2014-01-20" "2014-01-20" "2014-01-28" "2014-02-04" 

summary(reservation$Date)
# Min.          1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2014-02-04" "2014-02-22" "2014-03-12" "2014-03-13" "2014-04-03" "2014-04-22" 


table (abandoned$Contact_Phone == "")

#FALSE  TRUE 
#8403    39 

table(reservation$Contact_Phone == "")

abandoned<-abandoned[order(abandoned$Contact_Phone), ]

#Lets concentrate on only the test dataset

table(abandoned$Test_Control)

# control    test 
#  4176    4266 

table(reservation$Test_Control)

# control    test 
#  2086   18728 

abfile <- abandoned[which(abandoned$Test_Control == "test"), ]
resfile <- reservation[which(reservation$Test_Control == "test"), ]


table (abfile$Email == "")
# FALSE  TRUE 
#  563  3703


table (abfile$First_Name == "")
#FALSE  TRUE 
#4180    86 

table (abfile$Incoming_Phone == "")
#FALSE  TRUE 
#3672   594 

table (abfile$Contact_Phone == "")

#FALSE  TRUE 
# 4245    21 

table(resfile$Contact_Phone == "")

# FALSE  TRUE 
#  8800  9928 

table(resfile$Incoming_Phone == "")

# FALSE  TRUE 
# 17473  1255 

count<- 0

#Sorting both the data frames

abfile<-abfile[order(abfile$Contact_Phone), ]
resfile<-resfile[order(resfile$Contact_Phone), ]

c<-0

foreach(i=1:nrow(abfile)) %do%
{
if(abfile$Contact_Phone[i] == "" & abfile$Incoming_Phone[i] == "")
  count <- count + 1
 else
   c <- c+ 1
}

# c = 4266

foreach(i=1:nrow(resfile)) %do%
{
  if(resfile$Contact_Phone[i] == "" & resfile$Incoming_Phone[i] == "")
    count <- count + 1
  else
    c <- c+ 1
}
# c = 18728

# Hence, we can use contact phone or incoming phone number to check duplicate names


abfile <- abfile[order(abfile$Incoming_Phone), ]
resfile <- resfile[order(resfile$Incoming_Phone), ]


table (abfile$Incoming_Phone == "")
#FALSE  TRUE 
#3672   594 

table(resfile$Incoming_Phone == "")

# FALSE  TRUE 
# 17473  1255 

count <- 0
same <- c(1,2)

foreach(i = 1: nrow(abfile)) %do%
{
  if(abfile$Incoming_Phone[i] == "")
  {
    foreach(j=1: nrow(resfile)) %do%
   {
    if(abfile$Contact_Phone[i] == resfile$Contact_Phone[j])
    {
      count <- count + 1
      same[count] = abfile$Contact_Phone[i] 
      
    }
   }
  }
  else
  {
    foreach(j = 1256:nrow(resfile)) %do%
  {
    if(abfile$Incoming_Phone[i] == resfile$Incoming_Phone[j])
    {
      count <- count +1
      same[count] = abfile$Incoming_Phone[i] 
    #else
     # if(resfile$Incoming_Phone [i] > abfile$Incoming_Phone [j])
    }   
  }
}
}

#It can be seen that some of the numbers are repeated

abfile[which((abfile$Incoming_Phone) == "(802)-665-5195"),] #6 values
abfile[which((abfile$Contact_Phone) == "(802)-665-5195"),] # 2 values

resfile[which((resfile$Incoming_Phone) == "(802)-665-5195"),] #3
resfile[which((resfile$Contact_Phone) == "(802)-665-5195"),] #0

#So it means that out of 6 times the "(802)-665-5195" person has called, he has placed the order 3 times


#Lets check for all the repeated values
n_check <- data.frame(table(abfile$Incoming_Phone))
check <- n_check[n_check$Freq > 1, ]
sum(check$Freq)
#[1] 714
# nrow(n_check) + sum(check$Freq) - nrow(check) = nrow(abfile)
# 3607 + 714 - 55 = 4266

#cross-checking
sum(table(unique(m_check$Var1)))
# [1] 16450
sum(table(unique(n_check$Var1)))
# [1] 3607
#So check now has all the numbers with their frequency in abfile
#Now lets check if this frequency matches with the res file

m_check <- data.frame(table(resfile$Incoming_Phone))
check1 <- m_check[m_check$Freq > 1, ]
sum(check1$Freq)
#[1] 2958

# nrow(m_check) + sum(check1$Freq) - nrow(check1) = nrow(resfile)
# 16450 + 2958 - 680 = 18728 

colnames(check)[1] <- "Phone_number"
colnames(check1)[1] <- "Phone_number"

check$Phone_number <- gsub('[(]', '', check$Phone_number)
check$Phone_number <- gsub('[)]', '', check$Phone_number)
check$Phone_number <- gsub('-', '', check$Phone_number)

check1$Phone_number <- gsub('[(]', '', check1$Phone_number)
check1$Phone_number <- gsub('[)]', '', check1$Phone_number)
check1$Phone_number <- gsub('-', '', check1$Phone_number)




count <- 1
my <- c(0, 1)

check$Phone_number <- as.numeric(as.character(check$Phone_number))
check1$Phone_number <- as.numeric(as.character((check1$Phone_number)))

#Replacing NA with 0
check$Phone_number[1] <- 0
check1$Phone_number[1] <- 0

for(i in 1: nrow(check)) 
{
  for(j in 1:nrow(check1))
 {
    if(check$Phone_number[i] == check1$Phone_number[j])
    {
      my[count] <- check$Phone_number[i]
      count <- count + 1
    }
  }
}

#check$Var1 <- as.character(check$Var1)
#check1$Var1 <- as.factor(check1$Var1)
#which(abfile$Incoming_Phone == "")
