library(readxl)
library(ggplot2)

AF <- read_excel("C:/Users/Skalli/Desktop/R/Excel/Air_France.xls")


#creating profit column (Amount - Total Cost)
AF$profit <- AF$Amount - AF$`Total Cost`

#subsetting profit > 0
profit_AF <- AF[which( AF$profit > 0), ]

#subsetting profit > 0
not_profitable_profit_AF <- AF[which( AF$profit <= 0), ]
nrow(not_profitable_profit_AF)
# 4186

profitable_AF <- AF[which( AF$profit > 0), ]
nrow(profitable_AF)
# 324 

#check the 80% of revenue made
quantile(AF$profit, 0.8)


#orderby
AF <- AF[order(-AF$profit),]


corr_AF <- profit_AF[,c(24,19,16,17)]

corr_AF
cor(corr_AF)

#result = no correlation in the AF that isnt subset per publisher

profit_AF$profit_quant <- NA

a <- quantile(profit_AF$profit, probs = 0.8)

for(i in 1:nrow(profit_AF)){
  if(profit_AF$profit[i] > a){
    profit_AF$profit_quant[i] <- 1
  }
  else{profit_AF$profit_quant[i] <- 0}
}



bigprofit_AF <- profit_AF[which(profit_AF$profit_quant == 1),]

###############################
sum_campaign_profit <- sum(bigprofit_AF$profit[bigprofit_AF$Campaign == "Air France Branded"])

sum_campaign_profit

sum_campaign_loss <- sum(bigprofit_AF$`Total Cost`[bigprofit_AF$Campaign == "Air France Branded"])

sum_campaign_loss

sum_campaign_profit - sum_campaign_loss

# 2,164,383 

#####################

sum_campaign_profit_2 <- sum(bigprofit_AF$profit[bigprofit_AF$Campaign == "Air France Global Campaign"])

sum_campaign_profit_2

sum_campaign_loss_2 <- sum(bigprofit_AF$`Total Cost`[bigprofit_AF$Campaign == "Air France Global Campaign"])

sum_campaign_loss_2

sum_campaign_profit_2 - sum_campaign_loss_2

# 353,284.4

####################

sum_campaign_profit_3 <- sum(bigprofit_AF$profit[bigprofit_AF$Campaign == "Paris & France Terms"])

sum_campaign_profit_3

sum_campaign_loss_3 <- sum(bigprofit_AF$`Total Cost`[bigprofit_AF$Campaign == "Paris & France Terms"])

sum_campaign_loss_3 

sum_campaign_profit_3 - sum_campaign_loss_2

# score : -1,921.562

#Create subsets based on Campaign name
Air_France_French_Des <-bigprofit_AF[which(bigprofit_AF$Campaign=='Air France Brand & French Destinations'),]
Air_France_Branded <-bigprofit_AF[which(bigprofit_AF$Campaign=='Air France Branded'),]
Air_France_Global <-bigprofit_AF[which(bigprofit_AF$Campaign=='Air France Global Campaign'),]
Geo_Targeted_Houston <-bigprofit_AF[which(bigprofit_AF$Campaign=='Geo Targeted Houston'),]
Geo_Targeted_NY <-bigprofit_AF[which(bigprofit_AF$Campaign=='Geo Targeted New York'),]
Paris_France_Terms <-bigprofit_AF[which(bigprofit_AF$Campaign=='Paris & France Terms'),]
Unassigned <-bigprofit_AF[which(bigprofit_AF$Campaign=='Unassigned'),]
Western_Europe_Des <-bigprofit_AF[which(bigprofit_AF$Campaign=='Western Europe Destinations'),]

Air_France_French_Des_amount <- sum(Air_France_French_Des$Amount) 
Air_France_Branded_amount <- sum(Air_France_Branded$Amount) 
Air_France_Global_amount <- sum(Air_France_Global$Amount) 
Geo_Targeted_Houston_amount <- sum(Geo_Targeted_Houston$Amount) 
Geo_Targeted_NY_amount <- sum(Geo_Targeted_NY$Amount) 
Paris_France_Terms_amount <- sum(Paris_France_Terms$Amount) 
Unassigned_amount <- sum(Unassigned$Amount) 
Western_Europe_Des_amount <- sum(Western_Europe_Des$Amount) 

Air_France_French_Des_Cost <- sum(Air_France_French_Des$`Total Cost`) 
Air_France_Branded_Cost <- sum(Air_France_Branded$`Total Cost`) 
Air_France_Global_Cost <- sum(Air_France_Global$`Total Cost`) 
Geo_Targeted_Houston_Cost <- sum(Geo_Targeted_Houston$`Total Cost`) 
Geo_Targeted_NY_Cost <- sum(Geo_Targeted_NY$`Total Cost`) 
Paris_France_Terms_Cost <- sum(Paris_France_Terms$`Total Cost`) 
Unassigned_Cost <- sum(Unassigned$`Total Cost`) 
Western_Europe_Des_Cost <- sum(Western_Europe_Des$`Total Cost`) 



# Creating a dataframe with two columns: "name" and "age"
Campaign_frame <- data.frame(Campaign_name = c("Air_France_French_Des", "Air_France_Branded", "Air_France_Global", "Geo_Targeted_Houston", "Geo_Targeted_NY", "Paris_France_Terms", "Unassigned", "Western_Europe_Des"),
                             Amount = c(Air_France_French_Des_amount, Air_France_Branded_amount, Air_France_Global_amount, Geo_Targeted_Houston_amount, Geo_Targeted_NY_amount, Paris_France_Terms_amount, Unassigned_amount, Western_Europe_Des_amount),
                             Cost = c(Air_France_French_Des_Cost, Air_France_Branded_Cost, Air_France_Global_Cost, Geo_Targeted_Houston_Cost, Geo_Targeted_NY_Cost, Paris_France_Terms_Cost, Unassigned_Cost, Western_Europe_Des_Cost))

Campaign_frame$profit <- Campaign_frame$Amount - Campaign_frame$Cost
Campaign_frame$cost_ratio <- Campaign_frame$Cost / Campaign_frame$Amount
Campaign_frame$profit_margin <- Campaign_frame$profit / Campaign_frame$Cost


Campaign_frame$profit_margin <- paste0(round(Campaign_frame$profit_margin * 100, 2), "%")



#---------------------------------kayak AF---------------------------------
kayak_AF <- read_excel("C:/Users/Skalli/Desktop/R/Excel/Air_France.xls", sheet = 3)

#labelling
names(kayak_AF)[1] <- "Search Engine"
names(kayak_AF)[2] <- "Clicks "
names(kayak_AF)[3] <- "Media_Cost"
names(kayak_AF)[4] <- "Total Bookings"
names(kayak_AF)[5] <- "Avg Ticket"
names(kayak_AF)[6] <- "Total Revenue"
names(kayak_AF)[7] <- "Net_Revenue"


kayak_AF <- kayak_AF[3,]


#classify per publisher
yahoo_AF <- AF[which(AF$`Publisher Name` ==  'Yahoo - US'),]
google_AF <- AF[which(AF$`Publisher Name` == 'Google - US'),]
msn_AF <- AF[which(AF$`Publisher Name` == 'MSN - US'),]
overture_AF <- AF[which(AF$`Publisher Name` == 'Overture - US'),]
google_global_AF <- AF[which(AF$`Publisher Name` == 'Google - Global'),]
msn_global_AF <- AF[which(AF$`Publisher Name` == 'MSN - Global'),]
overture_global_AF <- AF[which(AF$`Publisher Name` == 'Overture - Global'),]


#subset kayak_df to only wanted columns
kayak_AF <- kayak_AF[c(1:4, 7)]

#creating values to generate the AFframe
google <- "Google US"
msn_global <- "MSN Global"
yahoo <- "Yahoo US"
msn <- "MSN US"
overture <- "Overture US"
google_global <- "Google Global"

overture_global <- "Overture Global"


google_clicks <- sum(google_AF$Clicks) 
yahoo_clicks <- sum(yahoo_AF$Clicks) 
msn_clicks <- sum(msn_AF$Clicks) 
overture_clicks <- sum(overture_AF$Clicks) 
google_global_clicks <- sum(google_global_AF$Clicks) 
msn_global_clicks <- sum(msn_global_AF$Clicks) 
overture_global_clicks <- sum(overture_global_AF$Clicks) 


google_total_cost <- sum(google_AF$`Total Cost`) 
yahoo_total_cost <- sum(yahoo_AF$`Total Cost`) 
msn_total_cost <- sum(msn_AF$`Total Cost`) 
overture_total_cost <- sum(overture_AF$`Total Cost`) 
google_global_total_cost <- sum(google_global_AF$`Total Cost`) 
msn_global_total_cost <- sum(msn_global_AF$`Total Cost`) 
overture_global_total_cost <- sum(overture_global_AF$`Total Cost`) 

google_clicks_Total_Volume_of_Bookings <- sum(google_AF$`Total Volume of Bookings`) 
yahoo_Total_Volume_of_Bookings <- sum(yahoo_AF$`Total Volume of Bookings`) 
msn_Total_Volume_of_Bookings <- sum(msn_AF$`Total Volume of Bookings`) 
overture_Total_Volume_of_Bookings <- sum(overture_AF$`Total Volume of Bookings`) 
google_global_Total_Volume_of_Bookings <- sum(google_global_AF$`Total Volume of Bookings`) 
msn_global_Total_Volume_of_Bookings <- sum(msn_global_AF$`Total Volume of Bookings`) 
overture_global_Total_Volume_of_Bookings <- sum(overture_global_AF$`Total Volume of Bookings`) 

google_amount <- sum(google_AF$Amount) 
yahoo_amount <- sum(yahoo_AF$Amount) 
msn_amount <- sum(msn_AF$Amount) 
overture_amount <- sum(overture_AF$Amount) 
google_global_amount <- sum(google_global_AF$Amount) 
msn_global_amount <- sum(msn_global_AF$Amount) 
overture_global_amount <- sum(overture_global_AF$Amount) 

#appending kayak_AF 

kayak_AF <- rbind(kayak_AF, c(google,google_clicks, google_total_cost, google_clicks_Total_Volume_of_Bookings, google_amount))
kayak_AF <- rbind(kayak_AF, c(yahoo, yahoo_clicks, yahoo_total_cost, yahoo_Total_Volume_of_Bookings, yahoo_amount))
kayak_AF <- rbind(kayak_AF, c(msn, msn_clicks, msn_total_cost, msn_Total_Volume_of_Bookings, msn_amount))
kayak_AF <- rbind(kayak_AF, c(overture, overture_clicks, overture_total_cost, overture_Total_Volume_of_Bookings, overture_amount))
kayak_AF <- rbind(kayak_AF, c(google_global, google_global_clicks, google_global_total_cost, google_global_Total_Volume_of_Bookings, google_global_amount))
kayak_AF <- rbind(kayak_AF, c(msn_global, msn_global_clicks, msn_global_total_cost, msn_global_Total_Volume_of_Bookings, msn_global_amount))
kayak_AF <- rbind(kayak_AF, c(overture_global, overture_global_clicks, overture_global_total_cost, overture_global_Total_Volume_of_Bookings, overture_global_amount))


#Subset analysis

#Create subsets based on publisher name
yahoo_AF <-AF[which(profit_AF$`Publisher Name`=='Yahoo - US'),]

google_AF <-AF[which(profit_AF$`Publisher Name`=='Google - US'),]
google_global_AF <-AF[which(profit_AF$`Publisher Name`=='Google - Global'),]

msn_AF <-AF[which(profit_AF$`Publisher Name`=='MSN - US'),]
msn_global_AF <-AF[which(profit_AF$`Publisher Name`=='MSN - Global'),]

overture_AF <-AF[which(profit_AF$`Publisher Name`=='Overture - US'),]
overture_global_AF <-AF[which(profit_AF$`Publisher Name`=='Overture - Global'),]

class(kayak_AF$Net_Revenue)
class(kayak_AF$Media_Cost)

kayak_AF$Net_Revenue <- as.numeric(kayak_AF$Net_Revenue)
kayak_AF$Media_Cost <- as.numeric(kayak_AF$Media_Cost)

kayak_AF$loss_gain <- kayak_AF$`Net_Revenue` - kayak_AF$`Media_Cost`

kayak_AF$ratio2 <- kayak_AF$`Media_Cost` / kayak_AF$`Net_Revenue`

kayak_AF$ratio <- kayak_AF$`Net_Revenue` / kayak_AF$`Media_Cost`

kayak_AF[order(-kayak_AF$loss_gain), "loss_gain"]

#creating profit column (Amount - Total Cost)
#AF$profit <- AF$Amount - AF$`Total Cost`

#create new variable corr to save the correlation between few metrics
#Correlation yahoo
corr_AF_yus<-yahoo_AF[,c(24,19,16,17)]
cor(corr_AF_yus)
#Correlation google US
corr_AF_gus<-google_AF[,c(24,19,16,17)]
cor(corr_AF_gus)
#Correlation google Global
corr_AF_ggl<-google_global_AF[,c(24,19,16,17)]
cor(corr_AF_ggl)
#Correlation msn US
corr_AF_mus<-msn_AF[,c(24,19,16,17)]
cor(corr_AF_mus)
#Correlation msn Global
corr_AF_mgl<-msn_global_AF[,c(24,19,16,17)]
cor(corr_AF_mgl)
#Correlation overture US
corr_AF_ous<-overture_AF[,c(24,19,16,17)] 
cor(corr_AF_ous)
#Correlation Overture Global
corr_AF_ogl<-overture_global_AF[,c(24,19,16,17)]
cor(corr_AF_ogl)


#logistic regression
AF_log<-lm(profit_quant==1 ~yahoo_us_AF$Impressions + yahoo_us_AF$`Engine Click Thru %` , AF=yahoo_us_AF,family='binomial')
summary(AF_log)

#the regression is to justify our model
#we found that engine click through and trans conv% are noise variables since they can't really represent the performance of the dependent variable which is profits and impressions




# All ggplots and various plot experiments


#keyword graphs
ggplot(bigprofit_AF, 
       aes(x= bigprofit_AF$`Publisher Name`, y=bigprofit_AF$profit, fill= bigprofit_AF$`Keyword`)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Profit", fill = 'Keyword')+
  ggtitle("Keywords That Generates most of the Profit")+
  coord_flip()

# Creating a keyword graph with customized colors
ggplot(bigprofit_AF, 
       aes(x= bigprofit_AF$`Publisher Name`, y=bigprofit_AF$profit, fill= bigprofit_AF$`Keyword`)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Profit", fill = 'Keyword')+
  ggtitle("The Top Keywords That Drive 80% of Our Profit")+
  coord_flip()

#campaign graphs

ggplot(bigprofit_AF, 
       aes(x= bigprofit_AF$`Publisher Name`, y=bigprofit_AF$`profit`, fill= bigprofit_AF$Campaign)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Profit", fill = 'Campaign')+ 
  ggtitle("Campaigns That Generates 80% of the Profit")+
  coord_flip()

ggplot(bigprofit_AF, 
       aes(x= bigprofit_AF$`Publisher Name`, y=bigprofit_AF$Impressions, fill= bigprofit_AF$Campaign)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Impression", fill = 'Campaign')+ 
  ggtitle("Campaigns That Impressed Customers and  Generates 80% of the Profit")+
  coord_flip()


#overall AF graphs

ggplot(AF, 
       aes(x= AF$`Publisher Name` ,  y= AF$profit, fill = AF$Status )) +
  geom_bar(stat = "identity")+
  labs(x= 'Publisher Name' , y = "Profit", fill = "Status") +
  ggtitle("Profit - Raw AF")

ggplot(AF, 
       aes(x= AF$`Publisher Name` ,  y= AF$`Total Cost`, fill = AF$Status )) +
  geom_bar(stat = "identity")+
  labs(x= 'Publisher Name' , y = "Total Cost", fill = "Status") +
  ggtitle("Total Cost - Raw AF")


ggplot(AF, 
       aes(x= AF$`Campaign`, y=AF$`Total Cost`, fill= AF$Status)) +
  geom_bar(stat="identity")+
  labs(x="Campaign", y="Total Cost", fill = 'Status') +
  ggtitle("Cost of Campaigns That Generates 80% of Profits") + 
  coord_flip()


ggplot(bigprofit_AF, 
       aes(x= bigprofit_AF$`Campaign`, y=bigprofit_AF$`profit`, fill= bigprofit_AF$Status)) +
  geom_bar(stat="identity")+
  labs(x="Campaign", y="Profit", fill = 'Status') + 
  ggtitle("Status of Campaigns That Generates 80% of Profits") +
  coord_flip()


#kayak plots
ggplot(kayak_AF, 
       aes(x= kayak_AF$`Clicks `, y=kayak_AF$`Total Bookings`, fill= kayak_AF$`Search Engine`)) +
  geom_bar(stat="identity")+
  labs(x="Clicks", y="Total Bookings", fill = 'Search Engines') + 
  ggtitle("Search Engine Comparison - Clicks &  Bookings") 

ggplot(kayak_AF, 
       aes(x= kayak_AF$`Media_Cost`, y=kayak_AF$`Net_Revenue`, fill= kayak_AF$`Search Engine`)) +
  geom_bar(stat="identity")+
  labs(x="Media_Cost", y="Net_Revenue", fill = 'Search Engines') + 
  ggtitle("Search Engine Comparison - Media_Cost &  ") +
  coord_flip()


































































