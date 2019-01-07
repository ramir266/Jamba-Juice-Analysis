#function used to plot Total Sales by Weekday and Month
#this produced boxplots, not lines
plot(factor(SMS_Retail$new_weekday[which(SMS_Retail$store_mon=="b.Feb-56")])
,SMS_Retail$Sales[which(SMS_Retail$store_mon=="b.Feb-56")],xlab="Weekday", 
ylab="Total Sales '$'",main= "Total Sales by Weekday for February Store #56")