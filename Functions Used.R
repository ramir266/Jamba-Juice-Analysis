#function used to plot Total Sales by Weekday and Month
#this produced boxplots, not lines
plot(factor(SMS_Retail$new_weekday[which(SMS_Retail$store_mon=="b.Feb-56")])
,SMS_Retail$Sales[which(SMS_Retail$store_mon=="b.Feb-56")],xlab="Weekday", 
ylab="Total Sales '$'",main= "Total Sales by Weekday for February Store #56")

#function used to plot Total Sales by Week and Month
plot(factor(SMS_Retail$new_week[which(SMS_Retail$store_mon=="a.Jan-549")])
     ,SMS_Retail$Sales[which(SMS_Retail$store_mon=="a.Jan-549")],xlab="Weeks", 
     ylab="Total Sales '$'",main= "Total Sales by Week for January Store #549")

#WEBSITE Used:
 # http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#summarySE per month
tgg<-summarySE(tg, measurevar = "Sales",groupvars=c("new_store","Month"))

#summarySE per weekday
tgd<-summarySE(tg, measurevar = "Sales",groupvars=c("new_store","Weekday"))

#ggplot formula
ggplot(tgc, aes(x=Week, y = Sales, colour = new_store)) + geom_errorbar(aes(ymin=Sales-se, ymax=Sales+se), width=.1) + geom_line() + geom_point()


#ggplot formula SE 
ggplot(tgc, aes(x=Week, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-se, ymax=Sales+se), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3)


#ggplot formula CI 
ggplot(tgc, aes(x=Week, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-ci, ymax=Sales+ci), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3)


#ggplot final SE
ggplot(tgc, aes(x=Week, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-se, ymax=Sales+se), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Week") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Week (SE)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))


#ggplot final CI
ggplot(tgc, aes(x=Week, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-ci, ymax=Sales+ci), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Week") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Week (CI)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))

#ggplot final per Month SE
ggplot(tgg, aes(x=Month, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-se, ymax=Sales+se), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Month") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Month (SE)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))

#ggplot final per Month CI
ggplot(tgg, aes(x=Month, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-ci, ymax=Sales+ci), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Month") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Month (CI)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))

#ggplot final per Weekday SE
ggplot(tgd, aes(x=Weekday, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-se, ymax=Sales+se), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Weekday") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Weekday (SE)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))

#ggplot final per Weekday CI
ggplot(tgd, aes(x=Weekday, y = Sales, colour = new_store,group= new_store)) + geom_errorbar(aes(ymin=Sales-ci, ymax=Sales+ci), colour="black", width=.1, position=pd) + geom_line(position=pd) + geom_point(position=pd, size = 3, shape = 21, fill = "white") + xlab("Weekday") + ylab("Sales")+ scale_colour_hue(name="Store", breaks =c("a.Store#56","b.Store#102","c.Store#161", "d.Store#482","e.Store#549","f.Store#638","g.Store#1235"),labels=c("Store 56","Store 102", "Store 161","Store 482", "Store 549", "Store 638", "Store 1235"),l=40) + ggtitle("Plot of Means of Sales by Stores per Weekday (CI)")+expand_limits(y=0) + theme_bw() + theme(legend.justification =c(1,0), legend.position =c(1,0))