library(shiny)
library(ggplot2)
suppressMessages(library(h2o))
library(ggmap)
require(rCharts)
library(plotly)
library(nnet)
library(networkD3)
set.seed(889)


options(digits=4)
options(verbose = FALSE)
options(show.error.messages = FALSE)
## Initializing h2o.

h2o.init()
set.seed(889)


Data <- read.csv("anim.csv",header=T)
Data <- Data[-1]
# Boosting <- h2o.loadModel(("C:\\Users\\Sapana\\Desktop\\MastersProject\\Crime\\h2o\\GBM_model_R_1460313883620_16"))
 
twodistricts <- read.csv("twodistricts.csv") ## links police district with actual districts
twodistricts <- twodistricts[-1]

district_regions <- read.csv('district_regions.csv')

locations <- read.csv('loations.csv')
locations <- locations[-c(1,3,5)]
locations$Address <- NULL

names(locations)[names(locations)=="lon"] <- "X"
names(locations)[names(locations)=="lat"] <- "Y"



## Function to parse ST, AV, CT etc 
substrRight <- function(x, n){
  sapply(x, function(xx)
         substr(xx, (nchar(xx)-n+1), nchar(xx))
         )
}

## Loading Models

load(file = "Multinomial.Rda")
Bagging <- h2o.loadModel("C:\\Users\\Sapana\\Desktop\\MastersProject\\Crime\\Grid_DRF_RTMP_sid_bdc7_3_model_R_1462733640885_2_model_394")
RandomForest <- h2o.loadModel("C:\\Users\\Sapana\\Desktop\\MastersProject\\Crime\\Grid_DRF_RTMP_sid_bdc7_3_model_R_1462733640885_1_model_395")
Boosting <- h2o.loadModel(("C:\\Users\\Sapana\\Desktop\\MastersProject\\Crime\\Grid_GBM_RTMP_sid_9b98_3_model_R_1462733125848_1_model_280"))

classifiers <- list()

  for ( i in seq(13,23,2)){
  classifiers[[i]] <- h2o.loadModel(paste("C:\\Users\\Sapana\\Desktop\\MastersProject\\Crime\\GLM_model_R_1460742344752_",i,sep=""))
}

library(lubridate)
shinyServer(function(input, output) {
	    
fromDTtry1 <- reactive({
		 strptime(input$datetime,'%m/%d/%Y %I:%M:%S %p')
})

var<- reactive({
	  weekdays(as.Date(fromDTtry1()))
})

output$DayOfWeek <- renderUI({
	  textInput('DayOfWeek',"Day of Week",value = var())
})
	
# MapFrame <- reactive({
# 	
# return(locale)
# 	
# })
# 
# output$mapsummary<-renderPrint({
# 	MapFrame()
# 	
# })

output$Map <- renderPlot({
later<-as.numeric(input$X)
longer<-as.numeric(input$Y)
locale<-data.frame(later,longer)

DriveMap <- get_map(location = c(lon=later,lat = longer), zoom = 14, maptype = c('roadmap'), source=c('google'))
ggmap(DriveMap)+
  geom_point( data=locale,aes(y=longer,x=later),color="blue",size=14,shape=13)+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
	
},width = 600, height = 600, res = 100)

df <- reactive({
	       # library(lubridate)
	       Year <- (year(ymd_hms(fromDTtry1())))
								Month<- (month(ymd_hms(fromDTtry1())))
								Day<- day(ymd_hms(fromDTtry1()))
								Hour<- hour(ymd_hms(fromDTtry1()))
								Minute<- (minute(ymd_hms(fromDTtry1())))
								Second<- second(ymd_hms(fromDTtry1()))
# 								TimeH <- as.numeric(Hour + (Minute/60))
# 								TimeM <- as.numeric((Hour * 60) + Minute)
# 								MonthAbb <- month.abb[Month]
								
								X = suppressWarnings((as.numeric(input$X)))
								Y = suppressWarnings((as.numeric(input$Y)))
								
							 UspsAbb <- substr(input$address, (nchar(input$address)-2+1), nchar(input$address))
								UspsAbb <- ifelse(grepl('/',UspsAbb), "Intersection",substr(input$address, (nchar(input$address)-2+1), nchar(input$address)) )
							
								DayOfWeek = input$DayOfWeek
								PdDistrict <- input$pddd
								
								mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
								
								Year2 <- (year(ymd(mydate[2])))
								Month2<- (month(ymd(mydate[2])))
								Day2<- day(ymd(mydate[2]))
								
								Year3 <- (year(ymd(mydate[3])))
								Month3<- (month(ymd(mydate[3])))
								Day3<- day(ymd(mydate[3]))
								
								Year4 <- (year(ymd(mydate[4])))
								Month4<- (month(ymd(mydate[4])))
								Day4<- day(ymd(mydate[4]))
								
								Year5 <- (year(ymd(mydate[5])))
								Month5<- (month(ymd(mydate[5])))
								Day5<- day(ymd(mydate[5]))
								
						  df = data.frame(X,Y,Year,Month,Day,Hour,Minute,PdDistrict,DayOfWeek, UspsAbb)
        
        df = rbind(df, '2'=c(X,Y,Year2,Month2,Day2,Hour,Minute,PdDistrict,DayOfWeek, UspsAbb))
        df = rbind(df, '3'=c(X,Y,Year3,Month3,Day3,Hour,Minute,PdDistrict,DayOfWeek, UspsAbb))
        df = rbind(df, '4'=c(X,Y,Year4,Month4,Day4,Hour,Minute,PdDistrict,DayOfWeek, UspsAbb))
        df = rbind(df, '5'=c(X,Y,Year5,Month5,Day5,Hour,Minute,PdDistrict,DayOfWeek, UspsAbb))
        
        index = c(3:5,8:10)
        df[index] <- lapply(df[index],as.factor)
        index_numeric = c(1:2,6:7)
          df[index_numeric] <- lapply(df[index_numeric],as.numeric)
       
        
        return((df))
	
})

output$experiment <-renderPrint({
		  	 str(df())
df()
})

## H2o Frame that will be used for all models except multinomial.
h2oframe1 <- reactive({
					
		if (!is.null(df())){		  
	 h2oframe1 <- as.h2o(df(),'h20frame')
	 h2oframe1[,c(1:2,6:7)] <- as.numeric(h2oframe1[,c(1:2,6:7)])
 	return(h2oframe1)
								}
})

## Code for Logistic 
b <- reactive({
	
		pred <- list()
  broad <- c("NonResiProp", "Others", "Personal" , "ResiProp",  "Statutory" ,"WhiteCollar")
  broad <- as.h2o(broad,'broad')
  for (i in seq(13,23,2) ){
	 
	 pred[[i]]<- h2o.predict(classifiers[[i]],newdata = h2oframe1(),type='response')
	 
  }

  
  PredDF <- as.data.frame(do.call(h2o.rbind,pred))
  PredDF$Category <- NA
  PredDF[c(1:5),][,4] <- "NonResiProp"
  PredDF[c(6:10),][,4] <- "Others"
  PredDF[c(11:15),][,4] <- "Personal"
  PredDF[c(16:20),][,4] <- "ResiProp"
  PredDF[c(21:25),][,4] <- "Statutory"
  PredDF[c(26:30),][,4] <- "WhiteCollar"
  
  
  FinalStack <- list()
  maxi <- list()
  for( i in 1:5){
  	b<- PredDF[seq(i,nrow(PredDF)+ i -1, 5),]
  	# b <- cbind(b,broad)
  	b$p1 <- b$p1/sum(b$p1)
  	c <- b[order(b$p1),]
  	FinalStack[[i]] <- tail(c,1)
  	
  }
  	
  OvaLogisPred <- do.call(rbind,FinalStack)
  
   return(OvaLogisPred)
})




# output$lazy <- renderPrint({
# 	str(df())
# })


output$summary <-renderPrint({
	
if (input$model == 'mult'){
	 df <- df()
	 index = c(1:2,6:7)
  df[index] <- lapply(df[index],as.numeric)
		myTest_Mult <- df[1,]
		
		MultNom_Prob <- predict(MultNom,myTest_Mult, "probs")
# 		MultNom_Prob$Predict <- colnames(MultNom_Prob)[(apply(MultNom_Prob,1,which.max))]
		
		
# 		MaxCategory <- max.col(MultNom_Prob,ties.method='random')
# 		PredCat <- colnames(MultNom_Prob)[MaxCategory]
# 		MultNom_Prob$Category <- PredCat
		
		return((MultNom_Prob))

}
	
if (input$model =="bg")	{
	myTest_Bag <- h2oframe1()
	PredBg <- h2o.predict(object=Bagging,newdata =myTest_Bag[1,])
return(PredBg)

}

if (input$model =="rf")	{
	 myTest_Bag <- h2oframe1()
  PredBg <- h2o.predict(object=RandomForest,newdata =myTest_Bag[1,])
return(PredBg)

}


if (input$model =="gbm")	{
	myTest_Bag <- h2oframe1()
	
PredBg <- as.data.frame(h2o.predict(object=Boosting,newdata =myTest_Bag[1,]))
return(PredBg)

}
	

if (input$model =="ova")	{

    b <- b()[-1][1,]
    return(b)
}
	
})



output$plot <- renderPlotly({
	
	if (input$model=='gbm'){
	 
	 	 PredBg <- as.data.frame(h2o.predict(object=Boosting,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData <- data.frame(p1= Max, x = PredCat,Date= mydate)
	 	

    base <-	ggplot(plotData, aes(Date, p1, label= x)) + geom_line() + xlab("Date") + ylab("Probabilities") +geom_label()
    base1 <- base +geom_text(vjust=0, colour="red" , size = 3) + expand_limits(y=0) +  ggtitle('Probabilities vs. Dates')
    (pfinal <- ggplotly(base1))
    pfinal
  
  
  
		}
	
		
		else if (input$model=='rf'){
	 
	 	 PredBg <- as.data.frame(h2o.predict(object=RandomForest,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData <- data.frame(p1= Max, x = PredCat,Date= mydate)
	 	

    base <-	ggplot(plotData, aes(Date, p1, label= x)) + geom_line() + xlab("Date") + ylab("Probabilities") +geom_label()
    base1<- base +geom_text(vjust=0, colour="red",angle = 45,size = 3) + expand_limits(y=0)  + ggtitle('Probabilities vs. Dates')
    
    (pfinal <- ggplotly(base1))
    pfinal
  
		}
	
	else	if (input$model=='bg'){
	 
	 	 PredBg <- as.data.frame(h2o.predict(object=Bagging,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData <- data.frame(p1= Max, x = PredCat,Date= mydate)

    base <-	ggplot(plotData, aes(Date, p1, label= x)) + geom_line() + xlab("Date") + ylab("Probabilities") + geom_label()
    base1 <- base +geom_text(angle = 45, vjust=0, colour="red",size =3) + expand_limits(y=0)  + ggtitle('Probabilities vs. Dates')
    (pfinal <- ggplotly(base1))
    pfinal
	
	}
	
	
	else if(input$model =='ova'){
	 	 b <- b()[-1]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData <- cbind(b,mydate)
	 	 names(plotData)[names(plotData)=='Category'] <- 'x'
	 	 names(plotData)[names(plotData)=='mydate'] <- 'Date'
	 	 # plotData <- data.frame(p1= Max, x = PredCat,Date= mydate)
		
		  base <-	ggplot(plotData, aes(Date, p1, label= x)) + geom_line() + xlab("Date") + ylab("Probabilities") +geom_label()
    base1 <- base +geom_text(vjust=0, colour="red",angle=45,size = 3) + expand_limits(y=0) + ggtitle('Probabilities vs. Dates')
    (pfinal <- ggplotly(base1))
    pfinal
	}
    
	
else if(input$model == 'mult'){
	
	   df <- df()
	   index = c(1:2,6:7)
    df[index] <- lapply(df[index],as.numeric)
	  	myTest_Mult <- df
	  	MultNom_Prob <- (predict(MultNom,myTest_Mult, "probs"))
	  	
	  	Max <- apply(MultNom_Prob,1,max)
    MaxCategory <- max.col(MultNom_Prob,ties.method = "random")
    PredCat <- colnames(MultNom_Prob)[MaxCategory]
    mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
    plotData <- data.frame(p1= Max, x = PredCat,Date= mydate)
    plotData$p1 <- round(plotData$p1,3)
    
#     p2 <- nPlot(p1 ~ Date, data = plotData, type = "lineChart")
#     p2$addParams(dom = 'plot')
#   
# 	 	 p2$xAxis(axisLabel = 'Dates')
# 	 	 p2$yAxis(axisLabel = 'Probabilities',width=50)
# 	 	 p2$xAxis(tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( d * 86400000 ));}!#" )
# 	 	 p2$yAxis(tickFormat = "#! function(d) {return d3.format(',.3f')(d)} !#")
# 	 	 p2$chart(forceY = c(0, max(plotData$p1)+.05))
# 	 	 
# 	 	 p2$chart(tooltipContent= "#! function(key, x, y, e){
#     return '<b>Probability:</b> ' + e.point.p1 + '<br/>' + '<b>Predicted Class: </b>' + e.point.x
#            } !#")
  
     base <-	ggplot(plotData, aes(Date, p1, label= x)) + geom_line() + xlab("Date") + ylab("Probabilities") +geom_label()
    base1 <- base +geom_text(vjust=0, colour="red",angle=45,size = 3) + expand_limits(y=0) + ggtitle('Probabilities vs. Dates')
    (pfinal <- ggplotly(base1))
    pfinal
   
	  
}    
		

	else (NULL)
		
	 
})


##Killer R Charts Implementation - Axis, Labels, Dates and Tooltip Customization
output$myChart <-renderChart({
	
	  	PredBg <- as.data.frame(h2o.predict(object=Boosting,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData1 <- data.frame(p1= Max, x = PredCat,Date= mydate, Model = rep('Boosting',5))
	 	 
	 	 PredBg <- as.data.frame(h2o.predict(object=Bagging,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData2 <- data.frame(p1= Max, x = PredCat,Date= mydate,Model = rep('Bagging',5))
	 	 
	 	 PredBg <- as.data.frame(h2o.predict(object=RandomForest,newdata =h2oframe1()))[,-1]
	 	 Max <- apply(PredBg,1,max)
	 	 MaxCategory <- max.col(PredBg,ties.method = "random")
	 	 PredCat <- colnames(PredBg)[MaxCategory]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData3 <- data.frame(p1= Max, x = PredCat,Date= mydate,Model = rep('Random Forest',5))
	 	 
	 	 b <- b()[-c(1:2)]
	 	 mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
	 	 plotData4 <- cbind(b,mydate)
	 	 names(plotData4)[names(plotData4)=='Category'] <- 'x'
	 	 names(plotData4)[names(plotData4)=='mydate'] <- 'Date'
	 	 plotData4$Model <- rep('OVA Logistic', length.out = 5)
	 	 
	   df <- df()
	   index = c(1:2,6:7)
    df[index] <- lapply(df[index],as.numeric)
		  myTest_Mult <- df
		  MultNom_Prob <- (predict(MultNom,myTest_Mult, "probs"))
		  
		  Max <- apply(MultNom_Prob,1,max)
    MaxCategory <- max.col(MultNom_Prob,ties.method = "random")
    PredCat <- colnames(MultNom_Prob)[MaxCategory]
    mydate <- seq(as.Date(fromDTtry1()), by = "months", length.out = 5)
    plotData5 <- data.frame(p1= Max, x = PredCat,Date= mydate)
    plotData5$Model <- rep('Multinomial', length.out = 5)
	   	 
	 	 FinalDataFrame <- rbind(plotData1,plotData2,plotData3,plotData4,plotData5)
	 	 FinalDataFrame$Date <- as.numeric((FinalDataFrame$Date))
	 	 FinalDataFrame$p1 <- round(FinalDataFrame$p1,3)
	 	 
	 	 p1 <- nPlot(p1 ~ Date, data = FinalDataFrame, group = 'Model', type = "lineChart", label='x')
	 	
	 	 p1$addParams(dom = 'myChart')
	 	 p1$xAxis(axisLabel = 'Date')
	 	 p1$yAxis(axisLabel = 'Probabilities',width=50)
	 	 p1$xAxis( tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( d * 86400000 ));}!#" )
	 	 p1$yAxis(tickFormat = "#! function(d) {return d3.format(',.3f')(d)} !#")
	 	  p1$chart(forceY = c(0, max(FinalDataFrame$p1)+.05))
	 	  # p1$chart(margin = list(left = 100))
	 	  
	 	  p1$chart(tooltipContent= "#! function(key, x, y, e){
     return '<b>Probability:</b> ' + e.point.p1 + '<br/>' +
              '<b>Model: </b>' + e.point.Model + '<br/>' + '<b>Predicted Class: </b>' + e.point.x
           } !#")
	 	 
	 	 
	 	 return(p1)
	 	 

})


output$force <- renderDiagonalNetwork({
	
	#### Create tree from a hierarchical R list
CrimeCategory <- list(name = "Categories", children = list(list(name = "Non Residential Property",
                    children = list(list(name = "Larceny/Theft"),list(name="Non Criminal"),
                    														list(name = "Recovered Vehicle"),list(name="Vehicle Theft")	)),
               list(name = "Residential Property",
                    children = list(list(name = "Arson"),
                                    list(name = "Burglary"), list(name="Trea"),
                    																list(name = "Trespass"),list(name = "Vandalism") )),
               list(name = "Personal",
                    children = list(list(name = "Assualt"),
                                    list(name = "Family Offenses"), list(name="Kidnapping"),
                    																list(name = "Missing Person"),list(name = "Prostituition"),
                    																list(name = "Robbery"),list(name = "Runaway"),list(name = "Sex Offenses Forcible"),
                    																list(name = "Sex Offenses Non Forcible"),list(name = "Suicide"), list(name = "Suspcious OCC"))),
               list(name = "Statutory",
                    children = list(list(name = "Disorderly Conduct"),
                                    list(name = "Driving Under The Influence"), list(name="Drunkenness"),
                    																list(name = "Liquor Laws"))),
               list(name = "White Collar",
                    children = list(list(name = "Bad Checks"),
                                    list(name = "Bribery"), list(name="Embezzlement"),
                    																list(name = "Extortion"), list(name="Forgery/Counterfeiting"),
                    																list(name = "Fraud"), list(name="Gambling"))),
               list(name = "Others",
                    children = list(list(name = "Drug/Narcotic"),
                                    list(name = "Loitering"), list(name="Other Offenses"),
                    																list(name = "Pornography/Obscene Mat"), list(name="Secondary Codes"),
                    																list(name = "Stolen Property"), list(name="Warrants"),
                    																list(name= "Weapon Laws")))
))

diagonalNetwork(List = CrimeCategory, fontSize = 17,opacity = 5, height = 50, width= 50,
textColour = '#5d8aa8',nodeStroke = '#997a8d', linkColour = '#ff6961',nodeColour = 'violet')

})

## Code for Animation Tab


starttime <- reactive({
		 x<- strptime(input$starttime,'%m/%d/%Y %I:%M:%S %p')
		 return(x)
})


# Core Data frame
output$value<- renderPrint({
	
 print(str(MainDataFrame()))
 
})

##This code generates all dates users specify
DateSeq <- reactive({
	DateSequence <- seq(from=as.POSIXct(starttime(), tz="EST"),
                    to=as.POSIXct(starttime(), tz="EST"),
                    by="days")  
	return(DateSequence)
	
})

###Takes the difference of two dates
# datediff <- reactive({
# 	 difference <- max(DateSeq()) - min(DateSeq())
# 	 return(difference)
# 	
# })
# 
# ## Converting difference from above to 
# datediff1 <- reactive({
# 	 
# 	 stepwise <- if (input$datetimerange =='days'){ days <- 86400; return(days)}
# 	             else if (input$datetimerange == 'hours'){hours <- 60;return(hours)}
# 	             else if (input$datetimerange == 'years'){years <-3136000 ;return(years)}
# 	             else  if (input$datetimerange == 'months'){ months <- 2629746; return(months)}
# 	             	else NULL
# 	return(stepwise)
# })
# 
# output$datestuff <-renderUI({
# 	
# 	 	
# 	 sliderInput("datestuff",
#                       "Start Animation:",
#                       min = min(DateSeq()),
#                       max = max(DateSeq()), value =min(DateSeq()),
#                       animate=TRUE, step=datediff1())
# })
# 
# # x<- seq(from=as.POSIXct(strptime("12/23/2014 1:05:00 pm", '%m/%d/%Y %I:%M:%S %p'), tz="EST"),
# # to=as.POSIXct(strptime("12/23/2014 1:05:00 pm", '%m/%d/%Y %I:%M:%S %p'), tz="EST"),
# # by=days)
# 
# 


## This code creates a data frame of unique latitude long along with districts and year, month info to generate predicxtions
MainDataFrame <- reactive({
	       
								
 df <- list()
	for (i in 1:length(DateSeq()))({
		
		df[[i]] <- data.frame(locations, Date = rep(DateSeq()[i],nrow(locations)))
		
	})
 
 DF<- do.call(rbind,df)
 
    DF$Year <- lubridate::year(ymd_hms(DF$Date))
				DF$Month<- lubridate::month(ymd_hms(DF$Date))
				DF$Day<- lubridate::day(ymd_hms(DF$Date))
				DF$Hour<- lubridate::hour(ymd_hms(DF$Date))
				DF$Minute<- lubridate::minute(ymd_hms(DF$Date))
				

				DF$Date <- as.POSIXct(paste(paste(DF$Year,DF$Month,DF$Day,sep='-'),paste(DF$Hour,DF$Minute,sep=':')) )
				DF$DayOfWeek <- as.factor(weekdays(as.Date(paste(DF$Year,DF$Month,DF$Day,sep='-'))))
# 				DF$DayOfWeek <- as.factor(DF$DayOfWeek)
# 				DF<- DF[-6]
				  DF$Date <- as.character(DF$Date)
			index = c(7:9)
  		DF[index] <- lapply(DF[index],as.factor)
				
    
# 				# DF <- merge(district_regions,twodistricts, by='District')
# 				DF <- district_regions

								
 return(DF)
								
								
								
})

# This code generates h2o frame that allows prediction
h2oframe <- reactive({
					
		if (!is.null(MainDataFrame())){		  
	 h2oframe <- as.h2o(MainDataFrame(),'h2oframe')
	 return(h2oframe)
								}
})

##This generates combined data frame of class predictions along with the locations data frame. 
VizData <-reactive({
	 
	 h2oframe <- h2oframe()
		PredBoosting <- h2o.predict(object=Boosting,newdata = h2oframe())
# 		Max <- as.h2o(apply(as.data.frame(PredBoosting)[,-1],1,max))  ##[,-1] removes the predict column
# 	 zz <- h2o.cbind(h2oframe,PredBoosting$predict, (Max))
#   yy <- as.data.frame(zz) ## converting h2o frame to zz
#   names(yy)[names(yy)=="x"] <- "Probability"
#   library(dplyr)
#   summarized_data <- yy %>% group_by(predict, PdDistrict) %>% summarize(Avg_Probability = mean(Probability))
#   summarized_data <- as.data.frame(summarized_data)
#  
#   afteraggdata <- merge(yy,summarized_data)
#   afteraggdata$ID <- as.numeric((afteraggdata$PdDistrict))
#   afteraggdata1 <-subset(afteraggdata, predict == input$category)
#   afteraggdata2 <- merge(afteraggdata1,twodistricts)
#   district_regions2 <-merge(district_regions,afteraggdata2, by="District")
		
		MergedData <- as.data.frame(h2o.cbind(h2oframe,(PredBoosting)))
  
  return(((MergedData)))

})

# output$value1 <-renderPrint({
# 	
# 	 DateSeq()
# 	
# })

output$plotViz<- renderPlot({
	# Generating SF map
locations = c(left = -122.5222, 
                bottom = 37.7073, 
                right = -122.3481,
                top = 37.8381)
  
SF <-get_map(location = locations, zoom = 12, source = "google", color="color")  

# VizData <- VizData()
Category <- input$category
District_crime <- ggmap(SF) + geom_polygon(data=district_regions,aes(x=long, y=lat,group=group, fill = District
																		                                                    ),size=0.5, color="#636363",alpha = .3) + 
	                             scale_color_continuous(low = "#3482FB", high = "#FB3482", space = "Lab",  guide = "colourbar")+
	                             ggtitle(paste("SF District Map: ", input$category, sep="")) + 
	     geom_point(data=VizData(), aes_string(x="X", y="Y",size = Category), color="black",  position = 'jitter',  alpha=1)+ 
	     scale_size(Category,range=c(1,6)) 
(District_crime)

}, width = 1000, height = 1000)

})
