library(shiny)
library(UsingR)
data(galton)

airline <- read.csv("airliner-data.csv")
m <- gregexpr("[a-zA-Z]{3}", airline$date, perl=TRUE)
airline$month <- tolower(regmatches(airline$date, m))
m <- gregexpr("-[0-9]{2,4}", airline$date, perl=TRUE)
tempYr <- regmatches(airline$date, m)
tempYr <- gsub("-0", "200", tempYr)
tempYr <- gsub("-1", "201", tempYr)
tempYr <- gsub("-", "", tempYr)
airline$year <- tempYr
airline <- airline[ifelse(airline$month=="character(0)", FALSE, TRUE),]
v <- as.character(airline$fat.)
airline$fatalities <- sapply(strsplit(v, split=c("+"), fixed=T), function(x) {sum(as.numeric(x))})
library(reshape2)
library(ggplot2)
plotYearBase <- function()
{
    vt <- melt(airline, id.vars=c("year"), measure.vars=c("fatalities"))
    sumVt <- dcast(vt, year ~ variable, sum)
    countVt <- dcast(vt, year ~ variable)
    names(countVt) <- c("year", "total.events")
    sumVt$total.events <- countVt$total.events
    new.df <- melt(sumVt, id.vars="year")
    names(new.df) <- c("year", "type", "count")
    ggplot(new.df, aes(x=year, y=count, fill=type)) + geom_histogram(stat="identity", position="dodge")
}

shinyServer(
    
    function(input, output) {
            
        output$plotdiag <- renderPlot({
            
            if(input$variable=="")
            {
                ""
            }
            else if({input$variable}=="Year")
            {
                vt <- melt(airline, id.vars=c("year"), measure.vars=c("fatalities"))
                sumVt <- dcast(vt, year ~ variable, sum)
                countVt <- dcast(vt, year ~ variable)
                names(countVt) <- c("year", "total.events")
                sumVt$total.events <- countVt$total.events
                new.df <- melt(sumVt, id.vars="year")
                names(new.df) <- c("year", "type", "count")
                ggplot(new.df, aes(x=year, y=count, fill=type)) + geom_histogram(stat="identity", position="dodge")
            }
            else if({input$variable}=="Month")
            {
                vt <- melt(airline, id.vars=c("month"), measure.vars=c("fatalities"))
                sumVt <- dcast(vt, month ~ variable, sum)
                countVt <- dcast(vt, month ~ variable)
                names(countVt) <- c("month", "total.events")
                sumVt$total.events <- countVt$total.events
                new.df <- melt(sumVt, id.vars="month")
                names(new.df) <- c("month", "type", "count")
                ggplot(new.df, aes(x=month, y=count, fill=type)) + geom_histogram(stat="identity", position="dodge")
            }
            else if({input$variable}=="Category")
            {
                vt <- melt(airline, id.vars=c("cat"), measure.vars=c("fatalities"))
                sumVt <- dcast(vt, cat ~ variable, sum)
                countVt <- dcast(vt, cat ~ variable)
                names(countVt) <- c("cat", "total.events")
                sumVt$total.events <- countVt$total.events
                new.df <- melt(sumVt, id.vars="cat")
                names(new.df) <- c("cat", "type", "count")
                ggplot(new.df, aes(x=cat, y=count, fill=type)) + geom_histogram(stat="identity", position="dodge")
            }
        })
        
        output$comments <- renderText({
            
            if(input$variable=="")
            {
                ""
            }
            else if({input$variable}=="Year")
            {
                paste("The trend shows a steady decline in the number of fatalities from 2005-2008 and 2010-2013. This suggests that the aviation safety measures were improving over the years. However, there are few anamolies like the 2009, 2010 and 2014. If you can observe, the number of events still follow a declining path over such years too. The recent tragic events, for whatever reasons, has caused heavy casualities and fear among people to fly, however the trend from the data suggests that the aviation safety is only getting better.")
            }
            else if({input$variable}=="Month")
            {
                paste("One interesting aspect to quesiton is: Are the accidents/fatalities related to bad weather that usually happens in the rainy season of May to September. The data says YES ! It is clearly evident from the graph that July has the most number of fatalities over the past 10 years followed by August, May, June and September. Hence, the unpredictable bad weather is directly related to the number of fatalities.")
            }
            else if({input$variable}=="Category")
            {
                paste("The Category variable suggests Accident Categories:",("A = Accident"),("I = Incident"),("H = Hijacking"),
("C = Criminal occurrence (sabotage, shoot down)"),("O = other occurrence (ground fire, sabotage)"),(" "),("1 = hull-loss"),
("2 = repairable damage"), ("The results suggests an obvious relation of Accident hull-loss with fatalities, however the next immediate threat is attributed to C - criminal occurrences, whereas Hijacking (label H) shows to be less severe."), sep="\n")
            }
        })

        output$documentation <- renderText({
            
            paste("Documentation:" , "This app is a fairly simply and straight forward to use. It is about understanding the recent trends in aviation incidents over the past 10 years. There were different parameters on the basis of which the aviation incidents can be studied. We can look at an Year based histogram by selecting the \"Year\" as the parameter value from the drop down menu. It gives a histogram over the last 10 years plotting the fatalities (red bar) and total incidents (blue bar) for each year. Similarly, we can look at a month based histogram by selecting the parameter value as \"Month\" from the drop down menu. This would showcase the month-wise total fatalities and incidents summed over the 10 years. The last one being the Category of accident, i.e., whether its an Accident or Hijack or Criminal Occurrence, etc along with its severity. The drop down menu gives \"Category\" as the last option and this displays a similar histogram as the other two (month, year).", sep="\n" )
        })
    }    
)

