library(shiny)
BP2010 <- read.csv("BP Apprehensions 2010.csv", header = TRUE, stringsAsFactors = FALSE)
PB2017 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = TRUE)
PBmonthly <- read.csv("monthly_sum.csv", header = TRUE, stringsAsFactors = TRUE)
rownames(PBmonthly) <- PBmonthly[,1]


twolineplot <- function(){
  #overlays the plots of 2 lines 1 blue and 1 red; for use with the apprehensions by month of 2010 and 2017
  x <-  factor(2:13, labels = c("October", "November", "December", "January", "February", "March", "April","May","June","July","August","September"))
  t2010 <- as.numeric(PBmonthly[1,2:13])
  t2017 <- as.numeric(PBmonthly[8, 2:13])
  plot(t2010 ~ x, type="p", xlab = "Month", ylab = "Apprehensions", main = "Apprehensions By Month", ylim=c(min(t2010),max(t2017)))
  lines(t2010 ~ x, col="blue")
  lines(t2017 ~ x, col="red")
  legend("topleft", 
         c("2010", "2017"), 
         fill = c("blue", "red"))
  
}

# Displaying Data By Sector
rownames(BP2010) <- BP2010[,1]
sector_plots <- function(year){
  #Produces a barplot of the data blocked by sector for the given year (years only allow for 2010 and 2017)
  if(year == 2010){
    barplot(BP2010[1:9,13], names.arg = rownames(BP2010)[1:9],
             las=2,
             axisnames=TRUE,
             main="2010 Border Patrol Apprehensions by Sector",
             border="blue",
             col="yellow")
  }
  else{
    rownames(PB2017) <- PB2017[,1]
    barplot(PB2017[1:9,13], names.arg = rownames(PB2017)[1:9],
        las=2,
        axisnames=TRUE,
        main="2017 Border Patrol Apprehensions by Sector",
        border="blue",
        col="yellow")
  }
}

disp_by_year <- function(plottype){
  #Display Data By Years for 2010 to 2017 as plottype; accepts line, line2, or defaults to bar
  if(plottype == "line"){
    x <- as.vector(t(PBmonthly))
    y <- ts(rev(x), start = c(2000, 10), frequency = 12)
    return(ts.plot(y, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3))))
  }
  else if(plottype == "line2"){
    plot (PBmonthly$year,rowSums(PBmonthly[1:18 , 2:13]), xlab = "Year", ylab = "Apprehensions", type = "p", main = "Apprehensions By Year")
    lines(PBmonthly$year,rowSums(PBmonthly[1:18 , 2:13]), col = "red")
    points(PBmonthly$year,rowSums(PBmonthly[1:18 , 2:13]), col = "red")
  }
  else{
    return(barplot(PBmonthly[1:9,13], names.arg = rownames(PBmonthly)[1:9],
          las=2,
          axisnames=TRUE,
          main="Border Patrol Apprehensions by Year",
          border="blue",
          col="yellow"))
  }
  
}


## Creates a 2 x 9 matrix from the 2010 data and 2017 for specified month
sideBySideMatrix <- function(month){
  matrix(c(BP2010[1:9,month], PB2017[1:9,month]), nrow = 2, byrow = TRUE)
}

## Creates the barplot for a given month
sideBySideBarPlot <- function(month, monthString){
  barplot(sideBySideMatrix(month), names.arg = rownames(BP2010),
          las=2,
          axisnames=TRUE,
          beside=TRUE,
          col=c("blue", "red"),
          main = paste("2010 vs 2017 Border Patrol Apprehensions in", monthString, sep=" "))
  legend("topleft", 
         c("2010", "2017"), 
         fill = c("blue", "red"))
}

## Creates the side by side bar plots for each month


comparep <- function(ttest, sigvalue ){
  #This function determines the message to display in the t test tab based on the user inputted significance value of the test
  if(ttest$p.value < as.double(sigvalue)){
    out <- paste("The t-value of", ttest$p.value, " is less than the significance value of", sigvalue, ", so we reject the null hypothesis that the means of the largest sectors of 2010 and 2017 are equal.")
  }
  else{
    out <- paste("The t-value of", ttest$p.value, " is more than the significance value of", sigvalue, ", so we fail to reject the null hypothesis that the means of the largest sectors of 2010 and 2017 are equal. There is no statistically significant difference between the means of the largest sectors.")
  }
  return(out)
  
  
}

ui <- navbarPage(title="Analysis of Illegal Alien Apprehension Data",
                 
                 tabPanel("About",
                          mainPanel(
                            h2("Each tab examines a different aspect of BP's data on Illegal Alien Apprehension"),
                            h3("This project was completed by Matthew Ciaramitaro, Steven Tran, and Praveen Kenderla")
                          )
                 ),
                 tabPanel("Apprehensions by sector",
                        p("The following graphs display the number of apprehensions distributed across each sector in the given year"),
                        sidebarPanel(
                          radioButtons("year", h3("Year"),
                                     choices = list(2010, 2017), selected = 2010)
                        ),
                        mainPanel(
                          plotOutput("sector"),
                          p("Notice how the maximum between 2010 and 2017 has changed from Tuscon to Rio Grande, with each quantity changing dramatically.")
                          
                        )
                      
                 ),
                 tabPanel("Apprehensions By Year",
                          p("The following barplot examines the change in apprehension number between 2010 and 2017"),
                          sidebarPanel(
                            radioButtons("plottype", h3("Year"),
                                         choices = list("line (years by month)"="line", "line (discrete years)" ="line2",  "bar"="bar"), selected = "line")
                          ),
                          mainPanel(
                            plotOutput("plottype")
                            
                          )
                 ),
                 tabPanel("Comparing 2010 to 2017 by month",
                          
                        p("The following bar graphs compare the number of Illegal Aliens apprehended over the selected month between 2010 and 2017"),
                        sidebarPanel(    
                          selectInput(  inputId = "month",
                                        label="Select a month", 
                                        choices= c("January",
                                                   "February",
                                                   "March",
                                                   "April",
                                                   "May",
                                                   "June",
                                                   "July",
                                                   "August",
                                                   "September",
                                                   "October",
                                                   "November",
                                                   "December"
                                                   )            
                            )
                            
                        ),
                        mainPanel(
                          plotOutput("month")
                        )
                 ),
                 navbarMenu("More",
                   tabPanel("T testing",
                          p("The following t-test for difference in means was done between Tuscon 2010 and Rio Grande 2017."),
                          sidebarPanel(
                            textInput(inputId="sigval", 
                                      label="Please enter the significance value for the T test",
                                      value=".05"
                            )
                          
                          ),
                          mainPanel(
                            
                            #TODO: Create Graph of Cum T dist for test
                            plotOutput("tgraph"),
                            br(),
                            p(textOutput("ttest")),
                            br(),
                            h2("Summary of T-test Results"),
                            verbatimTextOutput("tstats")
                          )
                   ),
                   tabPanel("Cumulative Apprehension Trends",
                          p("The following line graph examines the trends in months during 2010 and 2017 in a line graph."),
                          plotOutput("twoline")
                   )
                 ),
                 fluid = T
                 
)


server <- function(input, output) {
  #handling sector plot tab
  output$sector <- renderPlot({sector_plots(strtoi(input$year))})
  
  #handling months tab
  months = c("September"= 13, "October"=2, "November"=3, "December"=4, "January"=5, "February"=6, "March"=7, "April"=8,"May"=9,"June"=10,"July"=11,"August"=12  )
  output$month <- renderPlot({
      sideBySideBarPlot(months[input$month] , input$month)
    
  })
  #handling yearly apprehensions tab
  output$plottype <- renderPlot({disp_by_year(input$plottype)}) 
  
  
  #Performing T test
  x <- subset(BP2010, select=-c(Sector))
  x <- cbind(x,rowSums(x))
  x <- t(x)
  y <- subset(PB2017, select=-c(Sector))
  y <- cbind(y,rowSums(y))
  y <- t(y)
  t <- t.test(x[,8], y[,6])
  
  #Creating graph of Ttest
  range <- seq(-3,3,by=.1)
  cdf <- dt(range, t$parameter)
  output$tgraph <- renderPlot({
                    plot(cdf ~ range, type ="l") 
                    polygon(c( range[range <= t$statistic], t$statistic ),  c(cdf[range <= t$statistic], 0), col="blue") #create colored graph
                   }) #cdf of t distribution
  output$ttest <- renderText({comparep(t, input$sigval)})
  output$tstats <- renderText({paste("Null Hypothesis:", "The difference in means between 2010 and 2017 data is 0\n",
                                    "T statistic: ", t$statistic, "\n",
                                    "P Value: ", t$p.value,  "\n",
                                    "Degrees of Freedom", t$parameter, "\n",
                                    "Confidence Interval", t$conf.int
                   )})
  #generating x axis for comparison line plot
  output$twoline <- renderPlot(twolineplot())
}

shinyApp(ui=ui, server=server)