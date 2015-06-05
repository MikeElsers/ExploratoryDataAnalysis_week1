##  combined graphics  (4 plots, 2x2 matrix)


plot4 <-  function () {
    
    #--  save the locale (we need to switch to US) -----------
    backkup_locale <- Sys.getlocale('LC_TIME')
    
    Sys.setlocale('LC_TIME', 'C')
    
    
    #------------ set the file to be used -----------------------
    #-----------  (..._reduced  only for test purpose) ----------
    filename <-  "household_power_consumption.txt"
    #filename <-  "household_power_consumption__reduced.txt"
    
    
    alldata <- read.csv( filename,  sep = c(';')) 
    
    
    ##------------ convert date  and time --------------
    cmbDate <-  paste (  alldata$Date, "_", alldata$Time, sep="")
    
    alldata$Date  <-   as.Date( alldata$Date,  "%d/%m/%Y")
    alldata$FullDate  <-  strptime ( cmbDate ,  "%d/%m/%Y_%H:%M:%S"  )
    
    lowDate <-  as.Date ("2007-02-01", "%Y-%m-%d")
    highDate <- as.Date ("2007-02-02", "%Y-%m-%d")
    
    #---------- filter which data to be used ----------------------------
    data  <-  subset( alldata, 
                      (alldata$Date >= lowDate) & (alldata$Date <= highDate)  )
    
    
    #----------  convert G_act_pow to numeric ---------------------------
    #---------   (done after reducing the number of rows --> faster) ----
    data$Global_active_power  <-  
        as.numeric(as.character(data$Global_active_power))
    
    #----------  convert G_act_pow to numeric ---------------------------
    #---------   (done after reducing the number of rows --> faster) ----
    #   data$Global_active_power  <- as.numeric(as.character(data$Global_active_power))
    
    data$Sub_metering_1  <-
        as.numeric ( as.character ( data$Sub_metering_1))
    data$Sub_metering_2  <-
        as.numeric ( as.character ( data$Sub_metering_2))
    data$Sub_metering_3  <-
        as.numeric ( as.character ( data$Sub_metering_3))
    
    data$Voltage  <-
        as.numeric ( as.character ( data$Voltage))

    data$Global_reactive_power  <-
        as.numeric ( as.character ( data$Global_reactive_power))
    
    
    
    png("plot4.png", width=480, height=480)
    
    
    
    #par()              # view current settings
    opar <- par()       # make a copy of current settings
    par(lty=1)          # use a solid line 
    par (mfrow=c(2,2))  #-- use 2x2 matrix
    
    
    # -----------  do empty plot -----------------------------------
    plot (  
        data$FullDate
        , data$Global_active_power 
        , xlab = "" 
        , ylab = "Global Active Power"
        , type ="n"

    )
      
    #----------  now draw the lines  --------------------------------
    lines (  
        data$FullDate , 
        data$Global_active_power 
    )
  
    ## ==============================================================
    plot(
        data$FullDate
        , data$Voltage
        , xlab = "datetime"
        , ylab = "Voltage"
        , type="n"
        , yaxt="n"
        )

    lines (  
        data$FullDate 
        , data$Voltage 
    )

    axis(
         2
         , at= seq(234, 246, by=4)
         , las=2
        )

    
    
    ## ==============================================================
    # -----------  do empty plot -----------------------------------
    plot (  
        data$FullDate , 
        data$Sub_metering_1, 
        xlab = "", 
        ylab = "Energy sub metering "
        , type ="n"
    )
    
    
    #----------  now draw the lines  --------------------------------
    lines (  
        data$FullDate 
        , data$Sub_metering_1 
        , col = "black"
    )
    lines (  
        data$FullDate
        , data$Sub_metering_2 
        , col="red"
    )    
    lines (  
        data$FullDate
        , data$Sub_metering_3
        , col="blue"
    )
    
    #--------- make legend ---------------------
    legend(
        "topright"
        , legend =  c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        , col=  c("black","red","blue")
        , lty=1
        , bty="n"
    )
    
    ## ==============================================================
    plot(
        data$FullDate
        , data$Global_reactive_power
        , xlab = "datetime"
        , ylab = "Global_reactive_power"
        , type="n"
    )
    
    lines (  
        data$FullDate 
        , data$Global_reactive_power 
    )
  

    #-------- finish the plotting, create output file ------------
    dev.off()
        
    #----------   restore the parameters -------------------------
    
    par(opar)          # restore original settings
    
    Sys.setlocale('LC_TIME', backkup_locale)

}


##==============================================================================

test4  <-  function (){
    print ("")
    print (" --------- starting plot4 TEST  function --------------------")
    print ("")
    print (plot4())
}


##  start the test
#test4()