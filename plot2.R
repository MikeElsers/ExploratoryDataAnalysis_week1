##  week 01:   plot examples


#---------- plot 2 function  ------------------------------------

plot2    <-  function (){

    
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


    png("plot2.png", width=480, height=480)
    
    #------------ modify the parameters  -----------------------------------
    
    #par()              # view current settings
    opar <- par()      # make a copy of current settings
    par(lty=1)         # use a solid line 


    # -----------  do empty plot -----------------------------------
    plot (  
             data$FullDate , 
        data$Global_active_power, 
        xlab = "", 
        ylab = "Global Active Power (kilowatts)"
        , type ="n"
        )

    
    #----------  now draw the lines  --------------------------------
    lines (  
        data$FullDate , 
        data$Global_active_power 
    )

    
    #---------   create the plot output  (PNG file) ----------------s
    dev.off()


    #----------   restore the parameters -------------------------

    par(opar)          # restore original settings

    Sys.setlocale('LC_TIME', backkup_locale)

}

##==============================================================================

test2  <-  function (){
    print ("")
    print (" --------- starting plot2 TEST  function --------------------")
    print ("")
    print (plot2())
}


##  start the test
#test2()