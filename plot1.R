##  week 01:   plot examples


#---------- plot 1 function  ------------------------------------

plot1    <-  function (){

    #--  save the locale (we need to switch to US) -----------
    backkup_locale <- Sys.getlocale('LC_TIME')
    
    Sys.setlocale('LC_TIME', 'C')
    
    
    #-------  read the data ----------------------------------
    
    filename <-  "household_power_consumption.txt"
    alldata <- read.csv( filename,  sep = c(';')) 
    
    
    ##------------ convert date  and time --------------
    cmbDate <-  paste (  alldata$Date, "_", alldata$Time, sep="")

    alldata$Date  <-   as.Date( alldata$Date,  "%d/%m/%Y")
    alldata$FullDate  <-  strptime ( cmbDate ,  "%d/%m/%Y_%H:%M:%S"  )
    
    lowDate <-  as.Date ("2007-02-01", "%Y-%m-%d")
    highDate <- as.Date ("2007-02-02", "%Y-%m-%d")

    
    #---------- filter data -----------------------------------------
    data  <-  subset( alldata, (alldata$Date >= lowDate) & (alldata$Date <= highDate)  )

    data$Global_active_power  <-  as.numeric(as.character(data$Global_active_power))

    
    #-------- start plotting -------------------------------------
    png("plot1.png", width=480, height=480)

    
    #------------ create the histogram  -----------------------------------
    myhist <- hist (  data$Global_active_power, 
            col="red", 
            main="Global Active Power", 
            xlab="Global Active Power(kilowatts)"
    )

    
    #------------ housekeeping ---------------------------------------
    dev.off()

    Sys.setlocale('LC_TIME', backkup_locale) 
}

##==============================================================================

test1  <-  function (){
    print ("")
    print (" --------- starting plot1 TEST  function --------------------")
    print ("")
    print (plot1())
}


##  start the test
#test1()