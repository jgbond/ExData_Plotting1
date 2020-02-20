##################
# Load libraries #
##################

library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)

##########################
# Load and clean up data #
##########################

unzip("data/household_power_consumption.zip", exdir = "data")
rawdata <- "data/household_power_consumption.txt"

# Convert raw data to table
data <- read.table(rawdata, header = TRUE, sep = ";")
dt <- tbl_df(data)

# Convert date and time variables into single date-and-time variable
dt <- mutate(dt,
             date_times = as.POSIXct(paste(data$Date, data$Time),
                                     format = "%d/%m/%Y %H:%M:%S")
             )

# Filter by selected dates 
dt <- filter(dt, date_times >= "2007-02-01" & date_times <= "2007-02-02")

# Remove old date and time variables
dt <- select(dt, date_times, Global_active_power:Sub_metering_3)

# Convert all other variable classes from factor to numeric
dt <- mutate_if(dt, is.factor, as.character)
dt <- mutate_if(dt, is.character, as.numeric)

# Set variables to be used for x-axis labels later
start <- min(dt$date_times)
mid <- median(dt$date_times)
end <- max(dt$date_times)

#################
# Create Plot 4 #
#################

png(filename = "plot4.png")

# Set initial parameters
par(mfcol = c(2, 2),
    cex = 0.75,
    mai = c(0.75, 0.75, 0.5, 0.5))

# Create Plot 4.1

plot(dt$date_times,
     dt$Global_active_power,
     type = "n",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "Global Active Power"
     )

lines(dt$date_times, dt$Global_active_power)

axis(side = 2)

axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat"))

plot(dt$date_times,
     dt$Sub_metering_1,
     type = 'n',
     xaxt = 'n',
     ylab = "Energy sub metering",
     xlab = ""
     )

lines(dt$date_times,
      dt$Sub_metering_1)

lines(dt$date_times,
      dt$Sub_metering_2,
      col = "red")

lines(dt$date_times,
      dt$Sub_metering_3,
      col = "blue")

axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat")
     )

legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lty = 1,
       box.lty = 0
       )

# Create Plot 4.3

plot(dt$date_times,
     dt$Voltage,
     type = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Voltage"
     )

lines(dt$date_times,
      dt$Voltage)

axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat")
     )

title(xlab = "datetime", mgp = c(2.5, 1, 0))

# Create Plot 4.4

plot(dt$date_times,
     dt$Global_reactive_power,
     type = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Global_reactive_power"
     )

lines(dt$date_times,
      dt$Global_reactive_power)

axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat")
     )

title(xlab = "datetime", mgp = c(2.5, 1, 0))

dev.off()