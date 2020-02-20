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

#################
# Create Plot 3 #
#################

png(filename = "plot3.png")

plot(dt$date_times,
     dt$Sub_metering_1,
     type = 'n',
     xaxt = 'n',
     ylab = "Energy sub metering",
     xlab = ""
     )

# Add first line
lines(dt$date_times,
      dt$Sub_metering_1
      )

# Add second line, red
lines(dt$date_times,
      dt$Sub_metering_2,
      col = "red"
      )

# Add third line, blue
lines(dt$date_times,
      dt$Sub_metering_3,
      col = "blue"
      )

# Create x axis with correct labels
axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat")
     )

# Add legend
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lty = 1
       )

dev.off()