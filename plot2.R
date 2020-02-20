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

#############################
# Create Line Chart Plot #2 #
#############################

# Set variables to be used for x-axis labels later
start <- min(dt$date_times)
mid <- median(dt$date_times)
end <- max(dt$date_times)

png(filename = "plot2.png")

# Create plot
plot(dt$date_times,
     dt$Global_active_power,
     type = "n",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "Global Active Power (kilowatts)"
     )

# Draw the lines
lines(dt$date_times, dt$Global_active_power)

# Create the y axis
axis(side = 2)

# Create x axis with custom labels
axis(side = 1,
     at = c(start, mid, end),
     labels = c("Thu", "Fri", "Sat")
     )

dev.off()