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


###########################
# Create Histogram Plot 1 #
###########################

# Open PNG device
png(filename = "plot1.png")

# Create histogram
hist(dt$Global_active_power,
     col = "red",
     xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power"
     )

dev.off()