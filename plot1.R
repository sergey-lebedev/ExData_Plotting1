library('data.table')
filename <- 'household_power_consumption.txt'
data <- fread(filename)
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data$Global_active_power <- as.numeric(data$Global_active_power)
subdata <- subset(data, Date >= '2007-02-01' & Date <='2007-02-02')
png('plot1.png', width=480, height=480)
with(subdata, hist(Global_active_power, col='red', main='Global Active Power', xlab='Global Active Power (kilowatts)'))
dev.off()
