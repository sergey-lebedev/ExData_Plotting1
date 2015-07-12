library('data.table')
dateaxis <- function(sequence)
{
    n <- length(sequence)
    offset <- rep(0, n)
    for (i in (1:n)){
        offset[i] <- min(which(data$Date == sequence[i]))
    }
    offset <- offset - min(offset) + 1
    axis(side=1, labels=format(sequence, '%a'), at=offset)
}
filename <- 'household_power_consumption.txt'
data <- fread(filename)
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data$Global_active_power <- as.numeric(data$Global_active_power)
subdata <- subset(data, Date >= '2007-02-01' & Date <='2007-02-02')
png('plot2.png', width=480, height=480)
with(subdata, plot(Global_active_power, xaxt='n', xlab='', ylab='Global Active Power (kilowatts)',  type='l'))
sequence <- seq(min(subdata$Date), max(subdata$Date) + 1, by='days')
dateaxis(sequence)
dev.off()
