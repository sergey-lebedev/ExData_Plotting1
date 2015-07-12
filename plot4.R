library('data.table')
dateaxis <- function(sequence)
{
    sequence <- seq(min(subdata$Date), max(subdata$Date) + 1, by='days')
    n <- length(sequence)
    offset <- rep(0, n)
    for (i in (1:n)){
        offset[i] <- min(which(data$Date == sequence[i]))
    }
    offset <- offset - min(offset) + 1
    axis(side=1, labels=format(sequence, '%a'), at=offset)
}
filename <- 'household_power_consumption.txt'
data <- fread(filename, na.strings=c('?'))
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data$Global_active_power <- as.numeric(data$Global_active_power)
data$Voltage <- as.numeric(data$Voltage)
subdata <- subset(data, Date >= '2007-02-01' & Date <='2007-02-02')
sequence <- seq(min(subdata$Date), max(subdata$Date) + 1, by='days')
png('plot4.png', width=480, height=480)
par(mfrow=c(2, 2))
with(subdata, plot(Global_active_power, xlab='', ylab='Global Active Power', xaxt='n', type='l'))
dateaxis(sequence)
with(subdata, plot(Voltage, xlab='datetime', xaxt='n', type='l'))
dateaxis(sequence)
with(subdata, plot(Sub_metering_1, xlab='', ylab='Energy sub metering', xaxt='n', type='l'))
with(subdata, lines(Sub_metering_2, xaxt='n', type='l', col='red'))
with(subdata, lines(Sub_metering_3, xaxt='n', type='l', col='blue'))
dateaxis(sequence)
legend('topright', lwd=1, bty='n', col=c('black', 'red', 'blue'), legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'))
with(subdata, plot(Global_reactive_power, xlab='datetime', xaxt='n', type='l'))
dateaxis(sequence)
dev.off()
