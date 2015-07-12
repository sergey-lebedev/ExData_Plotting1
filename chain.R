source('devisor.R')
source('refinery.R')
source('splitter.R')
args <- commandArgs(trailingOnly = TRUE)
#filename <- args[1]
#filename <- '60118.csv'
#filename <- 'PL0000000.csv'

chain_tension_sensor_name = 'Натяжение.цепи'
first_engine_sensor_name = 'Баровый.двигатель.1'
second_engine_sensor_name = 'Баровый.двигатель.2'
#chain_tension_sensor_name = 'F2_22'
#first_engine_sensor_name = 'E3_3'
#second_engine_sensor_name = 'E3_4'

get_ecdf <- function(x){
    y <- NULL
    k <- knots(ecdf(x))
    for (i in k){
        y <- append(y, sum(x < i, na.rm=TRUE))
    }
    y <- y/length(x)
    return(list('x'=k, 'y'=y))
}


interpolation2 <- function(x, y, dmin){
    # Нормировка
    x <- (x - min(x)) / (max(x) - min(x))
    y <- (y - min(y)) / (max(y) - min(y))
    m <- 7
    n <- length(x)
    k <- 0
    inodes <- 1
    for (i in c((m+1):(n-(m+1)))){
        yl <- y[(i - m):i]
        xl <- x[(i - m):i]
        yr <- y[i:(i + m)]
        xr <- x[i:(i + m)]
        c <- lm(yl ~ xl)
        left_slope <- atan(c$coefficients[2])
        c <- lm(yr ~ xr)
        right_slope <- atan(c$coefficients[2])
        if ((k%%2) == 0){
            if ((right_slope - left_slope) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)
            }
        }
        else{
            if ((left_slope - right_slope) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)
            }
        }
    }
    # Дополнение до нужного числа участков
    if (inodes[k] != n){
        inodes <- append(inodes, rep(n, (k%%2) + 1))
    }
    return(inodes)
}

get_centers <- function(x){
    centers <- NULL
    px <- get_ecdf(x)
    nodes <- interpolation2(px$x, px$y, atan(1)/2)
    nodes <- thinner(px$x, nodes, 0.35)
    n <- length(nodes)
    for (i in seq(1, (n - 3), 2)){
        indexes <- (x >= px$x[nodes[i]]) & (x <= px$x[nodes[i + 3]])
        center <- median(x[indexes], na.rm=TRUE)
        centers <- append(centers, center)
    }
    return(centers)
}

plot_centers <- function(x, centers){
    n <- length(x)
    plot(x)
    for (center in centers){
        lines(c(1, n), rep(center, 2), col='red')
    }
}

thinner <- function(x, inodes, d){
    n <- length(inodes)
    distances <- diff(x[inodes[2:(n-1)]])
    shortlist <- (distances < d)
    while (sum(shortlist) != 0){
        minlist <- (distances == min(distances))
        m <- c(2:(n-2))[minlist][1]
        inodes <- inodes[-c(m, m+1)]
        n <- length(inodes)
        distances <- diff(x[inodes[2:(n-1)]])
        shortlist <- (distances < d)
    }
    return(inodes)
}

for (filename in args){
    file <- strsplit(filename,'\\.')[[1]][1]
    # Загрузка данных
    table <- read.csv(filename, sep=',', header=TRUE)
    #table <- read.csv(filename, sep=';', header=TRUE)

    # Создание подпапки для сохранения результатов обработки данных
    dirname <- file
    try(dir.create(dirname))
    try(setwd(dirname))

    # Пересчёт значений на временной оси в интервалы
    table[,1] <- strtoi(strftime(table[,1], '%s'))
    time <- table[,1]

    # Извлечение имён столбцов
    n <- length(table)
    nodes <- names(table)[1:n]

    # Удаление выбросов
    for (i in 2:n){
        values <- table[,i]
        me <- median(values, na.rm=TRUE)
        ma <- mad(values, na.rm=TRUE)
        indexes <- (values > (me - 5*ma)) & (values < (me + 5*ma))
        table[,i][!indexes] <- NA
        gc()
    }

    #hist(table[,chain_tension_sensor_name])

    png(paste(paste('time-chain_tension', file, sep='_'), 'png', sep='.'))
    plot(Натяжение.цепи ~ Время, data=table, cex=.5)
    dev.off()

    # Устранение отсутствующих сигналов
    #values <- table[,'Баровый.двигатель.1']
    #indexes <- is.na(values)
    #table[,'Баровый.двигатель.1'][indexes] <- 0

    #values <- table[,'Баровый.двигатель.2']
    #indexes <- is.na(values)
    #table[,'Баровый.двигатель.2'][indexes] <- 0

    # Задание минимального значения тока двигателя при котором будет рассчитываться натяжение
    m <- length(time)
    # Получение значений токов отсечки
    # Для первого барового двигателя
    i_min_1 <- divisor(table[,first_engine_sensor_name])
    # Для второго барового двигателя
    i_min_2 <- divisor(table[,second_engine_sensor_name])
    print(i_min_1)
    print(i_min_2)

    # Получение усреднённого значения тока
    current <- refinery(table[,first_engine_sensor_name], table[,second_engine_sensor_name])
    i_min <- (i_min_1 + i_min_2)/2

    # Фильтрация датчика натяжения >= 0
    chain_tension <- table[,chain_tension_sensor_name]
    idx <- (chain_tension >= 0)
    chain_tension[idx] <- NA

    # Фильтрация показаний датчика натяжения при отсутствии натяжения
    idx <- (current < i_min) | is.na(current)
    chain_tension[idx] <- NA
    median_current <- median(current[!idx], na.rm=TRUE)

    # Разбиение показаний на рабочие интервалы
    sequence <- splitter(time, 600)

    # Выявление самого протяженного рабочего интервала
    sorted <- sort(strtoi(summary(sequence)[,1]), index.return=TRUE, decreasing=TRUE)

    # Удаление пустых последовательностей
    n <- length(sorted$ix)
    list <- NULL
    for (i in sorted$ix){
        if (!prod(is.na(chain_tension[sequence[[i]]]))){
            list <- append(list, i)
        }
    }

    # Пакетная обработка
    for (i in c(1:length(list))){
        print(i)
        png(paste(paste('list', i, sep='_'), 'png', sep='.'))
        subsequence <- sequence[[list[i]]]
        try(centers <- get_centers(chain_tension[subsequence]))
        n <- length(subsequence)
        plot(time[subsequence], chain_tension[subsequence])
        for (center in centers){
            lines(c(time[subsequence][1], time[subsequence][n]), rep(center, 2), col='red')
        }
        #kmeans(chain_tension[subsequence], centers)
        dev.off()
    }
    # Возвращение на уровень выше
    setwd('..')
}
