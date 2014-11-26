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

    hist(table[,chain_tension_sensor_name])

    png(paste(paste('time-chain_tension', file, sep='_'), '.png', sep=''))
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
    m <- length(table[,'Время'])
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
    sequence <- splitter(table[,'Время'], 600)

    # Выявление самого протяженного рабочего интервала
    sorted <- sort(strtoi(summary(sequence)[,1]), index.return=TRUE, decreasing=TRUE)
    subsequence <- sequence[[sorted$ix[1]]]

    # Построение связи    
    plot(diff(current[subsequence]), diff(chain_tension[subsequence]))
    idx <- !is.na(diff(current[subsequence])) & !is.na(diff(chain_tension[subsequence]))
    cor(diff(current[subsequence])[idx], diff(chain_tension[subsequence])[idx])

    idx <- !is.na(chain_tension[subsequence]) & (current[subsequence] >= i_min)
    plot(table[,'Время'][subsequence], chain_tension[subsequence], type='l')
    plot(table[,'Время'][subsequence][idx], chain_tension[subsequence][idx], type='l')
    hist(chain_tension[subsequence], breaks=100)

    plot(table[,'Время'][subsequence][2:length(subsequence)], diff(chain_tension[subsequence]), type='l', col='2')
    lines(table[,'Время'][subsequence][2:length(subsequence)], diff(chain_tension[subsequence]) + 0.2*diff(current[subsequence]), type='l', col='5', ylim=c(-300, -170))

    # Удаление пустых последовательностей 
    n <- length(sorted$ix)
    list <- NULL
    for (i in sorted$ix){
        if (!prod(is.na(chain_tension[sequence[[i]]]))){
            list <- append(list, i)
        }
    }    

    # Обработка
    y <- get_ecdf(chain_tension[subsequence])
    plot(y)
    # 

    if(FALSE){
        idx <- (((table[,first_engine_sensor_name] > i_min_1) | 
                (table[,second_engine_sensor_name] > i_min_2)) &
                !is.na(table[,first_engine_sensor_name]) &
                !is.na(table[,second_engine_sensor_name]))
        current_time <- 0           
        i_time <- rep(0, m)
       
        for (i in 2:m){
            delta <- 0
            if (((table[,first_engine_sensor_name][i-1] > i_min_1 ) &
                 (table[,first_engine_sensor_name][i] > i_min_1) &
                 !is.na(table[,first_engine_sensor_name][i]) &
                 !is.na(table[,first_engine_sensor_name][i+1])
                 ) |
                ((table[,second_engine_sensor_name][i-1] > i_min_2 ) &
                 (table[,second_engine_sensor_name][i] > i_min_2) &
                 !is.na(table[,second_engine_sensor_name][i]) &
                 !is.na(table[,second_engine_sensor_name][i+1]))){
                delta <- table[,'Время'][i] - table[,'Время'][i-1]
            }
            current_time <- current_time + delta
            i_time[i] <- current_time
        }
        png(paste(paste('engine_time-chain_tension', file, sep='_'), '.png', sep=''))
        plot(i_time[idx], table[,chain_tension_sensor_name][idx], cex=.5)
        dev.off()
        }
    }
    # Возвращение на уровень выше
    setwd('..')    
}

get_ecdf <- function(x){
    y <- NULL
    k <- knots(ecdf(x))
    for (i in k){
        y <- append(y, sum(x < i, na.rm=TRUE))
    }
    y <- y/length(x)
    return(list('x'=k, 'y'=y))
}

interpolation <- function(x, y, dmin){
    # Нормировка
    x <- (x - min(x)) / (max(x) - min(x))
    y <- (y - min(y)) / (max(y) - min(y))
    lmin <- 2
    #dmin <- 0.0005
    m <- 15
    n <- length(x)
    k <- 0
    inodes <- 1
    ival <- 0
    global_median_store <- NULL
    local_median_store <- NULL
    for (i in c(2:(n-1))){
        global_median <- median(atan(diff(y[inodes[k + 1]:i])/diff(x[inodes[k + 1]:i])))
        local_median <- median(atan(diff(y[i:min(i + m, n)])/diff(x[i:min(i + m, n)])))
        #print(x[i])
        print(k)
        print(local_median)
        print(atan(diff(y[i:min(i + m, n)])/diff(x[i:min(i + m, n)])))
        print(global_median)
        global_median_store <- append(global_median_store, global_median) 
        local_median_store <- append(local_median_store, local_median)         
        if ((k%%2) == 0){
            if ((local_median - global_median) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)   
                ival <- append(ival, ival[k])
            }   
        }
        else{
            if ((global_median - local_median) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)
                ival <- append(ival, ival[k])
            }            
        }
        plot(x, y)
        l <- 0.2
        lines(c(x[i], x[i] + l*cos(global_median)), c(y[i], y[i] + l*sin(global_median)), col='red')
        lines(c(x[i], x[i] + l*cos(local_median)), c(y[i], y[i] + l*sin(local_median)), col='blue')
        #readline()
    }
    if (inodes[k] != n){
        inodes <- append(inodes, n)
    }
    print(inodes)
    return(list('inodes'=inodes, 
           'global_median'=global_median_store, 
           'local_median'=local_median_store)
    )      
}

interpolation2 <- function(x, y, dmin){
    # Нормировка
    x <- (x - min(x)) / (max(x) - min(x))
    y <- (y - min(y)) / (max(y) - min(y))
    #lmin <- 2
    m <- 7
    n <- length(x)
    k <- 0
    inodes <- 1
    #ival <- 0
    #left_slope_store <- NULL
    #right_slope_store <- NULL
    for (i in c((m+1):(n-(m+1)))){
        #left_slope <- atan((y[i] - y[i - m])/(x[i] - x[i - m]))
        #right_slope <- atan((y[i + m] - y[i])/(x[i + m] - x[i]))
        yl <- y[(i - m):i]
        xl <- x[(i - m):i]
        yr <- y[i:(i + m)]
        xr <- x[i:(i + m)]
        c <- lm(yl ~ xl)
        left_slope <- atan(c$coefficients[2])
        c <- lm(yr ~ xr)
        right_slope <- atan(c$coefficients[2])
        #print(x[i])
        #print(k)
        #print(left_slope)
        #print(right_slope)
        #left_slope_store <- append(left_slope_store, left_slope)
        #right_slope_store <- append(right_slope_store, right_slope)
        if ((k%%2) == 0){
            if ((right_slope - left_slope) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)   
                #ival <- append(ival, ival[k])
            }   
        }
        else{
            if ((left_slope - right_slope) > dmin){
                k <- k + 1
                inodes <- append(inodes, i)
                #ival <- append(ival, ival[k])
            }            
        }
        #plot(x, y)
        #l <- 0.2
        #lines(c(x[i], x[i] - l*cos(left_slope)), c(y[i], y[i] - l*sin(left_slope)), col='red')
        #lines(c(x[i], x[i] + l*cos(right_slope)), c(y[i], y[i] + l*sin(right_slope)), col='blue')
    }
    # Дополнение до нужного числа участков
    if (inodes[k] != n){
        inodes <- append(inodes, rep(n, (k%%2) + 1))
    }
    print(inodes)
    #return(list('inodes'=inodes, 'left_slope'=left_slope_store, 'right_slope'=right_slope_store))
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

#z <- interpolation(y$x, y$y, 0.0017)
nodes <- interpolation2(y$x, y$y, atan(1)/2)
nodes <- thinner(y$x, nodes, 0.35)
plot(y)
lines(y$x[nodes], y$y[nodes], type='b', col='red')
plot(y$x[2:length(y$x)], z$global_median)
lines(y$x[2:length(y$x)], z$local_median, col='red')
plot(y$x[2:length(y$x)], z$local_median - z$global_median)

subsequence <- sequence[[list[1]]]
centers <- get_centers(chain_tension[subsequence])
plot_centers(chain_tension[subsequence], centers)
kmeans(chain_tension[subsequence], centers)

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
        print(inodes)
    }
    return(inodes)
}
