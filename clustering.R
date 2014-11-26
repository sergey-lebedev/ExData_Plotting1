args <- commandArgs(trailingOnly = TRUE)
print(args)

for (filename in args){
    file <- strsplit(filename,'\\.')[[1]][1]
    # Загрузка данных
    #table <- read.csv(file=filename, sep=';', header=TRUE)
    table <- read.csv(file=filename, sep=',', header=TRUE)

    # Создание подпапки для сохранения результатов обработки данных
    dirname <- file
    try(dir.create(dirname))
    try(setwd(dirname))

    # Предварительная обработка данных
    #table <- subset(table, table[,'E1_0'] < 10 | table[,'E1_0'] > 24)

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
        # Построение гистограмм
        svg(paste(nodes[i], '.svg', sep=''))
        try(plot(hist(values[indexes]), main=nodes[i])) 
        dev.off()
        gc()
    }

    # Инициализация матрицы смежности
    distances <- matrix(data = NA, nrow=length(nodes), ncol=length(nodes), dimnames = list(c(nodes), c(nodes)))

    # Создание папки для попарного вывода графиков
    try(dir.create('pairwise'))
    setwd('pairwise')

    for (i in 1:length(nodes)){
        for (j in i:length(nodes)){
            # Расчёт весов в матрице смежности
            idx <- !is.na(table[nodes[i]]) & !is.na(table[nodes[j]])
            metrics <- (1 - cor(table[nodes[i]][idx], table[nodes[j]][idx])^2)
            #metrics <- (1 - abs(cor(table[nodes[i]][idx], table[nodes[j]][idx])))
            #print(nodes[i])
            #print(nodes[j])
            #print(metrics)
            if (is.na(metrics)){
                metrics <- 1
            }
            distances[i, j] <- metrics
            distances[j, i] <- metrics

            # Попарное построение графиков
            pairname <- paste(i, j, sep='-')
            png(paste(pairname, '.png', sep=''))
            try(plot(table[nodes[i]][idx], table[nodes[j]][idx], xlab=nodes[i], ylab=nodes[j]))
            dev.off()
            gc()
        }
    }

    # Возвращение на уровень выше
    setwd('..')    

    # Сохранение дендрограммы
    distance <- as.dist(distances)
    svg(paste(file, '.svg', sep=''), width=21, height=6)
    #svg(paste(file, '.svg', sep=''))
    plot(hclust(distance), main=filename)
    dev.off()
    gc()

    # Возвращение в корневую папку
    setwd('..')
}
