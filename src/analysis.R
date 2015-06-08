library("dplyr")
library("ggplot2")
library("googleVis")
library("Hmisc")

df_obitos           = read.table(file="data/obitos-PB-2006_2014.dat", header=T)

#selecionando agressoes: cid x85_y09
agressoes = df_obitos %>% filter((CLASSE =="Y" & SUB %in% seq(0, 9)) | (CLASSE =="X" & SUB %in% seq(85, 99)))
bala      = filter(agressoes, CAT %in% c("X93", "X94", "X95")) #X99 Y00
dplot     = bala %>% group_by(ANO, DS_NOME, DESCRICAO) %>% summarise(OCC=n()) %>% arrange(ANO, OCC)

Motion=gvisMotionChart(dplot, idvar="DS_NOME", timevar="ANO")
plot(Motion)

# percentual de mortes por dia da semana
dayofweek = agressoes %>% group_by(DOW) %>% summarise(FREQ = n() / nrow(agressoes) * 100)

p = ggplot(dayofweek, aes(as.factor(DOW), FREQ)) + geom_bar(stat="identity")
p = p + xlab("Day of week") + ylab("Occurrence (%)")
p = p + scale_x_discrete(breaks = seq(0, 6), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
p

# percentual de mortes por bala por dia da semana
dayofweek = bala %>% group_by(DOW) %>% summarise(FREQ = n() / nrow(bala) * 100)

p = ggplot(dayofweek, aes(as.factor(DOW), FREQ)) + geom_bar(stat="identity")
p = p + xlab("Day of week") + ylab("Occurrence (%)")
p = p + scale_x_discrete(breaks = seq(0, 6), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
p

# percentual de mortes por dia
byday = agressoes %>% group_by(DIA) %>% summarise(FREQ = n() / nrow(agressoes) * 100)

p = ggplot(byday, aes(DIA, FREQ)) + geom_line()
p = p + xlab("Day") + ylab("Occurrence (%)")
p

# percentual de mortes por bala por dia
byday = bala %>% group_by(DIA) %>% summarise(FREQ = n() / nrow(bala) * 100)

p = ggplot(byday, aes(DIA, FREQ)) + geom_line()
p = p + xlab("Day") + ylab("Occurrence (%)")
p

byday = bala %>% group_by(DIA) %>% summarise(FREQ = n())
dd    = data.frame(PERC=round(cumsum(byday$FREQ) / sum(byday$FREQ), digits=6), DAY=byday$DIA)

p = ggplot(dd, aes(DAY, PERC)) + geom_point()
p

byday = agressoes %>% group_by(DIA) %>% summarise(FREQ = n())
dd    = data.frame(PERC=round(cumsum(byday$FREQ) / sum(byday$FREQ), digits=6), DAY=byday$DIA)

p = ggplot(dd, aes(DAY, PERC)) + geom_line()
p
