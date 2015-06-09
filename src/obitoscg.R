library("dplyr")
library("ggplot2")
library("googleVis")
library("Hmisc")

df_obitos           = read.table(file="../data/obitos-PB-2006_2014.dat", header=T)

dfcg = df_obitos %>% filter(DS_NOME == "Campina Grande")


#selecionando agressoes: cid x85_y09
agressoes = dfcg %>% filter((CLASSE =="Y" & SUB %in% seq(0, 9)) | (CLASSE =="X" & SUB %in% seq(85, 99)))
bala      = filter(agressoes, CAT %in% c("X93", "X94", "X95")) #X99 Y00
faca      = filter(agressoes, CAT %in% c("X99", "Y00"))


dplot = bala %>% group_by(ANO, DIA) %>% summarise(FREQ = n())

p = ggplot(dplot, aes(DIA, FREQ)) + geom_line()
p = p + facet_wrap( ~ ANO)
p = p + scale_x_discrete(breaks = seq(1, 31, 5))
p = p + xlab("Day of month") + ylab("Occurrence")
p


dplot = rbind(data.frame(bala, TIPO="bala"), data.frame(faca, TIPO="face")) %>% group_by(ANO, MES, TIPO) %>% summarise(FREQ = n())

colnames(agressoes)

p = ggplot(dplot, aes(MES, FREQ, colour=TIPO)) + geom_point(size=2)
p = p + facet_wrap( ~ ANO)
p = p + scale_colour_hue("Tipo")
p = p + scale_x_discrete(breaks = seq(1, 31, 5))
p = p + xlab("Day of month") + ylab("Occurrence")
p

dplot = rbind(data.frame(bala, TIPO="bala"), data.frame(faca, TIPO="face")) %>% group_by(ANO, MES, DIA, TIPO) %>% summarise(FREQ = n())

colnames(agressoes)

p = ggplot(dplot, aes(DIA, FREQ, colour=TIPO)) + geom_point(size=2)
p = p + facet_grid(ANO ~ MES)
p = p + scale_colour_hue("Tipo")
p = p + scale_x_discrete(breaks = seq(1, 31, 5))
p = p + xlab("Day of month") + ylab("Occurrence")
p

p = ggplot(dplot, aes(as.factor(MES), FREQ, colour=TIPO)) + geom_boxplot(position="dodge")
p = p + facet_grid(ANO ~ .)
p = p + scale_colour_hue("Tipo")
p = p + xlab("Day of month") + ylab("Occurrence")
p


