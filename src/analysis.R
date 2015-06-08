library("dplyr")
library("ggplot2")
library("googleVis")

df_obitos           = read.table(file="data/obitos-PB-2006_2014.dat", header=T)

#selecionando agressoes: cid x85_y09
agressoes = df_obitos %>% filter((CLASSE =="Y" & SUB %in% seq(0, 9)) | (CLASSE =="X" & SUB %in% seq(85, 99)))
bala      = filter(agressoes, CAT %in% c("X93", "X94", "X95")) #X99 Y00
dplot     = bala %>% group_by(ANO, DS_NOME, DESCRICAO) %>% summarise(OCC=n()) %>% arrange(ANO, OCC)

Motion=gvisMotionChart(dplot, idvar="DS_NOME", timevar="ANO")
plot(Motion)

# bala %>% group_by(WED) %>% summarise(FREQ = n() / nrow(bala) * 100)
# bala %>% group_by(DOW) %>% summarise(FREQ = n() / nrow(bala) * 100)

# agressoes %>% group_by(WED) %>% summarise(FREQ = n() / nrow(bala) * 100)
dayweek = agressoes %>% group_by(DOW) %>% summarise(FREQ = n() / nrow(bala) * 100)

