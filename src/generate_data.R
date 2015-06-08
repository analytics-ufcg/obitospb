library(dplyr)
library(stringr)
library(chron)

df_obitos = read.csv("../data/obito_2006_2014.csv")

head(df_obitos)
df_obitos = df_obitos %>% filter(!is.na(DTNASC))

df_obitos$DTOBITO = str_pad(df_obitos$DTOBITO, 8, pad = "0")
df_obitos$DTNASC = str_pad(df_obitos$DTNASC, 8, pad = "0")

df_obitos = df_obitos %>% select(DTOBITO, HORAOBITO, DTNASC, IDADE, SEXO, RACACOR, ESTCIV, ESC, OCUP, CEPRES, LOCOCOR, CEPOCOR, CODESTOCOR, CODMUNOCOR, CAUSABAS)
df_obitos$CAT = substr(df_obitos$CAUSABAS, 1, 3)
df_obitos$CLASSE = substr(df_obitos$CAUSABAS, 1, 1)

df_causas = read.csv(file = "../data/CID-10-CATEGORIAS.CSV", encoding = "latin1", sep=";")
df_causas = df_causas %>% select(CAT, DESCRICAO)

df_final = merge(df_obitos, df_causas)

df_municipio = read.csv(file = "../data/tb_municip.csv", encoding = "latin1", sep=";")
df_municipio = df_municipio %>% select(CO_MUNICIP, CO_TIPO, DS_NOME, CO_REGIAO, IN_CAPITAL, NU_LATITUD, NU_LONGIT, NU_ALTITUD, NU_AREA)

df_final = inner_join(df_final, df_municipio, by = c("CODMUNOCOR" = "CO_MUNICIP"))

df_final$LOCATION  = paste(df_final$NU_LATITUD, ":", df_final$NU_LONGIT, sep="")
df_final           = filter(df_final, CODESTOCOR == 25) #filtrando paraiba

#tratando datas e subcategorias
df_final$DTOBITO = str_pad(df_final$DTOBITO, 8, pad = "0")
df_final$ANO     = as.numeric(substr(df_final$DTOBITO, 5, 8))
df_final$MES     = as.numeric(substr(df_final$DTOBITO, 3, 4))
df_final$DIA     = as.numeric(substr(df_final$DTOBITO, 1, 2))
df_final$DOW     = day.of.week(df_final$MES, df_final$DIA, df_final$ANO)
df_final$DTOBITO = as.Date(strptime(df_final$DTOBITO, format="%d%m%Y"))
df_final$WED     = is.weekend(df_final$DTOBITO)
df_final$SUB     = as.numeric(substring(df_final$CAT, 2))

write.table(df_final, file="../data/obitos-PB-2006_2014.dat", row.names=F)

