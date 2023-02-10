library(readr)

# [LEITURA DA BASE DE DADOS PARA CADA ESTADO DO NORDESTE]

AL2015 <- read_delim("AL2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

AL2015["Estado"]<-("Alagoas")

BA2015 <- read_delim("BA2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

BA2015["Estado"]<-("Bahia")


CE2015 <- read_delim("CE2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

CE2015["Estado"]<-("Ceara")

MA2015 <- read_delim("MA2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

MA2015["Estado"]<-("Maranhao")


PB2015 <- read_delim("PB2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

PB2015["Estado"]<-("Paraiba")

PE2015 <- read_delim("PE2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

PE2015["Estado"]<-("Pernambuco")

PI2015 <- read_delim("PI2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

PI2015["Estado"]<-("Piaui")

RN2015 <- read_delim("RN2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

RN2015["Estado"]<-("Rio Grande do Norte")

SE2015 <- read_delim("SE2015.txt", ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",",encoding = "ISO-8859-1"),
                     col_types = cols(`Vl Remun Media (SM)` = col_number(), 
                                      `Vl Remun Media Nom` = col_number()), 
                     trim_ws = TRUE)

SE2015["Estado"]<-("Sergipe")


# Unindo as bases de dados

banco <- rbind(AL2015,BA2015,CE2015,MA2015,PB2015,PE2015,PI2015,RN2015,SE2015)

rm(AL2015,BA2015,CE2015,MA2015,PB2015,PE2015,PI2015,RN2015,SE2015)

# Selecionando variáveis de interesse

banco <- cbind(banco[,7],banco[,13:14],banco[,16],banco[,18:20],banco[,23:24],banco[,26],
               banco[,27],banco[,31],banco[,36],banco[,38:41],banco[,45],banco[,58])


# Selecionando indivíduos com vínculos em empresas não públicas

privada1 <- split(banco,banco$"Tipo Vinculo">=10 & banco$"Tipo Vinculo"<30)$'TRUE'
privada2 <- split(banco,banco$"Tipo Vinculo">=40 & banco$"Tipo Vinculo"<95)$'TRUE'
privada <- rbind(privada1, privada2)

rm(privada1, privada2, banco)

# ------------------------------------------------------------------------------------------------------------------

# [RETIRANDO ALGUMAS OBSERVAÇÕES]

# Motivo Desligamento
privada <- split(privada, privada$Motivo.Desligamento <22)$'TRUE'
nrow(privada)
table(privada$Motivo.Desligamento)

# Tempo de Emprego
privada <- split(privada, privada$Tempo.Emprego >0)$`TRUE`
nrow(privada)
summary(privada$Tempo.Emprego)

# Idade
privada <- split(privada,privada$Idade >17 & privada$Idade <69)$'TRUE'
nrow(privada)
summary(privada$Idade)

# Faixa Etária (character)
privada$Faixa.Etária <- as.numeric(privada$Faixa.Etária)
privada <- split(privada,privada$Faixa.Etária>2 & privada$Faixa.Etária<9)$'TRUE'
nrow(privada)


# ------------------------------------------------------------------------------------------------------------------

# [RECATEGORIZANDO VARIÁVEIS]

library(forcats)

# Faixa Etária
privada$Faixa.Etária <- fct_collapse(factor(privada$Faixa.Etária),
                                       # Ate 24 anos
                                       "faixa1"='3',
                                       # De 25 a 29 anos
                                       "faixa2"='4',
                                       # De 30 a 39 anos
                                       "faixa3"='5',
                                       # De 40 a 49 anos
                                       "faixa4"='6',
                                       # 50 anos ou mais
                                       "faixa5"=c('7','8'))

summary(privada$Faixa.Etária)
nrow(privada)

# Faixa Remuneração SM
table(privada$Faixa.Remun.Média..SM.)

privada$Faixa.Remun.Média..SM. <- fct_collapse(factor(privada$Faixa.Remun.Média..SM.),
                                                 # Ate 1 salario minimo
                                                 "Teto1" = c("0","1"),
                                                 # De 1 a 3 salarios minimos                            
                                                 "Teto2" = c("2","3","4"),
                                                 # De 3 a 7 salarios minimos
                                                 "Teto3" = c("5","6","7"),
                                                 # De 7 a 15 salarios minimos
                                                 "Teto4" = c("8","9"),
                                                 # Mais de 15 salarios minimos
                                                 "Teto5" = c("10","11"))

summary(privada$Faixa.Remun.Média..SM.)
nrow(privada)
privada<-rbind(split(privada,privada$Faixa.Remun.Média..SM.=="99")$`FALSE`)
nrow(privada)


# Tamanho/Porte da empresa 

table(privada$Tamanho.Estabelecimento)

privada$Tamanho.Estabelecimento <-fct_collapse(factor(privada$Tamanho.Estabelecimento),
                                                 # micro1
                                                 "Micro1"=c("1","2"),
                                                 # micro2
                                                 "Micro2"=c("3","4"),
                                                 # pequena
                                                 "Pequena"=c("5","6"),
                                                 # media
                                                 "Media"=c("7","8"),
                                                 # grande
                                                 "Grande"=c("9","10"))

summary(as.factor(privada$Tamanho.Estabelecimento))
nrow(privada)

# Raça e Cor

table(privada$Raça.Cor)

privada$Raça.Cor <- fct_collapse(factor(privada$Raça.Cor),
                                   # branca
                                   "Branca/Amarela" = c("2","6"),
                                   # preta
                                   "Preta/Parda" = c("4","8"),
                                   # parda
                                   #"Parda" = c("08"),
                                   # outro
                                   "Outro" = c("1","9"))

summary(as.factor(privada$Raça.Cor))
nrow(privada)

# Faixa Hora Contratual

table(privada$Faixa.Hora.Contrat)

privada$Faixa.Hora.Contrat<-fct_collapse(factor(privada$Faixa.Hora.Contrat),
                                           # Ate 30
                                           "FxHora1" = c("1","2","3","4"),
                                           # De 30 a 40
                                           "FxHora2" = c("5"),
                                           # De 41 a 44
                                           "FxHora3" = c("6"))

summary(as.factor(privada$Faixa.Hora.Contrat))
nrow(privada)

# Mês Admissão

table(privada$Mês.Admissão)

privada$Admitido <- fct_collapse(factor(privada$Mês.Admissão),
                                   # Admitido
                                   "Admitido"=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                                   # Não admitido
                                   "Não admitido"=c("0"))

summary(as.factor(privada$Admitido))
nrow(privada)

# Mês Desligamento

table(privada$Mês.Desligamento)

privada$Desligado <- fct_collapse(factor(privada$Mês.Desligamento),
                                    # Desligado
                                    "Desligado"= c("1","2","3","4","5","6","7","8","9","10","11","12"),
                                    # N?o desligado
                                    "Não desligado"= c("0"))

summary(privada$Desligado)
nrow(privada)

# Sexo

table(privada$Sexo.Trabalhador)

privada$Sexo.Trabalhador  <- fct_collapse(factor(privada$Sexo.Trabalhador),
                                            # Masculino 
                                            "M" = c("1"),
                                            # Feminino
                                            "F" = c("2"))

summary(as.factor(privada$Sexo.Trabalhador))
nrow(privada)



# Escolaridade

table(privada$Escolaridade.após.2005)

privada$Escolaridade.após.2005 <- fct_collapse(factor(privada$Escolaridade.após.2005),
                                               # Analfabeto
                                               "Analfabeto" = c("1"),
                                               # Ate 5.A Inc + 5.A CO Fund + 6. a 9. Fund
                                               "Fund. incompleto" = c("2", "3", "4"),
                                               # Fund Comp + Medio Incomp
                                               "Fund. completo" = c("5",   "6"),
                                               # Medio Comp + Superior Incomp
                                               "Médio completo" = c("7", "8"),
                                               # Superior Comp + Mestrado + Doutorado
                                               "Superior completo" = c("9","10", "11"))

summary(factor(privada$Escolaridade.após.2005))

# ------------------------------------------------------------------------------------------------------------------

banco <- privada
rm(privada)

# [ACRESCENTANDO VARIÁVEL REGIÃO COM CATEGORIAS CAPITAL, REGIÃO METROPOLITANA E INTERIOR]

banco["regiao"]<-("interior")

#ALAGOAS

banco$regiao[banco$Municipio == "270040"] <- ("metropole") #atalaia
banco$regiao[banco$Municipio == "270050"] <- ("metropole") #barra de sto antonio
banco$regiao[banco$Municipio == "270060"] <- ("metropole") #barra de sao miguel
banco$regiao[banco$Municipio == "270220"] <- ("metropole") #coqueiro seco
banco$regiao[banco$Municipio == "270430"] <- ("capital")   #maceio
banco$regiao[banco$Municipio == "270470"] <- ("metropole") #marechal deodoro
banco$regiao[banco$Municipio == "270520"] <- ("metropole") #messias
banco$regiao[banco$Municipio == "270550"] <- ("metropole") #murici
banco$regiao[banco$Municipio == "270644"] <- ("metropole") #paripueira
banco$regiao[banco$Municipio == "270690"] <- ("metropole") #pilar
banco$regiao[banco$Municipio == "270770"] <- ("metropole") #rio largo
banco$regiao[banco$Municipio == "270790"] <- ("metropole") #santa luzia do norte
banco$regiao[banco$Municipio == "270890"] <- ("metropole") #satuba


# BAHIA

banco$regiao[banco$Municipio == "290570"] <- ("metropole") # cama?ari
banco$regiao[banco$Municipio == "290650"] <- ("metropole")#candeias
banco$regiao[banco$Municipio == "291005"] <- ("metropole")# dias d'avila
banco$regiao[banco$Municipio == "291610"] <- ("metropole")# itaparica
banco$regiao[banco$Municipio == "291920"] <- ("metropole")#lauro de freitas
banco$regiao[banco$Municipio == "291992"] <- ("metropole")#madre de deus
banco$regiao[banco$Municipio == "292100"] <- ("metropole")#mata de s?o jo?o
banco$regiao[banco$Municipio == "292520"] <- ("metropole")#pojuca
banco$regiao[banco$Municipio == "292740"] <- ("capital")#salvador
banco$regiao[banco$Municipio == "292920"] <- ("metropole")#sao francisco do conde
banco$regiao[banco$Municipio == "292950"] <- ("metropole")#sao sebastiao do pass?
banco$regiao[banco$Municipio == "293070"] <- ("metropole")#simoes filho
banco$regiao[banco$Municipio == "293320"] <- ("metropole")#vera cruz


#CEARA

banco$regiao[banco$Municipio == "230100"] <- ("metropole") #aquiraz 
banco$regiao[banco$Municipio == "230350"] <- ("metropole")#cascavel
banco$regiao[banco$Municipio == "230370"] <- ("metropole")#caucaia
banco$regiao[banco$Municipio == "230395"] <- ("metropole")#chorozinho
banco$regiao[banco$Municipio == "230428"] <- ("metropole")#eusebio
banco$regiao[banco$Municipio == "230440"] <- ("capital")#fortaleza
banco$regiao[banco$Municipio == "230495"] <- ("metropole")#guaiuba
banco$regiao[banco$Municipio == "230523"] <- ("metropole")#horizonte
banco$regiao[banco$Municipio == "230625"] <- ("metropole")#itaitinga
banco$regiao[banco$Municipio == "230765"] <- ("metropole")#maracanau 
banco$regiao[banco$Municipio == "230770"] <- ("metropole")#maranguape
banco$regiao[banco$Municipio == "230960"] <- ("metropole")#pacajus
banco$regiao[banco$Municipio == "230970"] <- ("metropole")#pacatuba
banco$regiao[banco$Municipio == "231020"] <- ("metropole")#paracuru
banco$regiao[banco$Municipio == "231025"] <- ("metropole")#paraipaba
banco$regiao[banco$Municipio == "231085"] <- ("metropole")#pindoretama
banco$regiao[banco$Municipio == "231240"] <- ("metropole")#s?o gon?alo do amarante
banco$regiao[banco$Municipio == "231260"] <- ("metropole")#s?o luis do curu
banco$regiao[banco$Municipio == "231350"] <- ("metropole")#trairi


# MARANH?O

banco$regiao[banco$Municipio == "210125"] <- ("metropole") #bacabeira 
banco$regiao[banco$Municipio == "210510"] <- ("metropole")#icatu
banco$regiao[banco$Municipio == "210750"] <- ("metropole")#paco do lumiar
banco$regiao[banco$Municipio == "210945"] <- ("metropole")#raposa
banco$regiao[banco$Municipio == "210960"] <- ("metropole")#rosario
banco$regiao[banco$Municipio == "211020"] <- ("metropole")#santa rita
banco$regiao[banco$Municipio == "211120"] <- ("metropole")#s?o jos? do ribamar
banco$regiao[banco$Municipio == "211130"] <- ("capital")#s?o luis
banco$regiao[banco$Municipio == "210110"] <- ("metropole")#axix?
banco$regiao[banco$Municipio == "210237"] <- ("metropole")#cachoeira grande 
banco$regiao[banco$Municipio == "210710"] <- ("metropole")#morros
banco$regiao[banco$Municipio == "210920"] <- ("metropole")#presidente juscelino
banco$regiao[banco$Municipio == "210020"] <- ("metropole")#alcantara

# PARAIBA

banco$regiao[banco$Municipio == "250180"] <- ("metropole") #bayeux 
banco$regiao[banco$Municipio == "250320"] <- ("metropole")#cabedelo
banco$regiao[banco$Municipio == "250460"] <- ("metropole")#conde
banco$regiao[banco$Municipio == "250490"] <- ("metropole")#cruz do espirito santo
banco$regiao[banco$Municipio == "250750"] <- ("capital")#jo?o pessoa
banco$regiao[banco$Municipio == "250860"] <- ("metropole")#lucena
banco$regiao[banco$Municipio == "251290"] <- ("metropole")#rio tinto
banco$regiao[banco$Municipio == "251370"] <- ("metropole")#santa rita
banco$regiao[banco$Municipio == "250060"] <- ("metropole")#alhandra
banco$regiao[banco$Municipio == "250300"] <- ("metropole")#caapor? 
banco$regiao[banco$Municipio == "251120"] <- ("metropole")#pedras de fogo

# PERNAMBUCO

banco$regiao[banco$Municipio == "260005"] <- ("metropole")#abreu e lima 
banco$regiao[banco$Municipio == "260105"] <- ("metropole")#aracoiaba
banco$regiao[banco$Municipio == "260290"] <- ("metropole")#cabo de santo augostinho
banco$regiao[banco$Municipio == "260345"] <- ("metropole")#camaragibe
banco$regiao[banco$Municipio == "260620"] <- ("metropole")#goiana
banco$regiao[banco$Municipio == "260680"] <- ("metropole")#igarassu
banco$regiao[banco$Municipio == "260720"] <- ("metropole")#ipojuca
banco$regiao[banco$Municipio == "260775"] <- ("metropole")#itapissuma
banco$regiao[banco$Municipio == "260790"] <- ("metropole")#jaboat?o dos guararapes
banco$regiao[banco$Municipio == "260940"] <- ("metropole")#moreno 
banco$regiao[banco$Municipio == "260960"] <- ("metropole")#olinda
banco$regiao[banco$Municipio == "261070"] <- ("metropole")#paulista
banco$regiao[banco$Municipio == "261160"] <- ("capital")#recife
banco$regiao[banco$Municipio == "261370"] <- ("metropole")#s?o lourenco da mata

# PIAUI

banco$regiao[banco$Municipio == "220040"] <- ("metropole") #altos 
banco$regiao[banco$Municipio == "220160"] <- ("metropole")#beneditinos
banco$regiao[banco$Municipio == "220273"] <- ("metropole")#coivaras
banco$regiao[banco$Municipio == "220325"] <- ("metropole")#curralinhos
banco$regiao[banco$Municipio == "220330"] <- ("metropole")#demerval lob?o
banco$regiao[banco$Municipio == "220550"] <- ("metropole")#jose de freitas
banco$regiao[banco$Municipio == "220555"] <- ("metropole")#lagoa alegre
banco$regiao[banco$Municipio == "220558"] <- ("metropole")#lagoa do piaui
banco$regiao[banco$Municipio == "220630"] <- ("metropole")#miguel le?o
banco$regiao[banco$Municipio == "220640"] <- ("metropole")#monsenhor gil 
banco$regiao[banco$Municipio == "220672"] <- ("metropole")#naz?ria
banco$regiao[banco$Municipio == "220779"] <- ("metropole")#pau d'arco do piaui
banco$regiao[banco$Municipio == "221100"] <- ("capital")#teresina
banco$regiao[banco$Municipio == "221110"] <- ("metropole")#uni?o

# RIO GRANDE DO NORTE

banco$regiao[banco$Municipio == "240120"] <- ("metropole") #ar?s 
banco$regiao[banco$Municipio == "240170"] <- ("metropole")#bom jesus
banco$regiao[banco$Municipio == "240360"] <- ("metropole")#extremoz
banco$regiao[banco$Municipio == "240420"] <- ("metropole")#goianinha
banco$regiao[banco$Municipio == "240460"] <- ("metropole")#ielmo marinho
banco$regiao[banco$Municipio == "240710"] <- ("metropole")#maca?ba
banco$regiao[banco$Municipio == "240750"] <- ("metropole")#maxaranguape
banco$regiao[banco$Municipio == "240780"] <- ("metropole")#monte alegre
banco$regiao[banco$Municipio == "240810"] <- ("capital")#natal
banco$regiao[banco$Municipio == "240820"] <- ("metropole")#nisia floresta 
banco$regiao[banco$Municipio == "240325"] <- ("metropole")#parnamirim
banco$regiao[banco$Municipio == "241200"] <- ("metropole")#s?o gon?alo do amarante
banco$regiao[banco$Municipio == "241220"] <- ("metropole")#s?o jose de mipibu
banco$regiao[banco$Municipio == "241480"] <- ("metropole")#vera cruz

# SERGIPE 

banco$regiao[banco$Municipio == "280030"] <- ("capital") #aracaju 
banco$regiao[banco$Municipio == "280060"] <- ("metropole")#barra dos coqueiros
banco$regiao[banco$Municipio == "280480"] <- ("metropole")#nossa senhora do socorro
banco$regiao[banco$Municipio == "280670"] <- ("metropole")#s?o crist?v?o


# ------------------------------------------------------------------------------------------------------------------

# [RENOMEANDO VARIÁVEIS] 

colnames(banco)[1] <- 'motivodeslig'
colnames(banco)[2] <- 'fxetaria'
colnames(banco)[3] <- 'fxhoracontrat'
colnames(banco)[4] <- 'fxremunmedia'

colnames(banco)[5] <- 'escolaridade'
colnames(banco)[6] <- 'horascontratuais'
colnames(banco)[7] <- 'idade'
colnames(banco)[8] <- 'mesadmissao'

colnames(banco)[9] <- 'mesdeslig'
colnames(banco)[10] <- 'municipio'
colnames(banco)[11] <- 'nacionalidade'
colnames(banco)[12] <- 'raca.cor'

colnames(banco)[13] <- 'remunmedia'
colnames(banco)[14] <- 'sexo'
colnames(banco)[15] <- 'tamestab'
colnames(banco)[16] <- 'tempoemprego'

colnames(banco)[17] <- 'tipoadmissao'
colnames(banco)[18] <- 'tipovinculo'
colnames(banco)[19] <- 'estado'
colnames(banco)[20] <- 'admitido'

colnames(banco)[21] <- 'desligado'
colnames(banco)[22] <- 'regiao'

# ------------------------------------------------------------------------------------------------------------------

# [SALVANDO A BASE DE DADOS]

banco$motivodeslig <- as.factor(banco$motivodeslig)
banco$mesadmissao <- as.factor(banco$mesadmissao)
banco$mesdeslig <- as.factor(banco$mesdeslig)
banco$municipio <- as.factor(banco$municipio)

banco$nacionalidade <- as.factor(banco$nacionalidade)
banco$tipoadmissao <- as.factor(banco$tipoadmissao)
banco$tipovinculo <- as.factor(banco$tipovinculo)

# Considerando indivíduos com tempo de emprego menor ou igual a 420 meses
banco <- split(banco, banco$tempoemprego <= 420)$'TRUE'  # 9140878

# Salvando a base de dados
write.csv(banco, "banco.txt", row.names = FALSE)





