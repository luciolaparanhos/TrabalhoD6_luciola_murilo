
## Trabalho D6
## MONITORAMENTO UPA24H
## Lucíola Paranhos e Murilo Lordello

# lista de pacotes
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis","survey","srvyr","ggplot2","scales")
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

#importando bases UPA E PRODU??O SIA/SUS
controleupa <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6analise/trabalho final/controle_upa24h_ago2018.xlsx", skip = 0)
producaoupa <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6analise/trabalho final/producao_upa_13082018.xlsx", skip = 3)

controleupa <- controleupa [,c(1,2,5,6,7,8,10,11,13,17,24,25,37,38,40,41,42,43)]


library(tidyverse)


controleupa <- controleupa %>%
# resumindo op??es de status
  mutate(situacao = str_sub(Status_UPA, 1,2))

# TOTALiza  UPA por situação (status resumido)
Controle_status <- controleupa %>%
  group_by(situacao)%>%
    summarise(nr_UPA = n())%>%
    mutate(percent=prop.table(nr_UPA),
           situacao = case_when(
             situacao == "1."~"Ação preparatória",
             situacao == "2."~"Ação preparatória",
             situacao == "3."~"Em obras",
             situacao == "4 "~"Concluída",
             situacao == "4."~"Em funcionamento")
           )

# total e percentual de upa por porte e por UF
UPA_UFporte <- controleupa %>%
  group_by(UF,porte) %>%
   summarise(total= n())%>%
    mutate(percent=prop.table(total))


# total e percentual de upa por porte
upa_porte <- controleupa %>%
  group_by(porte)%>%
  summarise(total=n())%>%
  mutate(prop = prop.table(total))


#controle upa em funcionamento
upa_funcionamento <- controleupa %>%
  filter(str_detect(Status_UPA, "4.4|4.5"))%>%
  mutate(CNES = as.numeric(CNES))

# total e percentual de upa em funcionamento por porte e por UF
UPA_UFporte_func <- controleupa %>%
  filter(str_detect(Status_UPA, "4.4|4.5")) %>%
  group_by(UF,porte) %>%
   summarise(total= n())%>%
   mutate(percent=prop.table(total))

# total e percentual de upa em funcionamento por porte
upa_porte_func <- controleupa %>%
  filter(str_detect(Status_UPA, "4.4|4.5"))%>%
  group_by(porte)%>%
  summarise(total=n())%>%
  mutate(prop = prop.table(total))


install.packages('ggplot2')
# Loading
library(ggplot2)
library(scales)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold", hjust = 0.5)
  )
#grafico upa por porte
  barras_porte <- ggplot(upa_porte,aes(x="", y=total, fill=porte))+
  geom_bar(width = 1, stat = "identity")
  pizza_porte <- barras_porte + coord_polar("y", start=0)+
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = total/3 + c(0, cumsum(total)[-length(total)]), 
                  label = percent(total/sum(total))), size=4)+
  ggtitle("% DE UPA POR PORTE")
  
pizza_porte + blank_theme + theme(axis.text.x=element_blank())

 # GRAFICO UPA EM FUNCIONAMENTO por porte e UF
 barras_porte_f <- ggplot(upa_porte_func,aes(x="", y=total, fill=porte))+
  geom_bar(width = 1, stat = "identity")
 pizza_porte_func <- barras_porte_f + coord_polar("y", start=0)+
   theme(axis.text.x=element_blank())+
   geom_text(aes(y = total/3 + c(0, cumsum(total)[-length(total)]), 
                 label = percent(total/sum(total))), size=4)+
    ggtitle("% DE UPA EM FUNCIONAMENTO POR PORTE")
 pizza_porte_func + blank_theme + + theme(axis.text.x=element_blank())
 
 # GRAFICO UPA por porte e UF
 porte_estado <- ggplot(data=UPA_UFporte, aes(x=UF, y=total, fill=porte)) +
   geom_bar(stat="identity")
 porte_estado
 
 # GRAFICO UPA EM FUNCIONAMENTO por porte e UF
 porte_estado_func <- ggplot(data=UPA_UFporte_func, aes(x=UF, y=total, fill=porte)) +
    geom_bar(stat="identity")+
    ggtitle("TOTAL DE UPA EM FUNCIONAMENTO POR UF E POR PORTE")
    #plot.title=element_text(size=14, face="bold", hjust = 0.5)
 porte_estado_func
 
library(lubridate)
 
# datas que marcam evolu??o da implanta??o das upa, financiadas pelo Ministerio da Sa?de
# exclu?das as constru?das por gestores locais e/ou realizadas pela Caixa Economica Federal
evolucao_upa <- controleupa %>%
filter(progr!="2008 CEF",!is.na(nrproposta))%>%
select(nrproposta,UF,municipio, ano_habil_construcao,Data1aParcela,Data_OIS,data_conclusao,Ano_funcionamento)

#acrescenta coluna que mostra tempo gasto na execu??o da obra, em dias
  # a ideia ? tirar m?dia desse tempo por estado depois
evolucao_upa <- evolucao_upa %>%
mutate(Ano_funcionamento = as.character(Ano_funcionamento))%>%
mutate(tempo_obra = ymd(as.Date(data_conclusao)) - ymd(as.Date(Data_OIS))) #%>%
#mutate(tempo_funciona = as.Date(Ano_funcionamento) - as.Date(data_conclusao))
#mutate(Ano_funcionamento = c(Ano_funcionamento, "-07-01"))

evolucao_upa_uf <- evolucao_upa%>%
  filter(!is.na(tempo_obra))%>%
  group_by(UF)%>%
  summarise(em_obras = n(),
            media_meses_obra = round(mean(tempo_obra)/30),
            min_dias_obra = min(tempo_obra),
            max_dias_obra = max(tempo_obra)
            )

evolucao_upa [evolucao_upa$UF == "AL", c("UF", "municipio", "tempo_obra")]

  
# AN?LISE DE PRODU??O DAS UPA

# desempilhando dados de produ??o mensal das UPA, todos os procedimentos
producao_mes <- producaoupa %>%
  mutate(CNES=as.numeric(CNES))%>%
  group_by(CNES,UF,MUNIC,OPCAO_CUSTEIO, ANO_MES)%>%
    summarise(total_atend= sum(ATENDIMENTO))%>%
    spread(ANO_MES, total_atend, fill = NA)
head(producao_mes)

# identificando UPA sem produção
semproducao <- upa_funcionamento%>%
  anti_join(producao_mes)
head(semproducao)

#identifiquei 2 UPA que n?o estava indicadas como 'em funcionamento' mas que tinham produ??o
erro <- producao_mes%>%
  anti_join(upa_funcionamento)


atendimentos <- producaoupa %>%
 select(CNES, UF, MUNIC, OPCAO_CUSTEIO,DES_PROC, COD_PROC, ANO_MES, ATENDIMENTO)%>%
  filter(COD_PROC %in% c('0301060096','0301060029','0301060118','0301060100'))
  atendimentos_mes <- atendimentos %>%
  mutate(CNES=as.numeric(CNES))%>%
  group_by(CNES,UF,MUNIC,OPCAO_CUSTEIO, ANO_MES)%>%
  summarise(total_atend= sum(ATENDIMENTO))%>%
  spread(ANO_MES, total_atend, fill = NA)

#  atendimentos_mes_uf <- atendimentos_mes%>%
#   group_by()

