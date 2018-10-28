
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

#importando bases UPA E PRODUÇÃO SIA/SUS
# controleupa - Dados administrativos da Coordenação de Urgência do Ministério da Saúde
# registra as propostas de construção de UPA
# producaoupa - Extração com Tabwin do Sistema SIA/SUS do Ministério da Saúde
# registra a produção das UPA que estão em funcionamento
controleupa <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6analise/trabalho final/controle_upa24h_ago2018.xlsx", skip = 0)
producaoupa <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6analise/trabalho final/producao_upa_13082018.xlsx", skip = 3)

controleupa <- controleupa [,c(1,2,5,6,7,8,10,11,13,17,24,25,37,38,40,41,42,43)]


# resumindo opÇÕES de status
controleupa <- controleupa %>%
  mutate(situacao = str_sub(Status_UPA, 1,2),
         situacao = case_when(
           situacao == "1."~"Ação preparatória",
           situacao == "2."~"Ação preparatória",
           situacao == "3."~"Em obras",
           situacao == "4 "~"Concluída",
           situacao == "4."~"Em funcionamento")
  )

# total e percentual de upa por situação e por UF
UPA_UF_situacao <- controleupa %>%
  group_by(UF,situacao) %>%
  summarise(total= n())%>%
  mutate(percent=prop.table(total))


# total e percentual de upa por ano da proposta e por situação
UPA_progr_situa <- controleupa %>%
  filter(!progr== "SES/SMS")%>%
  group_by(progr,situacao) %>%
    summarise(total= n())%>%
    mutate(percent=prop.table(total))

# GRAFICO UPA por situação e ano da proposta
UPA_progr <- ggplot(data=UPA_progr_situa, aes(x=progr, y=total, fill=situacao)) +
  geom_bar(stat="identity")+
  ggtitle("TOTAL DE UPA POR ANO PROPOSTA E SITUAÇÃO DE IMPLANTAÇÃO")+
  labs(x="Ano proposta", y = "Nº de UPA")
UPA_progr


# GRAFICO UPA por situação e UF
situacao_estado <- ggplot(data=UPA_UF_situacao, aes(x=UF, y=total, fill=situacao)) +
  geom_bar(stat="identity")+
  ggtitle("TOTAL DE UPA POR UF E SITUAÇÃO DE IMPLANTAÇÃO")+
  labs(x="UF", y = "Nº de UPA")
situacao_estado

# TOTALiza  UPA por situação (status resumido)
Controle_status <- controleupa %>%
  group_by(situacao)%>%
    summarise(nr_UPA = n())%>%
    mutate(percent=round(prop.table(nr_UPA)*100,1) )

#Controle_status

View(Controle_status)

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
  barras_porte <-ggplot(upa_porte, aes(x = porte, y = total, fill = porte)) + geom_bar(stat = "identity")+
  ggtitle("TOTAL DE UPA POR PORTE")
  barras_porte
  
   # GRAFICO UPA EM FUNCIONAMENTO por porte e UF
 barras_porte_f <- ggplot(upa_porte_func,aes(x="", y=total, fill=porte))+
  geom_bar(width = 1, stat = "identity")
 pizza_porte_func <- barras_porte_f + coord_polar("y", start=0)+
   theme(axis.text.x=element_blank())+
   geom_text(aes(y = total/3 + c(0, cumsum(total)[-length(total)]), 
                 label = percent(total/sum(total))), size=4)+
    ggtitle("% DE UPA EM FUNCIONAMENTO POR PORTE")
 pizza_porte_func + blank_theme + theme(axis.text.x=element_blank())
 
 # GRAFICO UPA por porte e UF
 porte_estado <- ggplot(data=UPA_UFporte, aes(x=UF, y=total, fill=porte)) +
   geom_bar(stat="identity")
 porte_estado
 
 # GRAFICO UPA EM FUNCIONAMENTO por porte e UF
 porte_estado_func <- ggplot(data=UPA_UFporte_func, aes(x=UF, y=total, fill=porte)) +
    geom_bar(stat="identity")+
    ggtitle("TOTAL DE UPA EM FUNCIONAMENTO POR UF E POR PORTE")+
  labs(x="", y = "Nº de UPA")
    porte_estado_func
 
library(lubridate)
 
# datas que marcam evolução da implantação das upa, financiadas pelo Ministerio da Saúde
# excluídas as construídas por gestores locais e/ou realizadas pela Caixa Economica Federal
evolucao_upa <- controleupa %>%
filter(progr!="2008 CEF",!is.na(nrproposta))%>%
select(nrproposta,UF,municipio, ano_habil_construcao,Data1aParcela,Data_OIS,data_conclusao,Ano_funcionamento)

#acrescenta coluna que mostra tempo gasto na execução da obra, em dias
# a ideia é tirar média desse tempo por estado depois
evolucao_upa <- evolucao_upa %>%
mutate(Ano_funcionamento = as.character(Ano_funcionamento))%>%
mutate(tempo_obra = ymd(as.Date(data_conclusao)) - ymd(as.Date(Data_OIS))) #%>%

# mostra tempo média de obra por estado
evolucao_upa_uf <- evolucao_upa%>%
  filter(!is.na(tempo_obra))%>%
  group_by(UF)%>%
  summarise(em_obras = n(),
            media_meses_obra = round(mean(tempo_obra)/30),
            min_dias_obra = min(tempo_obra),
            max_dias_obra = max(tempo_obra))

  
# ANáLISE DE PRODUçãO DAS UPA

# desempilhando dados de produção mensal das UPA, todos os procedimentos
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

#identifiquei 2 UPA que não estava indicadas como 'em funcionamento' mas que tinham produ??o
erro <- producao_mes%>%
  anti_join(upa_funcionamento)


#Filtra só os procedimentos de atendimento médico
atendimentos <- producaoupa %>%
 select(CNES, UF, MUNIC, OPCAO_CUSTEIO,DES_PROC, COD_PROC, ANO_MES, ATENDIMENTO)%>%
  filter(COD_PROC %in% c('0301060096','0301060029','0301060100'))

#Desempilha dados de atendimento da tabela produção
# mostra atendimentos de cada upa nos meses
atendimentos_mes <- atendimentos %>%
 mutate(CNES = as.numeric(CNES)) %>%
  group_by(CNES,UF,MUNIC,OPCAO_CUSTEIO, ANO_MES)%>%
  summarise(total_atend= sum(ATENDIMENTO))%>%
  spread(ANO_MES, total_atend, fill = 0)
    

# mostra média de atendimentos de cada upa no semestre
atendimentos_mes2 <- atendimentos %>%
  mutate(CNES = as.numeric(CNES)) %>%
  group_by(CNES,UF,MUNIC,OPCAO_CUSTEIO)%>%
  summarise(media_cnes_sem = round(sum(ATENDIMENTO)/6,0) )
 
  
# mostra média de atendimentos de cada estado por PORTE no semestre
# FILTRA Somente as opções III, V e VIII de custeio, que equivalem aos portes I, II e III
atendimentos_mes_uf2 <- atendimentos_mes2 %>%
  filter(OPCAO_CUSTEIO %in% c('III','V','VIII'))%>%
  group_by(UF,OPCAO_CUSTEIO)%>%
  summarise(nr_upa = n(),
            total_media = round(sum(media_cnes_sem)/nr_upa,0)) %>%
  spread(OPCAO_CUSTEIO,total_media, fill=0)%>%
  group_by(UF)%>%
    summarise(nr_upa = sum(nr_upa),
                 III = sum(III),
                   V = sum(V),
               VIII = sum(VIII))


# grafico boxplot da medIa atendimento por uf e porte I
atend_porteI <- atendimentos_mes2 %>%
  filter(OPCAO_CUSTEIO == "III")%>% 
  filter(MUNIC != "Itanhaém")%>%    #outlier
  filter(media_cnes_sem > 100)      #outlier (5 casos)
  box_atend_uf1 <- ggplot(atend_porteI, aes(x = UF, y = media_cnes_sem, fill=UF))+
  geom_boxplot()+  theme(legend.position="none")+
    labs(title="Média mensal de atendimentos por UF - UPA PORTE I",x="", y = "Média Atendimentos/mês")+
    geom_hline(yintercept = 4500, linetype="dashed", color = "red")
  box_atend_uf1

# grafico boxplot da medIa atendimento por uf e porte II
  atend_porteII <- atendimentos_mes2 %>%
    filter(OPCAO_CUSTEIO == "V")%>%
    filter(MUNIC != "Parnamirim")%>%   #outlier
    filter(media_cnes_sem > 350)      #outlier (2 casos)
    box_atend_uf2 <- ggplot(atend_porteII, aes(x = UF, y = media_cnes_sem,fill=UF))+
    geom_boxplot()+ theme(legend.position="none")+
      labs(title="Média mensal de atendimentos por UF - UPA PORTE II",x="", y = "Média Atendimentos/mês")+
      geom_hline(yintercept = 6750, linetype="dashed", color = "red")
    box_atend_uf2
  

# grafico boxplot da medIa atendimento por uf e porte III
  atend_porteIII <- atendimentos_mes2 %>%
    filter(OPCAO_CUSTEIO == "VIII")%>%
    filter(MUNIC != "Duque de Caxias")%>%  #outlier (2 casos)
    filter(media_cnes_sem > 600)          #outlier (3 casos)
    box_atend_uf3 <- ggplot(atend_porteIII, aes(x = UF, y = media_cnes_sem, fill=UF))+
    geom_boxplot()+ theme(legend.position="none")+
     labs(title="Média mensal de atendimentos por UF - UPA PORTE III",x="", y = "Média Atendimentos/mês")+
      geom_hline(yintercept = 10125, linetype="dashed", color = "red")
    box_atend_uf3
  


