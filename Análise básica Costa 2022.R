# Carregando os pacotes adequados
require(ggplot2)
require(dplyr)
require(RColorBrewer)
require(tidyr)
require(scales)
# require(ordinal) # Esse pacote faaz modelos ordinais não bayesianos, mas não usamos aqui
require(brms) # Esse o pacote para fazermos o modelo ordinal bayesiano
require(sjPlot) # Fundamental para extrair os efeitos marginais (última etapa deste tutorial)

# Carregando os dados

# Análise do experimento Escala Likert

# Transformando as variáveis de modo adequado
dados$Ordem<-as.factor(dados$Ordem)
dados$Num<-as.factor(dados$Num)
dados$answer<-as.factor(dados$answer)
dados$idade<-as.integer(dados$idade)
dados$genero<-as.factor(dados$genero)
dados$escolaridade<-as.factor(dados$escolaridade)
dados$idioma<-as.factor(dados$idioma)
dados$item<-as.factor(dados$item)

# Mudar os dados respostas para ordenados?!
dados$answer<-as.ordered(dados$answer)

# Contagem dos valores brutos:
contag <- dados %>%
  group_by(Ordem, Num, answer) %>%
  tally() %>%
  group_by(Ordem, Num) %>%
  spread(answer, n)

colnames(contag) <- c("Ordem", "Num", "Discordo_Totalmente", "Discordo", "Neutro", "Concordo", "Concordo_Totalmente")

# Importante: RESPOSTAS: há 3 x nº de sujeitos por condição, já que cada sujeito via 3 instâncias da mesma condição.

# Fazer porcentagens (tabela em formato horizontal)
porc_horiz <- dados %>%
  group_by(Ordem, Num, answer) %>%
  tally() %>%
  mutate(perc=n/sum(n)*100) %>%
  dplyr::select(-n) %>%
  group_by(Ordem, Num) %>%
  spread(answer, perc)

colnames(porc_horiz) <- c("Ordem", "Num", "Discordo_Totalmente", "Discordo", "Neutro", "Concordo", "Concordo_Totalmente")

################################################################################
# CÓDIGO PARA FAZER O GRÁFICO DE BARRAS DA TABELA DE PORCENTAGENS ACIMA
################################################################################

# Dividir o meio da escala (os julgamentos "Neutro"):
dados_meio <- porc_horiz %>%
  mutate(c1 = Neutro / 2,
         c2 = Neutro / 2) %>%
  dplyr::select(Ordem, Num, Discordo_Totalmente, Discordo, c1, c2, Concordo, Concordo_Totalmente) %>%
  gather(key = answer, value = perc, 3:8)

# Separando a escala em dois conjuntos, o "alto" e o "baixo":
meio_alto <- dados_meio %>%
  filter(answer %in% c("Concordo_Totalmente", "Concordo", "c2")) %>%
  mutate(answer = factor(answer, levels = c("Concordo_Totalmente", "Concordo", "c2"))) # Níveis na ordem normal!

meio_baixo <- dados_meio %>%
  filter(answer %in% c("c1", "Discordo", "Discordo_Totalmente")) %>%
  mutate(answer = factor(answer, levels = c("Discordo_Totalmente", "Discordo", "c1"))) # Níveis na ordem inversa!

# Estabelecimento de uma paleta de cores para organização!
legend_pal <- brewer.pal(name = "Spectral", n = 5) # Usar, do pacote RColorBrewer, a paleta de cores "spectral", com 5 cores 
legend_pal<-c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FFFFBF", "#FDAE61", "#D7191C") # Duplica a cor do meio manualmente
legend_pal <- gsub("#FFFFBF", "#9C9C9C", legend_pal) # Substituir a cor do meio por um cinza
names(legend_pal) <- c("Concordo_Totalmente", "Concordo", "c1", "c2", "Discordo", "Discordo_Totalmente") # Atribuir nomes às cores

# Estabelecendo o formato do eixo y nos gráficos de RT
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

# produzindo o gráfico, finalmente!
ggplot() + 
  geom_bar(data = meio_alto, aes(x = Num, y=perc, fill = answer), stat="identity") +
  geom_bar(data = meio_baixo, aes(x = Num, y=-perc, fill = answer), stat="identity") + 
  geom_hline(yintercept = 0, color =c("black")) +
  facet_wrap(~Ordem) +
  scale_y_continuous(breaks = seq(from = -100, to = 100, by = 10),
                     labels = scales::number_format(accuracy = 1)) +
  scale_fill_manual(values = legend_pal, 
                    breaks = c("Concordo_Totalmente", "Concordo", "c2", "Discordo", "Discordo_Totalmente"),
                    labels = c("Concordo_Totalmente", "Concordo", "Neutro", "Discordo", "Discordo_Totalmente")) +
  #coord_flip() +
  labs(x = "", y = "Porcentagem de Respostas (%)\n", fill = "Respostas") + 
  ggtitle("Painel 1: Distribuição percentual dos julgamentos na amostra",
          subtitle = "Barras empilhadas somam 100% cada") + 
  theme_classic()

################################################################################
# FIM DO CÓDIGO PARA FEITURA DO GRÁFICO
################################################################################
# Agora vamos de fato ajustar um modelo ordinal bayesiano

# Verificando quais os contrastes dos dados
contrasts(dados$Num)
contrasts(dados$Ordem)

# Mudar o nível-base para "um-todo" para facilitar a comparação entre as condições "um-todo" SG x PL
dados$Ordem<-relevel(dados$Ordem, ref = "um-todo")

# Calculando um modelo de regressão ordinal bayesiano
# Sem definição das priors ("flat priors")

# Há 3 modelos abaixo. Vamos rodar apenas o mais completo, pois demora um pouco

#m<-brm(answer ~ Ordem*Num + (1|subj) + (1|item), data= dados,
#       family = cumulative("logit", threshold = "flexible"))
#m1 <- brm(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1|item), data = dados,
#          family = cumulative("logit", threshold = "flexible"))
m2 <- brm(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1+Ordem*Num|item), data = dados,
          family = cumulative(link = "logit", threshold = "flexible")) # Probit não está funcionando... Não sei o motivo...

summary(m2)

################################################################################
# INÍCIO DO CÓDIGO PARA FEITURA DE GRÁFICO OPCIONAL COM AS ESTIMATIVAS
# ACHO POUCO PRÁTICO E COMPREENSÍVEL NESSE CASO
################################################################################

# Extrair os coefientes dos fatores fixos e intervalos de confiança calculados
fixos.m<-fixef(m2)[5:7,]

# Preparar um data frame com esses dados
colnames(fixos.m)<-c("Estimativas", "Est.Error", "lower", "upper")
fixos.m<-as.data.frame(fixos.m)

# Cálculo da razão de chance e das probabilidades
fixos.m<-fixos.m %>%
  mutate(OddsRatio=exp(Estimativas)) %>%
  mutate(Probs=OddsRatio/(1 + OddsRatio)*100)

rownames(fixos.m)<-c("ORDEM: todo x um", "NÚMERO: singular", "INTERAÇÃO: ordem x número")

# Preparando tabela para exportação
write.csv(fixos.m %>%
            mutate_if(is.numeric, round, digits=4) %>%
            mutate(Probs=round(Probs, 2)), "Efeitos_fixos.csv")

# Plotar o gráfico para publicação
fixos.m %>%
  ggplot(aes(x=reorder(rownames(fixos.m), Estimativas),
             y=Estimativas))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.0,
                position=position_dodge(.9))+
  geom_point(color="orange")+
  geom_text(aes(label=round(Estimativas, 3)), vjust=-1)+
  scale_x_discrete(labels=c("ORDEM: todo_um", "NÚMERO: Singular", "INTERAÇÃO: ordem x número"))+ # ao mudar a ordem é preciso mudar aqui
  labs(y = "LogOdds", x = "") + 
  ggtitle("Painel 1: Estimativas e intervalos de credibilidade (0.95)",
          subtitle = "Intervalos que não contêm zero são estatisticamente significativos") +
  coord_flip()+theme_classic()

# Interpretando: confesso que isso é muito difícil
# Não sei interpretar esses coeficientes

# Intercepto: um_todo PL
# Ordem: todo_um (SG) x um_todo PL (intercepto): ???
# num: (um_todo) SG x um_todo PL (intercepto): ???
# Interação: ???

# Por isso acho importante extrair os efeitos marginais
################################################################################
# FIM DO CÓDIGO PARA FEITURA DO GRÁFICO OPCIONAL
################################################################################

# Agora vamos extrair os valores que nos interessam do modelo:

# Extrair as probabilidades estimadas pelo modelo ("MARGINAL EFFECTS"):
# Referência aqui: https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_marginal_effects.html

model_data<-get_model_data(m2,
                           type = "pred",
                           terms = c("Ordem", "Num"),
                           ci.lvl = .95)

# The result is an object from the `ggeffects` class, as the `str` function shows
str(model_data)

# and the output is a little bit trickier to investigate and to plot (at least for my knowledge):
head(model_data)

# So, I decided to manually put these data in a "normal" `data.frame`:

model_data<-data.frame(ordem = model_data$x,
                       num = model_data$group,
                       Respostas = model_data$response.level,
                       Probabilidades = model_data$predicted,
                       lower = model_data$conf.low,
                       upper = model_data$conf.high)

# Notice that the factor `order` is a numeric vector (`1` or `2`). After investigating the original output, I realize
# that `1` stands for `um-todo` and `2` for `todo-um`. So I changed that vector in this fashion. I've tried to do it with `mutate_if`,
# from `dplyr` package, but I couldn't.

model_data$ordem<-c(rep("um-todo", 10), rep("todo-um", 10))

# Finally, I've transformed this character string into a factor:
model_data$ordem<-as.factor(model_data$ordem)

# Preparar uma paleta de cores condizente com a paleta usada no gráfico de barras empilhadas
paleta<-c("#D7191C", "#FDAE61", "#9C9C9C", "#ABDDA4", "#2B83BA")

# After all these steps, I finally could plot the graph the way I was trying to do, with
# and without credible intervals.

model_data %>%
  ggplot(aes(x=num, y=Probabilidades, group=Respostas, color=Respostas))+ # x = ordem ou num
  geom_line(position = position_dodge(0.3), alpha = 0.3, linetype = "dotted") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=0.2, position = position_dodge(0.3), alpha=0.3)+
  geom_point(position = position_dodge(0.3), size = 3, shape = 21, fill = "white", stroke = 1) +
  facet_wrap(~ordem)+ # num ou ordem
  scale_y_continuous(breaks = seq(from = 0, to = 1.0, by = 0.1),
                     labels = scales::label_percent(accuracy = 1))+
  scale_colour_manual(values=paleta,
                      labels=c("Discordo_Totalmente", "Discordo", "Neutro", "Concordo", "Concordo_Totalmente"))+
  labs(x = "Número da anáfora", y = "Probabilidades preditas\n", fill="Respostas")+ 
  ggtitle("Painel 2: Previsão de probabilidades estimada pelo modelo",
          subtitle = "Linhas verticais indicam intervalos de credibilidade preditos.")+ 
  theme_classic()+
  guides(colour = guide_legend(reverse=T)) # Apenas organizando a ordem da legenda.
