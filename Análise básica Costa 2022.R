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

dados <- read.csv("https://raw.githubusercontent.com/igordeo-costa/ModelosOrdinais/main/ScalaLikertCosta2022.csv")

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
  mutate(perc=n/sum(n)) %>%
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
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = .1),
                     labels = function(x) percent(abs(x))) + # Definir os valores negativos da legenda como positivos
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
#       family = cumulative("probit", threshold = "flexible")) # Esse roda com probit
#m1 <- brm(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1|item), data = dados,
#          family = cumulative("probit", threshold = "flexible")) # Roda com probit, mas com avisos...
m2 <- brm(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1+Ordem*Num|item), data = dados,
          family = cumulative(link = "logit", threshold = "flexible")) # Esse não roda com probit... Não sei o motivo...

summary(m2)

pp_check(m2, ndraws = 100)

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
  geom_point(color="orange", size = 3)+
  geom_text(aes(label=round(Estimativas, 3)), vjust=-1)+
  scale_x_discrete(labels=c("ORDEM: todo_um", "NÚMERO: Singular", "INTERAÇÃO:\n\ntodo_um x singular"))+ # ao mudar a ordem é preciso mudar aqui
  labs(y = "LogOdds", x = "") + 
  ggtitle("Painel 1: Estimativas e intervalos de credibilidade (0.95)",
          subtitle = "Intervalos que não contêm zero são estatisticamente significativos") +
  coord_flip()+theme_classic()

# Interpretando...

# Primeiro, devemos lembrar o contrastes do modelo.
# Neste caso estamos usando dummy code (0 e 1)
contrasts(dados$Num)
contrasts(dados$Ordem)

# As categorias codificadas com 0 (zero) formam a base contra as quais as demais serão contrastadas:
# Neste caso, um_todo (0) PL (0)

# O preditor Ordem: todo_um: -2.47 logOdds indica o contraste todo_um (1) PL (0) x um_todo (0) PL (0)
# Ou seja, mantendo-se o número constante, o efeito da ordem: todo_um aumenta a probabilidade de se DESCER na escala

# O preditor Número: singular: +1.62 logOdds indica o contraste um_todo (0) SG (1) x um_todo (0) PL (0)
# Ou seja, mantendo-se a ordem constante, o efeito do número: singular aumenta a probabilidade de se SUBIR na escala

# A interação Ordem x Número (todo_um singular): +1.66 logOdds indica que:
# Os níveis da variável Ordem interagem com os níveis da variável Número
# Basicamente:
# o nível singular aumenta a probabilidade de SUBIR na escala com todo_um
# o nível plural aumenta a probabilidade de DESCER na escala com todo_um

# IMPORTANTE!
# O fato de haver um efeito de interação aponta para olharmos com cuidado para os efeitos de Ordem e de Número
# Se não houvesse interação, o efeito de um nível da variável seria o mesmo para ambos os níveis da outra
# Ver explicação abaixo, se quiser saber mais.

################################################################################
# Explicação simples do efeito de interação
################################################################################
# Valores abaixo são inventados, apenas para ilustração
data.frame(.ordem = c("um_todo", "um_todo", "todo_um", "todo_um"),
.num = c("sg", "pl", "sg", "pl"),
.porc = c(.8, .6, .7, .5), # Troque o .2 (quando há interação) por .5 (quando não há interação) e compare os dois gráficos
.sd = c(rep(.02, 4))) %>%
  ggplot(aes(x = .ordem, y = .porc, group = .num, color = .num)) +
  geom_errorbar(aes(ymin = .porc-.sd, ymax = .porc+.sd), width = .05) +
  geom_point(size = 4) + geom_line() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(from = 0, to = 1, by = .1))

# Mudar de plural para singular (ou vice-versa):
# Quando a ordem é um_todo, a mudança é de 20% (80 - 60)
# Quando a ordem é todo_um, a mudança é de 50% (70 - 20)

# Mudar de um_todo para todo_um (ou vice-versa):
# Quando o número é singular, a mudança é de 10% (80 - 70)
# Quando o número é plural, a mudança é de 40% (60 - 20)

# Observe que o "efeito de interação" é sempre o mesmo: 30%
# 50 - 20 = 30
# 40 - 10 = 30
# Por isso, não importa o modelo que você ajuste para esses dados...
# ...com quaisquer contrastes o valor da interação será o mesmo!

# Se você mudou o .2 para .5 no exemplo acima, viu que:
# Mudar de plural para singular não é afetado pela ordem, que é 20% em ambos os casos (20 - 20 = 0% de alteração de um nível para outro)
# Mudar de um_todo para todo_um não é afetado pelo número, que é 10% em ambos os casos (10 - 10 = 0% de alteração de um nível para outro)
# Logo: não há efeito de interação!
################################################################################
# Fim da explicação simples do efeito de interação
################################################################################



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
  ggplot(aes(x=ordem, y=Probabilidades, group=Respostas, color=Respostas))+ # x = ordem ou num
  geom_line(position = position_dodge(0.3), alpha = 0.3, linetype = "dotted") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=0.2, position = position_dodge(0.3), alpha=0.3)+
  geom_point(position = position_dodge(0.3), size = 3, shape = 21, fill = "white", stroke = 1) +
  facet_wrap(~num)+ # num ou ordem
  scale_y_continuous(breaks = seq(from = 0, to = 1.0, by = 0.1),
                     labels = scales::label_percent(accuracy = 1))+
  scale_colour_manual(values=paleta,
                      labels=c("Discordo_Totalmente", "Discordo", "Neutro", "Concordo", "Concordo_Totalmente"))+
  labs(x = "\nOrdem dos Quantificadores", y = "Probabilidades preditas\n", fill="Respostas")+ 
  ggtitle("Painel 2: Previsão de probabilidades estimada pelo modelo",
          subtitle = "Linhas verticais indicam intervalos de credibilidade preditos.")+ 
  theme_classic()+
  guides(colour = guide_legend(reverse=T)) # Apenas organizando a ordem da legenda.


# Observação
# Esse comando plota os mesmos valores acima, mas num formato diferente e, a meu ver, não tão prático
plot_model(m2, type = "pred", terms = c("Num", "Ordem"))

# Pode-se plotar, ainda, a distribuição de cada um dos coeficientes estimados
plot(m2)

################################################################################
# AJUSTANDO UM MODELO COM PRIORS ESPECÍFICAS
################################################################################
# Referência aqui sobre get_prior
# https://paul-buerkner.github.io/brms/reference/get_prior.html
# Apenas indica as priors possíveis para cada parámetro do modelo a ser aplicado
get_prior(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1+Ordem*Num|item), data = dados,
          family = cumulative(link = "logit", threshold = "flexible"))

# Referência aqui sobre set_prior
# Define priors manualmente
# https://paul-buerkner.github.io/brms/reference/set_prior.html

# Ver Burkner & Vuorre (2019), p. 90 (só chupei o exemplo de lá sem nem pensar a respeito)
prior_manual <-
  prior(normal(0, 0.1), class = "b") + # Assume distribuição normal com média 0 e desvio padrão 5 para todos os coeficientes estimados (b de beta)
  prior(normal(0, 1.5), class = "Intercept") # Assume distribuição normal com média 0 e desvio padrão 5 para os interceptos (nesse caso tau cuts)

m3 <- brm(answer ~ Ordem*Num + (1+Ordem*Num|subj)+(1+Ordem*Num|item), data = dados,
          family = cumulative(link = "logit", threshold = "flexible"), # Probit não está funcionando... Não sei o motivo...
          prior = prior_manual,
          sample_prior = "only") 

summary(m3)

pp_check(m3, ndraws = 100)

################################################################################
# Montando o gráfico dos efeitos marginais
################################################################################
model3_data<-get_model_data(m3,
                           type = "pred",
                           terms = c("Ordem", "Num"),
                           ci.lvl = .95)

model3_data<-data.frame(ordem = model3_data$x,
                       num = model3_data$group,
                       Respostas = model3_data$response.level,
                       Probabilidades = model3_data$predicted,
                       lower = model3_data$conf.low,
                       upper = model3_data$conf.high)

model3_data$ordem<-c(rep("um-todo", 10), rep("todo-um", 10))

model3_data$ordem<-as.factor(model3_data$ordem)

# Preparar uma paleta de cores condizente com a paleta usada no gráfico de barras empilhadas
paleta<-c("#D7191C", "#FDAE61", "#9C9C9C", "#ABDDA4", "#2B83BA")

model3_data %>%
  ggplot(aes(x=ordem, y=Probabilidades, group=Respostas, color=Respostas))+ # x = ordem ou num
  geom_line(position = position_dodge(0.3), alpha = 0.3, linetype = "dotted") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=0.2, position = position_dodge(0.3), alpha=0.3)+
  geom_point(position = position_dodge(0.3), size = 3, shape = 21, fill = "white", stroke = 1) +
  facet_wrap(~num)+ # num ou ordem
  scale_y_continuous(breaks = seq(from = 0, to = 1.0, by = 0.1),
                     labels = scales::label_percent(accuracy = 1))+
  scale_colour_manual(values=paleta,
                      labels=c("Discordo_Totalmente", "Discordo", "Neutro", "Concordo", "Concordo_Totalmente"))+
  labs(x = "Número da anáfora", y = "Probabilidades preditas\n", fill="Respostas")+ 
  ggtitle("Painel 2: Previsão de probabilidades estimada pelo modelo",
          subtitle = "Linhas verticais indicam intervalos de credibilidade preditos.")+ 
  theme_classic()+
  guides(colour = guide_legend(reverse=T)) # Apenas organizando a ordem da legenda.

# Observe que, como as minhas priors são completamente sem sentido, as estimativas do modelo também são loucas!!!
