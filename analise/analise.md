# Análise

Neste arquivo, encontra-se o código para replicação das análises apresentadas no relatório [**Estatísticas e dados do segmento de podcasts no Brasil em 2019**](https://www.voltdata.info/conteudo/2019/estatsticas-de-podcasts). 

Na apresentação do código, seguiremos a ordem em que o conteúdo foi apresentado no relatório.

Primeiramente, carregamos os pacotes e funções que serão usados.


```r
## Pacotes
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(knitr)

### Funcoes

# Tema para a criacao dos graficos
source('https://raw.githubusercontent.com/voltdatalab/pesquisa-podcasts/master/analise/tema_grafico.R')
```

No *chunk* abaixo também listamos as respectivas versões de cada pacote aberto na sessão.


```r
## Lista dos pacotes abertos na sessao
sessionInfo()
```

```
## R version 3.6.1 (2019-07-05)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Mojave 10.14.6
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] knitr_1.23      scales_1.0.0    ggthemes_4.2.0  lubridate_1.7.4
##  [5] forcats_0.4.0   stringr_1.4.0   dplyr_0.8.1     purrr_0.3.2    
##  [9] readr_1.3.1     tidyr_0.8.3     tibble_2.1.3    ggplot2_3.1.1  
## [13] tidyverse_1.2.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.4.1     compiler_3.6.1  
##  [5] plyr_1.8.4       tools_3.6.1      digest_0.6.19    jsonlite_1.6    
##  [9] evaluate_0.14    nlme_3.1-140     gtable_0.3.0     lattice_0.20-38 
## [13] pkgconfig_2.0.2  rlang_0.4.0      cli_1.1.0        rstudioapi_0.10 
## [17] yaml_2.2.0       haven_2.1.0      xfun_0.7         withr_2.1.2     
## [21] xml2_1.2.0       httr_1.4.0       hms_0.4.2        generics_0.0.2  
## [25] grid_3.6.1       tidyselect_0.2.5 glue_1.3.1       R6_2.4.0        
## [29] readxl_1.3.1     rmarkdown_1.13   modelr_0.1.4     magrittr_1.5    
## [33] backports_1.1.4  htmltools_0.3.6  rvest_0.3.4      assertthat_0.2.1
## [37] colorspace_1.4-1 stringi_1.4.3    lazyeval_0.2.2   munsell_0.5.0   
## [41] broom_0.5.2      crayon_1.3.4
```

Em seguida, abrimos os bancos de dados a serem utilizados ([eles estão salvos na pasta "analise" deste repositório](https://github.com/voltdatalab/pesquisa-podcasts/tree/master/analise)), e fazemos uma rápida inclusão de variável e transformação de outra nessas bases. 


```r
# Abre os bancos de podcasts brasileiros e internacionais, obtidos com a raspagem
bra <- read.csv("https://raw.githubusercontent.com/voltdatalab/pesquisa-podcasts/master/analise/nacionais.csv", header = T)
int <- read.csv("https://raw.githubusercontent.com/voltdatalab/pesquisa-podcasts/master/analise/internacionais.csv", header = T)

# Cria uma variavel para indicar o local de producao
bra$local <- "Brasil"
int$local <- "EUA"

# Cria um dataset juntando os episodios brasileiros e internacionais
agregado <- bind_rows(bra, int)

# Transforma a variavel episode_date em todos os bancos para a classe "Date"
int$episode_date <- as.Date(int$episode_date)
bra$episode_date <- as.Date(bra$episode_date)
agregado$episode_date <- as.Date(agregado$episode_date)
```

A seguir, seguem os códigos para replicação dos gráficos e tabelas gerados para o relatório.

### Produção de podcasts - série mensal


```r
# Organiza dados de numero de episodios por mes (Brasil)
episodios_mes <- bra %>%
  na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  filter(episode_date < "2019-08-01") %>%
  select(episode_date) %>%
  group_by(mes=floor_date(episode_date, "month")) %>%
  count(name = "n_episodios") 

episodios_mes$local <- "Brasil"

# Organiza dados de numero de episodios por mes (EUA)
episodios_mes_int <- int %>%
  na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  filter(episode_date < "2019-08-01") %>%
  select(episode_date) %>%
  group_by(mes=floor_date(episode_date, "month")) %>%
  count(name = "n_episodios")

episodios_mes_int$local <- "EUA"

# Junta os bancos de episodios por mes do Brasil e EUA
producao_total_por_mes <- bind_rows(episodios_mes, episodios_mes_int)

# Cria um grafico a partir dos bancos unidos
grafico_producao_mes <- ggplot(producao_total_por_mes) + 
  geom_line(aes(mes,n_episodios, colour = local)) +  
  scale_x_date(breaks = pretty_breaks(10), limits = as.Date(c("2004-01-01","2019-08-01"))) +
  scale_colour_manual(values=c("#386cb0","#B80062")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(subtitle = "Total de episódios com mais de 3 min de duração produzidos\npor mês pelos 100 principais podcasts no Brasil e nos EUA", 
       y = "episódios por mês", 
       x = "ano", 
       title = "Produção de podcasts - série mensal", 
       caption = "Fonte: Levantamento Volt Data Lab. Data-base agosto/2019") 

# Inclui o tema ao grafico produzido acima  
grafico_producao_mes + tema()
```

### Produção de podcasts - série anual


```r
# Organiza dados de numero de episodios por ano (Brasil)
episodios_ano <- bra %>%
  na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  filter(episode_date < "2019-01-01") %>%
  select(episode_date) %>%
  group_by(mes=floor_date(episode_date, "year")) %>%
  count(name = "n_episodios")

# Organiza dados de numero de episodios por ano (EUA)
episodios_ano_int <- int %>%
  na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  filter(episode_date < "2019-01-01") %>%
  select(episode_date) %>%
  group_by(mes=floor_date(episode_date, "year")) %>%
  count(name = "n_episodios")

episodios_ano$local <- "Brasil"
episodios_ano_int$local <- "EUA"

# Junta os bancos de episodios por ano do Brasil e EUA
producao_total <- bind_rows(episodios_ano, episodios_ano_int)

# Cria um grafico a partir dos bancos unidos
grafico_prod_anual <- ggplot(producao_total) + 
  geom_line(aes(mes,n_episodios, colour = local)) + 
  scale_x_date(breaks = pretty_breaks(10)) +
  scale_colour_manual(values = c("#386cb0","#B80062")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(subtitle = "Total de episódios com mais de 3 min de duração produzidos\npor ano pelos 100 principais podcasts no Brasil e nos EUA", 
       y = "episódios por ano", 
       x = "", 
       title = "Produção de podcasts - série anual", 
       caption = "Fonte: Levantamento Volt Data Lab. Data-base agosto/2019") 

# Inclui o tema ao grafico produzido acima
grafico_prod_anual + tema()
```

### Brasil - 2010 a 2018


```r
# Conta o n. de ep. de podcasts brasileiros por ano 
novos_pods <- bra %>%
  filter(length_minutes >= params$duracao) %>%
  mutate(ano = format(episode_date, "%Y")) %>%
  filter(!is.na(ano)) %>% 
  select(ano, episode_title) %>%
  group_by(ano) %>%
  filter(ano >= 2010 && ano < 2019) %>%
  summarise(Unique_Elements = n_distinct(episode_title))

# Gera uma tabela com as estatisticas descriticas
kable(data.frame(round(unclass(summary(novos_pods$Unique_Elements)))), caption = "Brasil  - 2010 a 2018")
```

### EUA - 2010 a 2018


```r
# Conta o n. de ep. de podcasts norte-americanos por ano 
novos_pods_int <- int %>%
  filter(length_minutes >= params$duracao) %>%
  mutate(ano = format(episode_date, "%Y")) %>%
  filter(!is.na(ano)) %>% 
  select(ano, episode_title) %>%
  group_by(ano) %>%
  filter(ano >= 2010 && ano < 2019) %>%
  summarise(Unique_Elements = n_distinct(episode_title))

# Gera uma tabela com as estatisticas descriticas
kable(data.frame(round(unclass(summary(novos_pods_int$Unique_Elements)))), caption = "EUA - 2010 a 2018")
```

### Estatísticas de duração de podcasts em minutos - Top 100 podcasts Brasil


```r
# Cria tabela com as estatisticas descriticas de duracao de podcasts brasileiros
duracao_media_br <- bra %>%
  filter(length_minutes >= params$duracao) %>%
  #summarise(mean(length_minutes)) %>%
  summarise(round(Mean=mean(length_minutes)), Max=max(length_minutes), Min=min(length_minutes), Median=median(length_minutes), round(Std=sd(length_minutes))) %>%
  rename("média" = "round(Mean = mean(length_minutes))", "máximo" = "Max","mínimo" = "Min", "mediana" = "Median", "desvio padrão" = "round(Std = sd(length_minutes))")

# Gera e formata a tabela, incluindo título  
kable(duracao_media_br, format = 'pandoc', caption = "Estatísticas de duração, em minutos - Top 100 podcasts - Brasil")
```

### Estatísticas de duração de podcasts em minutos - Top 100 podcasts Brasil


```r
# Cria tabela com as estatisticas descriticas de duracao de podcasts internacionais
duracao_media_int <- int %>%
  filter(length_minutes >= params$duracao) %>%
  #summarise(mean(length_minutes)) %>%
  summarise(round(Mean=mean(length_minutes)), Max=max(length_minutes), Min=min(length_minutes), Median=median(length_minutes), round(Std=sd(length_minutes))) %>%
  rename("média" = "round(Mean = mean(length_minutes))", "máximo" = "Max","mínimo" = "Min", "mediana" = "Median", "desvio padrão" = "round(Std = sd(length_minutes))")

# Gera e formata a tabela, incluindo título
kable(duracao_media_int, format = 'pandoc', caption = "Estatísticas de duração, em minutos - Top 100 podcasts - EUA")
```

### Gráfico - Duração de podcasts por país


```r
# Filtra o banco para nao incluir episodios com menos de 3min ou mais de 3h
duracao_geral <- agregado %>%
  filter(length_minutes > params$min_analise) %>%
  filter(length_minutes < params$max_analise) %>%
  select(length_minutes, local) %>%
  filter(length_minutes >= params$duracao)

# Gera um grafico a partir dos dados de duracao
grafico_duracao_geral <- ggplot(duracao_geral, aes(local, length_minutes, colour = local, show.legend = FALSE)) + 
  #geom_boxplot(width = 0.3, alpha = 0.4, outlier.shape = NA) + 
  geom_jitter(shape=11, alpha = 0.05, size = 0.5, width = 0.1) +
  scale_colour_manual(values=c("#386cb0","#B80062")) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5, colour = "#cbcbcb") +
  annotate("text", label = "Mediana = 46 min", x = 2.3, y = 60, color = "#222222", family = "Inconsolata", fontface = "bold", size = 3) +
  annotate("text", label = "Mediana = 66 min", x = 1.3, y = 86, color = "#222222", family = "Inconsolata", fontface="bold", size = 3) +
  labs(subtitle = "Duração de cada episódio no Brasil e nos EUA entre 3\ne 180 minutos, pelos 100 principais podcasts no Brasil e nos EUA", 
       y = "duração em minutos", 
       x = "", 
       title = "Duração de podcasts - por país", 
       caption = "Fonte: Levantamento Volt Data Lab. Data-base agosto/2019")
  
# Inclui o tema ao grafico produzido acima
grafico_duracao_geral + tema() + theme(legend.position = "none")
```

### Gráfico - Duração de podcasts


```r
# Organiza banco de dados com duracao de podcasts brasileiros
espectro_duracao <- bra %>%
  filter(length_minutes >= params$min_analise) %>%
  select(length_minutes) %>%
  group_by(length_minutes) %>%
  rename(duracao_minutos = length_minutes) %>%
  count(name = "total_episodios") %>%
  filter(duracao_minutos < params$max_analise) %>%
  arrange(duracao_minutos)

espectro_duracao$local <- "Brasil"

# Organiza banco de dados com duracao de podcasts norte-americanos
espectro_duracao_int <- int %>%
  filter(length_minutes >= params$min_analise) %>%
  select(length_minutes) %>%
  group_by(length_minutes) %>%
  rename(duracao_minutos = length_minutes) %>%
  count(name = "total_episodios") %>%
  filter(duracao_minutos < params$max_analise) %>%
  arrange(duracao_minutos)

espectro_duracao_int$local <- "EUA"

# Junta os bancos de dados com duracao de podcasts brasileiros e americanos
comparativo_duracao <- bind_rows(espectro_duracao_int,espectro_duracao)
#print(comparativo_duracao)

# Gera um grafico incluindo as informacoes do Brasil e dos EUA em linhas separadas
grafico_espectro <- ggplot(comparativo_duracao, aes(duracao_minutos, total_episodios, group=local, colour = local, fill = local)) + 
  geom_area(position = "identity", alpha = 0.7, show.legend = NA, size = 0.5) +
  scale_colour_manual(values=c("#386cb0","#B80062")) +
  annotate("text", label = "Boa parte dos podcasts\n--> brasileiros tem longa duração", x = 150, y = 120, color = "#222222", family = "Inconsolata", fontface = "bold", size = 3) +
  scale_fill_manual(values = c("#386cb0","#B80062")) +
  labs(subtitle = "Minutagem dos 100 principais podcasts no Brasil e nos EUA. Para\nmelhor ncomparação, considera podcasts entre 10min e 3h", 
       y = "número de episódios", 
       x = "tempo de duração, em minutos", 
       title = "Duração de podcasts", 
       caption = "Fonte: Levantamento Volt Data Lab. Data-base agosto/2019")

# Inclui o tema ao grafico produzido acima
grafico_espectro + tema() 
```

### Gráfico - Longevidade de podcasts 


```r
# Organiza dados de shows unicos ativos por ano
novos_pods <- agregado %>%
  mutate(ano = format(episode_date, "%Y")) %>%
  filter(!is.na(ano)) %>% 
  select(ano, title, local) %>%
  group_by(local, ano) %>%
  filter(ano >= 2010 && ano < 2019) %>%
  summarise(Unique_Elements = n_distinct(title))

# Gera grafico de shows unicos ativos por ano
grafico_novos_pods <- ggplot(novos_pods, aes(ano, Unique_Elements, colour = local, fill = local), family = "Inconsolata") +
  scale_colour_manual(values = c("#386cb0","#B80062")) + 
  scale_fill_manual(values = c("#386cb0","#B80062")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(subtitle = "Shows únicos ativos em ago/2019 que publicaram episódios em anos 
anteriores. Esse dado indica a criação de novos shows.
Considera os 100 principais shows em agosto/2019.", 
       y = "número de shows", 
       x = "", 
       title = "Longevidade de podcasts", 
       caption = "Fonte: Levantamento Volt Data Lab. Data-base agosto/2019")

# Inclui o tema ao grafico produzido acima
grafico_novos_pods + tema() 
```

### Periodicidade


```r
# Organiza dados de periodicidade de podcasts brasileiros e norte-americanos,
# criando uma tabela de frequencia absoluta para cada um

# Brasil
podcasts_periodicidade <- bra %>%
  #na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  distinct(title, frequency) %>%
  group_by(frequency) %>%
  count(name = "n_episodios")

# EUA
podcasts_periodicidade_int <- int %>%
  #na.omit() %>%
  filter(length_minutes >= params$duracao) %>%
  distinct(title, frequency) %>%
  group_by(frequency) %>%
  count(name = "n_episodios")

podcasts_periodicidade$local <- "BRA"
podcasts_periodicidade_int$local <- "EUA"

periodicidade <- bind_rows(podcasts_periodicidade, podcasts_periodicidade_int)
periodicidade$frequency <- gsub("Weekly podcast", "semanal", periodicidade$frequency)
periodicidade$frequency <- gsub("Daily podcast", "diário", periodicidade$frequency)
periodicidade$frequency <- gsub("Monthly podcast", "mensal", periodicidade$frequency)
periodicidade$frequency <- gsub("Fortnightly podcast", "quinzenal", periodicidade$frequency)
periodicidade$frequency <- gsub("Indeterminate", "indeterminado", periodicidade$frequency)
periodicidade$frequency <- gsub("Show is on a break or finished.", "em hiato ou encerrado", periodicidade$frequency)
periodicidade <-  spread(periodicidade, local, n_episodios, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

# Gera e formata a tabela, incluindo título
kable(periodicidade, format = "pandoc", caption = "Periodicidade dos 100 principais podcasts no Brasil e nos EUA", full_width = F, position = "left", bootstrap_options = "striped")
```

### Evolução de episódios por programa - Top 10 Brasil


```r
# Cria banco de dados com numero de ep por podcast brasileiro
episodios_por_show <- bra %>%
  filter(length_minutes >= params$duracao) %>%
  select(title, author) %>%
  count(title, name = "episodios", sort = T)%>%
  rename("nome do show" = "title")

# Gera e formata a tabela, incluindo título
kable(head(episodios_por_show, n = 10L), format = 'pandoc', caption = "Evolução de episódios por programa no Brasil", full_width = F, position = "left", bootstrap_options = "striped")
```

### Evolução de episódios por programa - Top 10 EUA


```r
# Cria banco de dados com numero de ep por podcast internacional
episodios_por_show_int <- int %>%
  filter(length_minutes >= params$duracao) %>%
  select(title, author) %>%
  count(title, name = "episodios", sort = T) %>%
  rename("nome do show" = "title")

# Gera e formata a tabela, incluindo título
kable(head(episodios_por_show_int, n = 10L), format = 'pandoc', caption = "Evolução de episódios por programa nos EUA", full_width = F, position = "left", bootstrap_options = "striped")
```

### Evolução de episódios por produtor de conteúdo ou grupo econômico - Top 10 Brasil


```r
# Cria banco de dados com numero de ep por produtor de conteudo (Brasil)
episodios_por_produtor <- bra %>%
  filter(length_minutes >= params$duracao) %>%
  select(author) %>%
  count(author, name = "episodios", sort = T) %>%
  rename("autor/produtora" = "author")

# Gera e formata a tabela, incluindo título
kable(head(episodios_por_produtor, n=10L), format = 'pandoc', caption = "Episódios por produtor no Brasil", full_width = F, position = "left", bootstrap_options = "striped")
```

### Evolução de episódios por produtor de conteúdo (grupo econômico) - Top 10 EUA


```r
# Cria banco de dados com numero de ep por produtor de conteudo (EUA)
episodios_por_produtor_int <- int %>%
  filter(length_minutes >= params$duracao) %>%
  select(author) %>%
  count(author, name = "episodios", sort = T)%>%
  rename("autor/produtora" = "author")

# Gera e formata a tabela, incluindo título
kable(head(episodios_por_produtor_int, n=10L), format = 'pandoc', caption = "Episódios por produtor nos EUA", full_width = F, position = "left", bootstrap_options = "striped")
```


