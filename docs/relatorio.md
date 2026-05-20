---
title: "Relatório para o curso Ciência de Dados 2"
author: "Patricia Camacho"
date: "2026-04-30"
output: 
   html_document:
    keep_md: true
---



Os dados dos óbitos são do Sistema de Informações sobre Mortalidade (SIM), disponibilizados através do Departamento de Informática do Ministério da Saúde [DATASUS](http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def)

Os dados dos óbitos, extraídos e tratados, estão na subpasta 'dados', no arquivo AYA.csv AYA - adolescent and young adult


``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   4.0.0     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
AYA <- read_csv('~/trabalho_final_CD2/dados/AYA.csv')
```

```
## New names:
## Rows: 197088 Columns: 8
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (4): rcbp, sexo, idade_gp, CID_3 dbl (4): ...1, ano, pop, numcasos
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

#Principais neoplasias

Calculando o total de óbitos por câncer de início precoce, segundo o tipo de neoplasia, no conjunto de treze RCBP, entre os anos de 2001 a 2017.


``` r
daux01 <- AYA[ , c('CID_3', 'numcasos')] %>%
  group_by(CID_3) %>%
  summarise(tot_obitos = sum(numcasos, na.rm=T), .groups = "drop")
daux01 <- na.omit(daux01)
```

Gerando o gráfico de barras para as causas dos óbitos


``` r
g1 <- ggplot(daux01, aes(x = reorder(CID_3, tot_obitos), y = tot_obitos)) +
  geom_col(fill = '#c34a36', alpha = .6) +
  labs(
    title = "Óbitos pelas principais neplasias em adolescentes e adultos jovens (15 a 39 anos), de 2001 a 2017, em treze RCBP* brasileiros.",
    x = "neplasias",
    y = "total de óbitos (n)",
    caption = 'Fonte: SIM, Ministério da Saúde, Brasil.
    *RCBP - Registro de Câncer de Base Populacional
    LH - linfoma de Hodgkin; LNH - linfoma não Hodgkin; SNC - sistema nervoso central; TBL - traqueia, bronquios e pulmão'
  ) +
  geom_text(
    aes(label = CID_3),
    hjust = -0.02,
    size = 2.5
  ) +
  scale_y_continuous(
    breaks = seq(0, 2500, 100),
    labels = scales::label_comma(big.mark = " ", decimal.mark = "."),
    expand = expansion(mult = c(0, 0.32))) +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), 
        text = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linetype = 'dotted', colour = 'grey70')
  ) +
  coord_flip()
g1
```

![](relatorio_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#Variação temporal das taxas por sexo

Calculando as taxas de mortalidade por sexo ao longo do período do estudo


``` r
daux00 = unique(AYA[,c("sexo","ano","pop")])%>% 
  group_by(sexo, ano) %>%
  summarise(pop_agreg = sum(pop, na.rm=T), .groups = "drop")

daux01 <- AYA[ , c('sexo','ano','numcasos')] %>%
  group_by(sexo, ano) %>%
  summarise(tot_obitos = sum(numcasos, na.rm=T), .groups = "drop")

daux03 = left_join(daux00, daux01, by = c("sexo","ano"))

daux03$tx_esp = (daux03$tot_obitos/daux03$pop_agreg)*100000

daux03 <- daux03 [ , c("ano", "sexo", "tx_esp")] %>% 
  pivot_wider(
  names_from = sexo,
  values_from = tx_esp
  )
```

Construindo um gráfico interativo para as variações das taxas de mortalidade por sexo ao longo do período do estudo


``` r
library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(daux03, main = 'Taxas de mortalidade por câncer 
        de início precoce, por 100 mil adolescentes e adultos
        jovens (15 a 39 anos), segundo o sexo, em treze RCBP 
        brasileiros, de 2001 a 2017' , xlab = 'ano', ylab = 'taxa') %>%
  dySeries("Masculino", label = "Male", color = '#009efa') %>%
  dySeries("Feminino", label = "Female", color = '#ff6f91') %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 25)
```

```{=html}
<div class="dygraphs html-widget html-fill-item" id="htmlwidget-0636eac7edb76fc80253" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0636eac7edb76fc80253">{"x":{"attrs":{"title":"Taxas de mortalidade por câncer \n        de início precoce, por 100 mil adolescentes e adultos\n        jovens (15 a 39 anos), segundo o sexo, em treze RCBP \n        brasileiros, de 2001 a 2017","xlabel":"ano","ylabel":"taxa","labels":["ano","Male","Female"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true},"y":{"drawAxis":true}},"colors":["#009efa","#ff6f91"],"series":{"Male":{"axis":"y"},"Female":{"axis":"y"}},"stackedGraph":false,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false,"showRangeSelector":true,"rangeSelectorHeight":25,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017],[12.46291250645935,12.08670392913805,13.07163352716199,11.90056366164096,11.94403211092651,10.96129924825416,12.09852730129939,11.47218756987564,12.1914381448738,10.83924402163219,11.37522200534777,13.16840318756653,11.06865800267289,11.17513434929929,12.61232010667086,12.56763277853771,11.52489456450673],[15.46599153042776,14.16816534553756,13.10754590484395,14.39812961116904,14.73437652357967,14.43752362917224,12.37614869631873,14.59240138535705,13.80154398391892,14.65314567839015,13.26625848936299,15.36091981706409,15.08975422968184,15.6231730630739,14.67547057888114,15.92023626083116,14.62338656873725]],"fixedtz":false,"tzone":""},"evals":["attrs.interactionModel"],"jsHooks":[]}</script>
```
