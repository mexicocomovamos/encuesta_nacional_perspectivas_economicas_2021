Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("foreign")) install.packages("foreign") & require(foreign)
if(!require("mxmaps")) devtools::install_github('diegovalle/mxmaps') & require(mxmaps)
if(!require("haven")) install.packages("haven") & require(haven)
if(!require("srvyr")) install.packages("srvyr") & require(srvyr)
if(!require("ggpubr")) install.packages("ggpubr") & require(ggpubr)
if(!require("gtable")) install.packages("gtable") & require(gtable)
if(!require("grid")) install.packages("grid") & require(grid)
if(!require("gridExtra")) install.packages("gridExtra") & require(gridExtra)
require(tidyverse)


# Desactiva notación científica
options(scipen=999)

# Colores MCV -----
mcv_discrete <- c(
  "#7D0A4C", "#6E1640", "#9E5E91", "#AA6D9B",
  "#462759", "#30367C", "#AD1E34", "#E7E4E4"
)

# Directorios ----
inp <- "mcv/reforma_encuesta_econ/01_datos/"
out <- "mcv/reforma_encuesta_econ/03_gráficas/"

# Mapa para gráficas ----
load("00_mxmap/00_mxmap.RData")
mxmap <- mxstate.map %>% 
  left_join(
    df_mxstate %>% 
      mutate(
        pib_per = case_when(
          state_abbr == "BC" ~ "1. Ingresos altos",
          state_abbr == "BCS" ~ "1. Ingresos altos",
          state_abbr == "SON" ~ "1. Ingresos altos",
          state_abbr == "AGS" ~ "1. Ingresos altos",
          state_abbr == "COAH" ~ "1. Ingresos altos",
          state_abbr == "NL" ~ "1. Ingresos altos",
          state_abbr == "CAMP" ~ "1. Ingresos altos",
          state_abbr == "TAB" ~ "1. Ingresos altos",
          state_abbr == "CDMX" ~ "1. Ingresos altos",
          state_abbr == "QROO" ~ "1. Ingresos altos",
          state_abbr == "QRO" ~ "1. Ingresos altos",
          state_abbr == "SLP" ~ "2. Ingresos medios",
          state_abbr == "GTO" ~ "2. Ingresos medios",
          state_abbr == "CHIH" ~ "2. Ingresos medios",
          state_abbr == "DGO" ~ "2. Ingresos medios",
          state_abbr == "JAL" ~ "2. Ingresos medios",
          state_abbr == "SIN" ~ "2. Ingresos medios",
          state_abbr == "TAM" ~ "2. Ingresos medios",
          state_abbr == "ZAC" ~ "2. Ingresos medios",
          state_abbr == "YUC" ~ "2. Ingresos medios",
          state_abbr == "MOR" ~ "2. Ingresos medios",
          state_abbr == "COL" ~ "2. Ingresos medios",
          state_abbr == "HGO" ~ "2. Ingresos medios",
          state_abbr == "NAY" ~ "3. Ingresos bajos",
          state_abbr == "CHPS" ~ "3. Ingresos bajos",
          state_abbr == "OAX" ~ "3. Ingresos bajos",
          state_abbr == "PUE" ~ "3. Ingresos bajos",
          state_abbr == "TLAX" ~ "3. Ingresos bajos",
          state_abbr == "GRO" ~ "3. Ingresos bajos",
          state_abbr == "VER" ~ "3. Ingresos bajos",
          state_abbr == "MEX" ~ "3. Ingresos bajos",
          state_abbr == "MICH" ~ "3. Ingresos bajos",
          T ~ NA_character_
        )
      ) %>% 
      select(
        region, state_abbr, pib_per
      )
  )

mcv_pib <- c(mcv_discrete[6],mcv_discrete[3],mcv_discrete[1])

leyenda <- 
  ggplot(mxmap, aes(long, lat, group=group, fill = as.factor(pib_per))) +
  geom_polygon(color = "black", size = .2) +
  scale_fill_manual("",values = c(mcv_pib[1],mcv_pib[2],mcv_pib[3])) +
  theme_void() +
  guides(fill=F) +
  coord_map() 

# Datos ----
d <- read.spss(paste0(inp,"N20210213_1200n.sav"),to.data.frame = T) %>% 
  as.tibble() %>% 
  janitor::clean_names() 

codebook <- tibble(etiqueta = colnames(d)) %>%
  mutate(pregunta = attr(d, "variable.labels")) %>% 
  filter(
    etiqueta!="b",
    etiqueta!="c",
    etiqueta!="estrato",
    etiqueta!="gedad",
    etiqueta!="escol_x",
    etiqueta!="p2",
    etiqueta!="p7",
    etiqueta!="p8",
    etiqueta!="p28",
    etiqueta!="fin",
    etiqueta!="dia",
    etiqueta!="tiempo",
    etiqueta!="nombre",
    etiqueta!="telefono"
  )


# Recodificación ----
v_labels <- codebook$etiqueta[13:93]
v_names <- codebook$pregunta[13:93]
v_names <- tolower(v_names)
v_names <- trimws(v_names)
v_names <- str_replace_all(v_names, "[[:punct:]]", "")
v_names <- str_replace_all(v_names, "  ", " ")
v_names <- str_replace_all(v_names, " ", "_")
v_names <- paste0("v_",v_names)
v_names[80] <- "v_8_qué_es_lo_primero_que_le_viene_a_la_mente_cuando_le_menciono_la_palabra_bienestar"

recode_names <- function(x){
  x = 
    case_when(
      x == v_labels[1] ~ v_names[1],
      x == v_labels[2] ~ v_names[2],
      x == v_labels[3] ~ v_names[3],
      x == v_labels[4] ~ v_names[4],
      x == v_labels[5] ~ v_names[5],
      x == v_labels[6] ~ v_names[6],
      x == v_labels[7] ~ v_names[7],
      x == v_labels[8] ~ v_names[8],
      x == v_labels[9] ~ v_names[9],
      x == v_labels[10] ~ v_names[10],
      x == v_labels[11] ~ v_names[11],
      x == v_labels[12] ~ v_names[12],
      x == v_labels[13] ~ v_names[13],
      x == v_labels[14] ~ v_names[14],
      x == v_labels[15] ~ v_names[15],
      x == v_labels[16] ~ v_names[16],
      x == v_labels[17] ~ v_names[17],
      x == v_labels[18] ~ v_names[18],
      x == v_labels[19] ~ v_names[19],
      x == v_labels[20] ~ v_names[20],
      x == v_labels[21] ~ v_names[21],
      x == v_labels[22] ~ v_names[22],
      x == v_labels[23] ~ v_names[23],
      x == v_labels[24] ~ v_names[24],
      x == v_labels[25] ~ v_names[25],
      x == v_labels[26] ~ v_names[26],
      x == v_labels[27] ~ v_names[27],
      x == v_labels[28] ~ v_names[28],
      x == v_labels[29] ~ v_names[29],
      x == v_labels[30] ~ v_names[30],
      x == v_labels[31] ~ v_names[31],
      x == v_labels[32] ~ v_names[32],
      x == v_labels[33] ~ v_names[33],
      x == v_labels[34] ~ v_names[34],
      x == v_labels[35] ~ v_names[35],
      x == v_labels[36] ~ v_names[36],
      x == v_labels[37] ~ v_names[37],
      x == v_labels[38] ~ v_names[38],
      x == v_labels[39] ~ v_names[39],
      x == v_labels[40] ~ v_names[40],
      x == v_labels[41] ~ v_names[41],
      x == v_labels[42] ~ v_names[42],
      x == v_labels[43] ~ v_names[43],
      x == v_labels[44] ~ v_names[44],
      x == v_labels[45] ~ v_names[45],
      x == v_labels[46] ~ v_names[46],
      x == v_labels[47] ~ v_names[47],
      x == v_labels[48] ~ v_names[48],
      x == v_labels[49] ~ v_names[49],
      x == v_labels[50] ~ v_names[50],
      x == v_labels[51] ~ v_names[51],
      x == v_labels[52] ~ v_names[52],
      x == v_labels[53] ~ v_names[53],
      x == v_labels[54] ~ v_names[54],
      x == v_labels[55] ~ v_names[55],
      x == v_labels[56] ~ v_names[56],
      x == v_labels[57] ~ v_names[57],
      x == v_labels[58] ~ v_names[58],
      x == v_labels[59] ~ v_names[59],
      x == v_labels[60] ~ v_names[60],
      x == v_labels[61] ~ v_names[61],
      x == v_labels[62] ~ v_names[62],
      x == v_labels[63] ~ v_names[63],
      x == v_labels[64] ~ v_names[64],
      x == v_labels[65] ~ v_names[65],
      x == v_labels[66] ~ v_names[66],
      x == v_labels[67] ~ v_names[67],
      x == v_labels[68] ~ v_names[68],
      x == v_labels[69] ~ v_names[69],
      x == v_labels[70] ~ v_names[70],
      x == v_labels[71] ~ v_names[71],
      x == v_labels[72] ~ v_names[72],
      x == v_labels[73] ~ v_names[73],
      x == v_labels[74] ~ v_names[74],
      x == v_labels[75] ~ v_names[75],
      x == v_labels[76] ~ v_names[76],
      x == v_labels[77] ~ v_names[77],
      x == v_labels[78] ~ v_names[78],
      x == v_labels[79] ~ v_names[79],
      x == v_labels[80] ~ v_names[80],
      x == v_labels[81] ~ v_names[81],
      T ~ x
    )
}

data <- d %>% 
  rename(
    n_punto = punto
  )  %>% 
  rename_at(
    vars(starts_with("p")),
    funs(recode_names)
  ) %>% 
  rename(
    v_si_hoy_hubiera_elecciones_para_diputado_federal_por_quién_votaría_usted = boleta,
    weight = factor_expa,
    cruce_sexo = b,
    cruce_grupos_edad = gedad,
    cruce_edu = escol_x,
    cruce_pib_capita = estrato
  ) %>% 
  select(
    cve_unica_sec:a, starts_with("cruce"), starts_with("v_"), weight
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("v_")), 
    .funs = funs(factor)
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("cruce_")),
    .funs = funs(factor)
  ) 
data$weight <- as.numeric(data$weight)
data[data == "Ns/Nc"] <- NA

# Proporciones simples ----
# * Tabla de frecuencias ----
prop_simples <- data.frame()
for (i in 1:length(v_names)){
  tempo <- data %>% 
    select(v_names[i], weight) %>% 
    rename(var_v = starts_with("v_")) %>% 
    drop_na(var_v) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    group_by(var_v) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  prop_simples <- bind_rows(prop_simples, tempo)
  rm(tempo)
}



# * Gráficas ----
v_names_loop <- str_replace_all(v_names, "v_", "")
v_num <- sub("\\_.*", "", v_names_loop)
v_num[1:13] <- paste0("0",v_num[1:13])
v_num[78:80] <- paste0("0",v_num[78:80])
v_num[77] <- "40"

vector_fiuf <- as.character(codebook$pregunta[13:93])
vector_fiuf <- sub('.*\\. ', '', vector_fiuf)

fiuffi <- "Fuente: Encuesta @MexicoComoVamos-Reforma | febrero 2021"

a <- prop_simples

for(i in 1:length(v_names_loop)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop[i]), 
         aes(x = reorder(str_wrap(var_v,10), -prop), 
             y = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             fill = prop)) + 
    geom_bar(stat = "identity", width = .3) +
    geom_text(size = 7, vjust = -0.5) +
    scale_y_continuous(
      limits = c(0,100),
      breaks = seq(0,100,25),
      labels = paste0(
        as.character(seq(0,100,25)), "%"
      )
    ) + 
    scale_fill_gradient(high = mcv_discrete[6], low = mcv_discrete[1]) +
    labs(title= str_wrap(fiuf, width = 75),
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 20),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "none")
  
  
  ggsave(filename = paste0(
    out, "00_simples/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}
beepr::beep(1)
rm(i, a)

# Proporciones por sexo ----
# * Tabla de frecuencias ----
prop_sexo <- data.frame()
for (i in 1:length(v_names)){
  tempo <- data %>% 
    select(v_names[i],cruce_sexo, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(sexo = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(sexo) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  prop_sexo <- bind_rows(prop_sexo, tempo)
  rm(tempo)
}


# * Gráficas ----
fiuff <- "Proporciones por sexo"

a <- prop_sexo %>% 
  mutate(
    prop = ifelse(
      str_detect(sexo, "ombr"), prop*(-1), prop
    )
  )


for(i in 1:length(v_names_loop)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop[i]), 
         aes(x = reorder(str_wrap(var_v,15), abs(prop), function(x){ sum(x) }), 
             y = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             fill = sexo)) + 
    geom_bar(stat = "identity", width = .6) +
    geom_text(hjust = "outward", size = 7,
              position = position_dodge(width = 0)) +
    scale_y_continuous(
      limits = c(-100,100),
      breaks = seq(-100,100,25),
      labels = paste0(
        c(as.character(seq(100,0,-25)),
          as.character(seq(25,100,25))), "%"
      )
    ) + 
    scale_fill_manual("", values = c(mcv_discrete[6], mcv_discrete[1])) +
    labs(title= str_wrap(fiuf, width = 75),
         subtitle = fiuff,
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 25, hjust = 0.5),
          plot.caption = element_text(size = 20),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "bottom") +
    coord_flip() 
  
  
  ggsave(filename = paste0(
    out, "01_sexo/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}

beepr::beep(1)

rm(i, a)

# Proporciones por grupo de edad ----
# * Tabla de frecuencias ----
prop_grupos_edad <- data.frame()
for (i in 1:length(v_names)){
  tempo <- data %>% 
    select(v_names[i], cruce_grupos_edad, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(grupos_edad = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(grupos_edad) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  prop_grupos_edad <- bind_rows(prop_grupos_edad, tempo)
  rm(tempo)
}



# * Gráficas ----
fiuff <- "Proporciones por grupos de edad"

a <- prop_grupos_edad 

for(i in 1:length(v_names)){
  fiuf <- vector_fiuf[i]
  b <- a %>% 
    filter(v_id == v_names_loop[i]) %>% 
    filter(!prop<0.01)
  ggplot(b, 
         aes(x = paste0(grupos_edad, "\naños"), 
             fill = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             y = var_v)) + 
    geom_tile() +
    geom_text(size = ifelse(length(levels(as.factor(b$var_v)))>6, 2.5, 7), 
              col = "#ffffff") + 
    scale_fill_gradientn(colours = mcv_discrete) +
    labs(title= str_wrap(fiuf, width = 40),
         subtitle = fiuff,
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 25, hjust = 0.5),
          plot.caption = element_text(size = 20),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = "none") +
    coord_fixed()
  
  ggsave(filename = paste0(
    out, "02_grupos_edad/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
  
  rm(b)
}

rm(i, a)

# Proporciones por escolaridad ----
# * Tabla de frecuencias ----
prop_edu <- data.frame()
for (i in 1:length(v_names)){
  tempo <- data %>% 
    select(v_names[i], cruce_edu, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(niv_edu = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(niv_edu) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  prop_edu <- bind_rows(prop_edu, tempo)
  rm(tempo)
}



# * Gráficas ----
fiuff <- "Proporciones por escolaridad"

a <- prop_edu %>% filter(niv_edu!="NE")

for(i in 1:length(v_names_loop)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop[i]), 
         aes(x = reorder(str_wrap(var_v,10), -prop), 
             y = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             fill = prop)) + 
    geom_bar(stat = "identity", width = .3) +
    geom_text(size = 7, vjust = -0.5) +
    facet_wrap(~niv_edu) +
    scale_y_continuous(
      limits = c(0,100),
      breaks = seq(0,100,25),
      labels = paste0(
        as.character(seq(0,100,25)), "%"
      )
    ) + 
    scale_fill_gradient(high = mcv_discrete[6], low = mcv_discrete[1]) +
    labs(title= str_wrap(fiuf, width = 75),
         subtitle = fiuff,
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 25, hjust = 0.5),
          plot.caption = element_text(size = 20),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "none")
  
  
  ggsave(filename = paste0(
    out, "03_escolaridad/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}

# Proporciones por PIB per cápita ----
# * Tabla de frecuencias ----
prop_pib_capita <- data.frame()
for (i in 1:length(v_names)){
  tempo <- data %>% 
    select(v_names[i], cruce_pib_capita, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(pib_capita = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(pib_capita) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  prop_pib_capita <- bind_rows(prop_pib_capita, tempo)
  rm(tempo)
}

prop_pib_capita <- prop_pib_capita %>% 
  mutate(
    pib_capita = case_when(
      pib_capita == "Ingresos alto" ~ "1. Ingresos altos",
      pib_capita == "Ingresos medio" ~ "2. Ingresos medios",
      pib_capita == "Ingresos bajo" ~ "3. Ingresos bajos",
      T ~ NA_character_
    )
  )

# * Gráficas ----
fiuff <- "Proporciones por nivel de PIB per cápita"

a <- prop_pib_capita 

for(i in 1:length(v_names)){
  fiuf <- vector_fiuf[i]
  for(x in 1:3){
    tempo <- 
      ggplot(a %>% filter(v_id == v_names_loop[i], pib_capita==unique(mxmap$pib_per)[x]), 
             aes(x = str_wrap(var_v,10), 
                 y = round(prop*100, 2), 
                 label = paste0(abs(round(prop*100, 2)), "%"))) + 
      geom_bar(stat = "identity", width = .3, fill = mcv_pib[x]) +
      geom_text(size = 7, vjust = -0.5) +
      scale_y_continuous(
        limits = c(0,100),
        breaks = seq(0,100,25),
        labels = paste0(
          as.character(seq(0,100,25)), "%"
        )
      ) + 
      labs(title= unique(mxmap$pib_per)[x]) +
      theme_minimal() +
      theme(plot.title = element_text(size = 25, face = "italic",hjust = 0.5),
            plot.subtitle = element_text(size = 25),
            plot.caption = element_text(size = 20),
            plot.background = element_rect(fill = "transparent",colour = NA),
            text = element_text(family = "Arial Narrow"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            legend.position = "none") 
    assign(paste0("c_", x), tempo)
  }
  
  c_0 <- 
    ggarrange(c_1 + theme(axis.text.y = element_text(size = 12)),
              c_2 + theme(axis.text.y = element_blank()),
              c_3 + theme(axis.text.y = element_blank()),
              # c_4 + theme(axis.text.y = element_text(size = 12)),
              # c_5 + theme(axis.text.y = element_blank()), 
              leyenda,ncol = 2,nrow = 2)
  
  
  
  annotate_figure(c_0,
                  top = text_grob(str_wrap(fiuf,75), size = 30, face = "bold", family = "Arial Narrow"),
                  bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
  )
  
  ggsave(filename = paste0(
    out, "04_pib_capita/", v_num[i], ".png"
  ), width = 20, height = 15, dpi = 100, bg = "transparent")
  
  rm(c_0,c_1,c_2,c_3,c_4,c_5)
}



# Guardar tablas de frecuencias ----
openxlsx::write.xlsx(
  prop_simples, paste0(out, "00_simples/00_props_simples.xlsx")
)

openxlsx::write.xlsx(
  prop_sexo, paste0(out, "01_sexo/00_props_sexo.xlsx")
)

openxlsx::write.xlsx(
  prop_grupos_edad, paste0(out, "02_grupos_edad/00_props_gedad.xlsx")
)

openxlsx::write.xlsx(
  prop_edu, paste0(out, "03_escolaridad/00_props_escolaridad.xlsx")
)

openxlsx::write.xlsx(
  prop_pib_capita, paste0(out, "04_pib_capita/00_props_pib_capita.xlsx")
)


