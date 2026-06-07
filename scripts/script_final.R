# =============================================================
# SCRIPT FINAL — Simulación en terminales portuarios
# Systematic Literature Review — Visualizaciones completas
# n = 87 estudios incluidos (excluye Mathematical Analytical)
# =============================================================
setwd("C:/Users/sebas/OneDrive/Escritorio/Simulation Paradigms in Port Terminal Operations")
# ── Paquetes ──────────────────────────────────────────────────
library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(forcats)

# ── Carga y filtrado base ─────────────────────────────────────
tabla <- read_excel("data/data.xlsx",
                    sheet = "Extracted Data + Decision",
                    skip  = 2)
tabla <- subset(tabla, `Phase 2 Decision` == "Include")
tabla <- subset(tabla, `Main paradigm`    != "Mathematical Analytical")
# n = 87

# ── Fusión de variantes de Container ─────────────────────────
tabla$`Terminal type merged` <- case_when(
  grepl("Container", tabla$`Terminal type`, ignore.case = TRUE) ~ "Container",
  TRUE ~ tabla$`Terminal type`
)

# =============================================================
# BLOQUE 1 — PANORAMA GENERAL
# =============================================================

# ── G01: Papers por año ──────────────────────────────────────
papers_year <- tabla %>%
  count(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(papers_year, aes(x = factor(Year), y = n)) +
  geom_bar(stat = "identity", fill = "#2C7BB6",
           color = "white", linewidth = 0.3, width = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")),
            vjust = -0.3, size = 3, color = "grey25") +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Publications over time",
       subtitle = paste0("Total: ", nrow(tabla), " included studies"),
       x = "Year", y = "Number of papers")
ggsave("outputs/G01_papers_per_year_final.png",
       width = 10, height = 6, bg = "white")

# ── G02: Main paradigm ───────────────────────────────────────
ggplot(tabla, aes(x = fct_infreq(`Main paradigm`),
                  fill = `Main paradigm`)) +
  geom_bar(color = "white", linewidth = 0.3) +
  geom_text(stat  = "count",
            aes(label = paste0(after_stat(count), "\n(",
                               round(after_stat(count) / nrow(tabla) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "none",
        panel.grid.major.x = element_blank()) +
  labs(title = "Main simulation paradigm",
       x = "", y = "Count")
ggsave("outputs/G02_paradigm_final.png",
       width = 8, height = 5, bg = "white")

# ── G03: Real terminal? — explicando "Partial" ───────────────
real_term <- tabla %>%
  count(`Real terminal?`) %>%
  mutate(
    pct   = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", pct, "%)"),
    nota  = case_when(
      `Real terminal?` == "Yes"     ~ "Based on a specific\nreal terminal",
      `Real terminal?` == "Partial" ~ "Uses real terminal data\nbut with simplifications",
      `Real terminal?` == "No"      ~ "Generic / hypothetical\nterminal"
    )
  ) %>%
  mutate(`Real terminal?` = factor(`Real terminal?`,
                                   levels = c("Yes", "Partial", "No")))

ggplot(real_term, aes(y = fct_rev(`Real terminal?`),
                      x = n, fill = `Real terminal?`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  geom_text(aes(label = nota, x = 0.5), hjust = 0,
            size = 2.8, color = "grey45", fontface = "italic") +
  scale_fill_manual(values = c("Yes" = "#2C7BB6",
                               "Partial" = "#ABD9E9",
                               "No"  = "#D7191C")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  labs(title    = "Real terminal basis",
       subtitle = '"Partial" = real data used but operations simplified or generalized',
       x = "Count", y = "")
ggsave("outputs/G03_real_terminal_final.png",
       width = 9, height = 4, bg = "white")

# =============================================================
# BLOQUE 2 — TERMINAL TYPE
# =============================================================

# ── G04: Terminal type fusionado ─────────────────────────────
ggplot(tabla, aes(x = fct_infreq(`Terminal type merged`),
                  fill = `Terminal type merged`)) +
  geom_bar(color = "white", linewidth = 0.3) +
  geom_text(stat  = "count",
            aes(label = paste0(after_stat(count), "\n(",
                               round(after_stat(count) / nrow(tabla) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "none",
        panel.grid.major.x = element_blank()) +
  labs(title    = "Terminal type (container variants merged)",
       subtitle = "Container (Reefer) and Container (Empty) merged into Container",
       x = "", y = "Count")
ggsave("outputs/G04_terminal_type_merged_final.png",
       width = 8, height = 5, bg = "white")

# ── G05: Terminal type por año (apilado) ─────────────────────
# Mostrar si RoRo / Liquid bulk crecen en años recientes
term_year <- tabla %>%
  count(Year, `Terminal type merged`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

term_colors <- c("Container"   = "#2C7BB6",
                 "RoRo"        = "#D7191C",
                 "Dry bulk"    = "#F4A442",
                 "Liquid bulk" = "#1A9641")

ggplot(term_year,
       aes(x = factor(Year), y = n, fill = `Terminal type merged`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(data = subset(term_year, `Terminal type merged` != "Container" & n > 0),
            aes(label = `Terminal type merged`),
            position = position_stack(vjust = 0.5),
            size = 2.6, color = "white", fontface = "bold") +
  scale_fill_manual(values = term_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Terminal type by year — is non-Container growing?",
       subtitle = "Non-Container labels shown inside bars",
       x = "Year", y = "Number of papers", fill = "Terminal type")
ggsave("outputs/G05_terminal_type_by_year_final.png",
       width = 11, height = 6, bg = "white")

# =============================================================
# BLOQUE 3 — OPTIMIZACIÓN
# =============================================================

# ── G07: Métodos de optimización (solo Yes, n=30) ────────────
tabla_opt_yes <- tabla %>%
  filter(`Integrates optimization?` == "Yes",
         !is.na(`Optimization method`),
         `Optimization method` != "Not specified")

tabla_opt_yes$`Opt method grouped` <- case_when(
  grepl("Metaheuristic|metaheuristic|OptQuest|EDA|evolutionary|TPE|BO|SA",
        tabla_opt_yes$`Optimization method`) ~ "Metaheuristic (all variants)",
  grepl("Reinforcement learning|Deep reinforcement",
        tabla_opt_yes$`Optimization method`) ~ "Reinforcement learning / DRL",
  grepl("Linear programming|Mathematical programming|Dynamic programming",
        tabla_opt_yes$`Optimization method`) ~ "Mathematical programming",
  grepl("Genetic algorithm",
        tabla_opt_yes$`Optimization method`) ~ "Genetic algorithm",
  grepl("Multi-agent",
        tabla_opt_yes$`Optimization method`) ~ "Multi-agent optimization",
  grepl("DOE",
        tabla_opt_yes$`Optimization method`) ~ "Design of Experiments",
  grepl("MCDM|AHP",
        tabla_opt_yes$`Optimization method`) ~ "MCDM / AHP",
  TRUE ~ tabla_opt_yes$`Optimization method`
)

# Calcular total para porcentajes
n_total <- nrow(tabla_opt_yes)

ggplot(tabla_opt_yes,
       aes(y = fct_infreq(`Opt method grouped`),
           fill = `Opt method grouped`)) +
  geom_bar(color = "white", linewidth = 0.3) +
  geom_text(stat  = "count",
            aes(label = after_stat(
              paste0(count, " (", round(count / n_total * 100, 1), "%)")
            )),
            hjust = -0.15, size = 3.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  labs(title    = "Optimization methods used (studies with optimization, n=27)",
       subtitle = "Metaheuristic variants grouped; 'Not specified' excluded",
       x = "Count", y = "")

ggsave("outputs/G07_optimization_methods_final.png",
       width = 12, height = 6, bg = "white")

# =============================================================
# BLOQUE 4 — TIME HORIZON
# =============================================================

# ── G08: Time horizon global (barras horizontales con %) ─────
th_global <- tabla %>%
  filter(`Time horizon` != "Not specified") %>%
  count(`Time horizon`) %>%
  mutate(pct  = round(n / sum(n) * 100, 1),
         `Time horizon` = fct_reorder(`Time horizon`, n))

ggplot(th_global, aes(y = `Time horizon`, x = n, fill = `Time horizon`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")),
            hjust = -0.1, size = 3.5) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  labs(title    = "Time horizon of simulation models",
       subtitle = '"Not specified" excluded (n=5)',
       x = "Count", y = "")
ggsave("outputs/G08_time_horizon_global_final.png",
       width = 9, height = 5, bg = "white")

# ── G09: Time horizon por región (% apilado) ─────────────────
th_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Time horizon`)) %>%
  count(Region, `Time horizon`) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(th_region,
       aes(x = reorder(Region, -n, sum),
           y = pct, fill = `Time horizon`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct >= 8, paste0(pct, "%"), "")),
            position = position_stack(vjust = 0.5), size = 2.8) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title    = "Time horizon by region (proportional)",
       subtitle = "Operational dominates Asia-Pacific; Strategic + Multiple dominate Europe",
       x = "", y = "Percentage (%)", fill = "Time horizon")
ggsave("outputs/G09_time_horizon_by_region_final.png",
       width = 12, height = 6, bg = "white")

# =============================================================
# BLOQUE 5 — ANÁLISIS METODOLÓGICO (Sensitivity / Validation)
# =============================================================

# ── G10: Sensitivity analysis por año ───────────────────────
sens_year <- tabla %>%
  count(Year, `Sensitivity analysis?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(sens_year,
       aes(x = factor(Year), y = n,
           fill = `Sensitivity analysis?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(data = subset(sens_year,
                          `Sensitivity analysis?` == "Yes" & n > 0),
            aes(label = paste0(n, "\n(", pct, "%)")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#1A9641", "No" = "#BDBDBD")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Sensitivity analysis over time",
       subtitle = "Green labels = studies with sensitivity analysis (count and %)",
       x = "Year", y = "Number of papers", fill = "Sensitivity analysis")
ggsave("outputs/G10_sensitivity_by_year_final.png",
       width = 11, height = 6, bg = "white")

# ── G11: Validation status por año ──────────────────────────
val_year <- tabla %>%
  count(Year, `Validated?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

val_colors <- c("Yes"           = "#1A9641",
                "Partially"     = "#ABD9E9",
                "No"            = "#D7191C",
                "Not specified" = "#BDBDBD")

ggplot(val_year,
       aes(x = factor(Year), y = n, fill = `Validated?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0, paste0(n, "\n(", pct, "%)"), "")),
            position = position_stack(vjust = 0.5),
            size = 2.3, color = "grey20") +
  scale_fill_manual(values = val_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Validation status over time",
       subtitle = paste0("Overall: ",
                         sum(tabla$`Validated?` == "Yes"), " Yes / ",
                         sum(tabla$`Validated?` == "Partially"), " Partially / ",
                         sum(tabla$`Validated?` == "No"), " No"),
       x = "Year", y = "Number of papers", fill = "Validated?")
ggsave("outputs/G11_validated_by_year_final.png",
       width = 11, height = 6, bg = "white")

# =============================================================
# BLOQUE 6 — SOFTWARE Y ANIMACIÓN
# =============================================================

# ── G12: Software / Tool — evolución temporal ────────────────
# Excluir "Not specified"; mostrar los que tienen n >= 2
top_tools <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "") %>%
  count(`Tool / Software`, sort = TRUE) %>%
  filter(n >= 2) %>%
  pull(`Tool / Software`)

# Normalizar Arena / Matlab → Arena para la línea de Arena
tabla_tools <- tabla %>%
  mutate(`Tool / Software` = case_when(
    `Tool / Software` == "Arena / Matlab" ~ "Arena",
    TRUE ~ `Tool / Software`
  )) %>%
  filter(`Tool / Software` %in% top_tools) %>%
  count(Year, `Tool / Software`)

ggplot(tabla_tools,
       aes(x = factor(Year), y = n,
           color = `Tool / Software`,
           group = `Tool / Software`)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = 0:5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Simulation software — evolution over time",
       subtitle = "Only tools with ≥2 papers; Arena/Matlab merged into Arena",
       x = "Year", y = "Number of papers", color = "Tool / Software")
ggsave("outputs/G12_software_evolution_final.png",
       width = 12, height = 6, bg = "white")

# ── G13: Includes animation? por año ─────────────────────────
anim_year <- tabla %>%
  count(Year, `Includes animation?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(anim_year,
       aes(x = factor(Year), y = n, fill = `Includes animation?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(data = subset(anim_year,
                          `Includes animation?` == "Yes" & n > 0),
            aes(label = paste0(n, " (", pct, "%)")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#E67E22", "No" = "#BDBDBD")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Includes animation? — evolution over time",
       subtitle = paste0("Overall: 13 Yes (15%) — labeled inside bars"),
       x = "Year", y = "Number of papers", fill = "Includes animation?")
ggsave("outputs/G13_animation_by_year_final.png",
       width = 11, height = 6, bg = "white")

# =============================================================
# BLOQUE 7 — SOSTENIBILIDAD Y OBJETIVO DEL MODELO
# =============================================================

# ── G14: Considers sustainability? (global, barras) ──────────
sust_global <- tabla %>%
  count(`Considers sustainability?`) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(sust_global,
       aes(y = fct_reorder(`Considers sustainability?`, n),
           x = n, fill = `Considers sustainability?`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Yes" = "#1A9641", "No" = "#BDBDBD")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  labs(title = "Considers sustainability?", x = "Count", y = "")
ggsave("outputs/G14_sustainability_global_final.png",
       width = 8, height = 4, bg = "white")

# ── G15: Sustainability por región (% apilado) ───────────────
sust_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Considers sustainability?`)) %>%
  count(Region, `Considers sustainability?`) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(sust_region,
       aes(x = reorder(Region, -n, sum),
           y = pct, fill = `Considers sustainability?`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#1A9641", "No" = "#BDBDBD")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title    = "Considers sustainability? by region (proportional)",
       subtitle = "Latin America and Middle East show 0% sustainability focus",
       x = "", y = "Percentage (%)", fill = "Sustainability")
ggsave("outputs/G15_sustainability_by_region_final.png",
       width = 10, height = 5, bg = "white")

# ── G16: Heatmap región × model objective ────────────────────
heat_region_obj <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Model objective`)) %>%
  count(Region, `Model objective`) %>%
  complete(Region, `Model objective`, fill = list(n = 0))

# Abreviar etiquetas largas para el heatmap
heat_region_obj$`Model objective` <- recode(
  heat_region_obj$`Model objective`,
  "Sustainability + Performance evaluation" = "Sust. + Perf. eval.",
  "Sustainability + Capacity planning"      = "Sust. + Cap. plan.",
  "Education and training"                  = "Education/training",
  "Performance evaluation"                  = "Performance eval.",
  "Capacity planning"                       = "Capacity planning"
)

ggplot(heat_region_obj,
       aes(x = `Model objective`, y = Region, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.5) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276",
                      name = "Count") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "Heatmap: region × model objective",
       x = "", y = "")
ggsave("outputs/G16_heatmap_region_objective_final.png",
       width = 13, height = 6, bg = "white")

# =============================================================
# BLOQUE 8 — SUBPROCESOS
# =============================================================

# ── G17: Top subprocesos (agrupados) × región ────────────────
# Crear columna de categoría de subproceso
tabla$`Subprocess category` <- case_when(
  grepl("AGV", tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "AGV routing / dispatching",
  grepl("quay crane|crane assign",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Quay crane scheduling",
  grepl("berth", tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Berth allocation",
  grepl("traffic|congestion|vehicle traffic",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Traffic management",
  grepl("gate", tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Gate operations",
  grepl("yard truck|truck dispatch",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Truck / yard dispatching",
  grepl("stacking|retrieval|container stack|storage",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Stacking / storage",
  grepl("yard plan|yard layout|yard oper",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Yard planning / layout",
  grepl("layout|process layout|design",
        tabla$`Specific subprocess`,
        ignore.case = TRUE)                          ~ "Layout / design",
  grepl("Not specified|^$",
        tabla$`Specific subprocess`)                 ~ NA_character_,
  TRUE                                               ~ "Other"
)

# Top categorías
top_subcat <- tabla %>%
  filter(!is.na(`Subprocess category`),
         `Subprocess category` != "Other") %>%
  count(`Subprocess category`, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(`Subprocess category`)

heat_subproc <- tabla %>%
  filter(`Subprocess category` %in% top_subcat,
         !is.na(Region), Region != "Not specified") %>%
  count(Region, `Subprocess category`) %>%
  complete(Region, `Subprocess category`, fill = list(n = 0))

ggplot(heat_subproc,
       aes(x = Region, y = `Subprocess category`, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.5) +
  scale_fill_gradient(low = "#FDFEFE", high = "#1F618D",
                      name = "Count") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title    = "Top subprocess categories by region",
       subtitle = "AGV routing and traffic management are the most studied across regions",
       x = "", y = "")
ggsave("outputs/G17_subprocess_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# BLOQUE 9 — MAPAS GEOGRÁFICOS
# =============================================================

# ── Preparar geometría mundial ───────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>%
  mutate(
    continent_raw = countrycode(iso_a3, "iso3c", "continent"),
    region_label  = case_when(
      continent_raw == "Americas" &
        subregion %in% c("South America",
                         "Central America",
                         "Caribbean")           ~ "Latin America",
      continent_raw == "Americas"               ~ "North America",
      continent_raw == "Asia" &
        name %in% c("Iran", "Iraq",
                    "Saudi Arabia",
                    "United Arab Emirates",
                    "Kuwait", "Qatar",
                    "Bahrain", "Oman", "Yemen",
                    "Jordan", "Lebanon", "Syria",
                    "Israel", "Turkey")          ~ "Middle East",
      continent_raw == "Asia"                   ~ "Asia-Pacific",
      continent_raw == "Oceania"                ~ "Asia-Pacific",
      continent_raw == "Europe"                 ~ "Europe",
      continent_raw == "Africa"                 ~ "Africa",
      TRUE ~ continent_raw
    )
  )

# ── G18: Mapa — papers por región ────────────────────────────
papers_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  count(Region) %>%
  rename(region_label = Region)

map18 <- world %>% left_join(papers_region, by = "region_label")

ggplot(map18) +
  geom_sf(aes(fill = n), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#D6EAF8", high = "#1A5276",
                      name = "Papers", na.value = "#EAECEE") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  labs(title    = "Distribution of papers by region",
       subtitle = "Asia-Pacific = 39 | Europe = 39 | Latin America = 3 | Middle East = 2")
ggsave("outputs/G18_map_papers_by_region_final.png",
       width = 12, height = 7, bg = "white")

# ── G19: Mapa — papers por país ──────────────────────────────
papers_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  mutate(Country = recode(Country,
                          "USA"       = "United States",
                          "Hong Kong" = "China")) %>%
  count(Country, sort = TRUE)

papers_country$iso_a3 <- countrycode(papers_country$Country,
                                     "country.name", "iso3c",
                                     warn = FALSE)

map19 <- world %>% left_join(papers_country, by = "iso_a3")

ggplot(map19) +
  geom_sf(aes(fill = n), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#D6EAF8", high = "#1A5276",
                      name = "Papers",
                      na.value = "#EAECEE",
                      trans = "sqrt") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  labs(title    = "Papers by country (square-root color scale)",
       subtitle = "China leads with 21 papers; Netherlands 8; UK 7")
ggsave("outputs/G19_map_papers_by_country_final.png",
       width = 12, height = 7, bg = "white")

# ── G20: Mapa — time horizon dominante por región ────────────
dominant_th <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Time horizon`),
         `Time horizon` != "Not specified") %>%
  count(Region, `Time horizon`) %>%
  group_by(Region) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(region_label = Region, dominant_horizon = `Time horizon`)

map20 <- world %>% left_join(dominant_th, by = "region_label")

th_colors_map <- c(
  "Operational"            = "#D7191C",
  "Strategic"              = "#2C7BB6",
  "Multiple"               = "#8E44AD",
  "Operational + Tactical" = "#E67E22",
  "Strategic + Tactical"   = "#1A9641",
  "Tactical"               = "#F4A442"
)

ggplot(map20) +
  geom_sf(aes(fill = dominant_horizon), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = th_colors_map,
                    name   = "Dominant time horizon",
                    na.value = "#EAECEE") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  labs(title    = "Dominant time horizon by region",
       subtitle = "Asia-Pacific → Operational | Europe → Strategic + Multiple")
ggsave("outputs/G20_map_time_horizon_by_region_final.png",
       width = 12, height = 7, bg = "white")

# ── G21: Mapa — % sustainability Yes por región ──────────────
sust_pct <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Considers sustainability?`)) %>%
  group_by(Region) %>%
  summarise(pct_yes = round(mean(`Considers sustainability?` == "Yes") * 100, 1),
            n_total = n()) %>%
  rename(region_label = Region)

map21 <- world %>% left_join(sust_pct, by = "region_label")

ggplot(map21) +
  geom_sf(aes(fill = pct_yes), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#D5F5E3", high = "#1E8449",
                      name = "% Yes",
                      limits = c(0, 100),
                      na.value = "#EAECEE") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  labs(title    = "Considers sustainability? — % Yes by region",
       subtitle = "Asia-Pacific 41% | Europe 44% | Latin America 33% | Middle East 0%")
ggsave("outputs/G21_map_sustainability_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# FIN DEL SCRIPT
# =============================================================

# =============================================================
# MAPAS ADICIONALES — Simulación en terminales portuarios
# Todos los mapas que tienen variación geográfica real
# n = 87 estudios incluidos
# =============================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# ── Carga y filtrado base ─────────────────────────────────────
tabla <- read_excel("data/data.xlsx",
                    sheet = "Extracted Data + Decision",
                    skip  = 2)
tabla <- subset(tabla, `Phase 2 Decision` == "Include")
tabla <- subset(tabla, `Main paradigm`    != "Mathematical Analytical")

tabla$`Terminal type merged` <- ifelse(
  grepl("Container", tabla$`Terminal type`, ignore.case = TRUE),
  "Container", tabla$`Terminal type`
)

# ── Geometría mundial ─────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>%
  mutate(
    continent_raw = countrycode(iso_a3, "iso3c", "continent"),
    region_label  = case_when(
      continent_raw == "Americas" &
        subregion %in% c("South America", "Central America",
                         "Caribbean")           ~ "Latin America",
      continent_raw == "Americas"               ~ "North America",
      continent_raw == "Asia" &
        name %in% c("Iran", "Iraq", "Saudi Arabia",
                    "United Arab Emirates", "Kuwait", "Qatar",
                    "Bahrain", "Oman", "Yemen", "Jordan",
                    "Lebanon", "Syria", "Israel", "Turkey") ~ "Middle East",
      continent_raw == "Asia"                   ~ "Asia-Pacific",
      continent_raw == "Oceania"                ~ "Asia-Pacific",
      continent_raw == "Europe"                 ~ "Europe",
      continent_raw == "Africa"                 ~ "Africa",
      TRUE ~ continent_raw
    )
  )

# ── Helper: join por país ─────────────────────────────────────
join_country <- function(df_metric) {
  df_metric <- df_metric %>%
    mutate(Country = recode(Country,
                            "USA"       = "United States",
                            "Hong Kong" = "China"))
  df_metric$iso_a3 <- countrycode(df_metric$Country,
                                  "country.name", "iso3c", warn = FALSE)
  world %>% left_join(df_metric, by = "iso_a3")
}

# ── Tema base para todos los mapas ───────────────────────────
tema_mapa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================
# MAPA A — % que integra optimización por país
# Asia-Pacific tiene tasa de optimización muy alta vs Europa
# =============================================================
opt_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total  = n(),
    n_yes    = sum(`Integrates optimization?` == "Yes"),
    pct_opt  = round(n_yes / n_total * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapA <- join_country(opt_country)

ggplot(mapA) +
  geom_sf(aes(fill = pct_opt), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#FEF9E7", high = "#B7950B",
                      name = "% papers\nwith optimization",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Integrates optimization? — % Yes by country",
    subtitle = "Germany 67% | Indonesia 60% | China 52% | Netherlands 12% | Italy 0%\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_A_optimization_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA B — % con análisis de sensibilidad por país
# Notable: Países Bajos y UK tienen 0%; Alemania 67%
# =============================================================
sens_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total = n(),
    pct_yes = round(sum(`Sensitivity analysis?` == "Yes") / n() * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapB <- join_country(sens_country)

ggplot(mapB) +
  geom_sf(aes(fill = pct_yes), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276",
                      name = "% papers with\nsensitivity analysis",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Sensitivity analysis — % Yes by country",
    subtitle = "Germany 67% | India & Malaysia 50% | Netherlands, UK, Sweden, Poland, France → 0%\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_B_sensitivity_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA C — % con validación "Yes" por país
# (excluye Partial, No, Not specified — solo Yes puros)
# =============================================================
val_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total   = n(),
    pct_valid = round(sum(`Validated?` == "Yes") / n() * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapC <- join_country(val_country)

ggplot(mapC) +
  geom_sf(aes(fill = pct_valid), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EAFAF1", high = "#1E8449",
                      name = "% validated\n(Yes only)",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Model validation (Yes) — % by country",
    subtitle = "France 50% | Germany 67% | Netherlands 0% (7 No) | Indonesia 0%\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_C_validation_yes_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA D — % con animación por país
# Italia 50%, Turquía 50% son outliers curiosos
# =============================================================
anim_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total  = n(),
    pct_anim = round(sum(`Includes animation?` == "Yes") / n() * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapD <- join_country(anim_country)

ggplot(mapD) +
  geom_sf(aes(fill = pct_anim), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#FEF5E7", high = "#E67E22",
                      name = "% papers\nwith animation",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Includes animation? — % Yes by country",
    subtitle = "Italy 50% | Turkey 50% | Sweden 33% | UK, Netherlands, Germany → 0%\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_D_animation_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA E — % sostenibilidad por país
# Indonesia 80% es el dato más llamativo
# =============================================================
sust_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total  = n(),
    pct_sust = round(sum(`Considers sustainability?` == "Yes") / n() * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapE <- join_country(sust_country)

ggplot(mapE) +
  geom_sf(aes(fill = pct_sust), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EAFAF1", high = "#1E8449",
                      name = "% papers\nwith sustainability",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Considers sustainability? — % Yes by country",
    subtitle = "Indonesia 80% | Netherlands 62% | France, India, Sweden, Turkey → 0%\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_E_sustainability_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA F — % optimización por REGIÓN (mapa coropleta)
# Asia-Pacific 51% vs Europa 18% — brecha enorme
# =============================================================
opt_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  group_by(Region) %>%
  summarise(
    n_total = n(),
    pct_opt = round(sum(`Integrates optimization?` == "Yes") / n() * 100, 1)
  ) %>%
  rename(region_label = Region)

mapF <- world %>% left_join(opt_region, by = "region_label")

ggplot(mapF) +
  geom_sf(aes(fill = pct_opt), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#FEF9E7", high = "#B7950B",
                      name = "% papers\nwith optimization",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Integrates optimization? — % Yes by region",
    subtitle = "Asia-Pacific 51% vs Europe 18% — notable methodological gap"
  )
ggsave("outputs/MAPA_F_optimization_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA G — Paradigma dominante por país
# (DES domina todo, pero Indonesia es ABM y Malaysia es SDM)
# =============================================================
paradigm_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total          = n(),
    dominant_paradigm = `Main paradigm`[which.max(tabulate(
      match(`Main paradigm`, unique(`Main paradigm`))))]
  ) %>%
  filter(n_total >= 2)

mapG <- join_country(paradigm_country)

paradigm_colors <- c(
  "DES"    = "#2C7BB6",
  "ABM"    = "#D7191C",
  "SDM"    = "#1A9641",
  "Hybrid" = "#8E44AD"
)

ggplot(mapG) +
  geom_sf(aes(fill = dominant_paradigm), color = "white", linewidth = 0.2) +
  scale_fill_manual(values    = paradigm_colors,
                    name      = "Dominant paradigm",
                    na.value  = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Dominant simulation paradigm by country",
    subtitle = "DES dominates globally | Indonesia → ABM | Malaysia → SDM (each 1 SDM paper)\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_G_paradigm_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA H — % Real terminal "Yes" por país
# (qué tan aplicados son los modelos a terminales reales)
# =============================================================
real_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total  = n(),
    pct_real = round(sum(`Real terminal?` == "Yes") / n() * 100, 1)
  ) %>%
  filter(n_total >= 2)

mapH <- join_country(real_country)

ggplot(mapH) +
  geom_sf(aes(fill = pct_real), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276",
                      name = "% papers on\nreal terminals",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Based on real terminal — % Yes by country",
    subtitle = "France, Malaysia, Poland, Sweden → 100% | Indonesia, South Korea → lower real terminal use\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_H_real_terminal_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA I — Año promedio de publicación por país
# (qué países publican más recientemente)
# =============================================================
year_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_total  = n(),
    mean_year = round(mean(Year, na.rm = TRUE), 1)
  ) %>%
  filter(n_total >= 2)

mapI <- join_country(year_country)

ggplot(mapI) +
  geom_sf(aes(fill = mean_year), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#FDFEFE", high = "#1F618D",
                      name = "Mean publication\nyear",
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Mean publication year by country",
    subtitle = "Sweden 2022.7 | Indonesia 2022.4 | South Korea 2022.3 | France 2016.5 (earliest avg)\nGrey = countries with <2 papers"
  )
ggsave("outputs/MAPA_I_mean_year_by_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA J — % Real terminal "Partial" por región
# (ver dónde se simplifica más el terminal real)
# =============================================================
partial_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  group_by(Region) %>%
  summarise(
    n_total    = n(),
    pct_partial = round(sum(`Real terminal?` == "Partial") / n() * 100, 1)
  ) %>%
  rename(region_label = Region)

mapJ <- world %>% left_join(partial_region, by = "region_label")

ggplot(mapJ) +
  geom_sf(aes(fill = pct_partial), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#F9F3E3", high = "#935116",
                      name = "% papers with\npartial real terminal",
                      limits = c(0, 60),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "\"Partial\" real terminal — % by region",
    subtitle = "Asia-Pacific 38% | Europe 31% — where are models most simplified vs real terminals?"
  )
ggsave("outputs/MAPA_J_partial_terminal_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA K — Objetivo dominante del modelo por región
# (Asia-Pacific → Optimization; Europa → Performance eval + Multiple)
# =============================================================
obj_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Model objective`)) %>%
  group_by(Region) %>%
  summarise(
    dominant_obj = `Model objective`[which.max(tabulate(
      match(`Model objective`, unique(`Model objective`))))]
  ) %>%
  rename(region_label = Region)

mapK <- world %>% left_join(obj_region, by = "region_label")

obj_colors <- c(
  "Multiple"                                 = "#8E44AD",
  "Optimization"                             = "#B7950B",
  "Performance evaluation"                   = "#2C7BB6",
  "Sustainability"                           = "#1A9641",
  "Capacity planning"                        = "#D7191C",
  "Sustainability + Performance evaluation"  = "#1F618D",
  "Education and training"                   = "#E67E22"
)

ggplot(mapK) +
  geom_sf(aes(fill = dominant_obj), color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = obj_colors,
                    name     = "Dominant model\nobjective",
                    na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Dominant model objective by region",
    subtitle = "Asia-Pacific → Optimization | Europe → Multiple | Latin America → Performance eval."
  )
ggsave("outputs/MAPA_K_model_objective_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# FIN
# =============================================================
message("✓ 11 mapas adicionales generados (MAPA_A a MAPA_K)")
message("✓ Guardados en outputs/ con sufijo _final")

# =============================================================
# SOFTWARE TOOLS × GEOGRAFÍA — Mapas y gráficos vinculados
# n = 87 incluidos | 39 con software especificado (45%)
# =============================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# ── Carga y filtrado ──────────────────────────────────────────
tabla <- read_excel("data/data.xlsx",
                    sheet = "Extracted Data + Decision",
                    skip  = 2)
tabla <- subset(tabla, `Phase 2 Decision` == "Include")
tabla <- subset(tabla, `Main paradigm`    != "Mathematical Analytical")

# Normalizar variantes de Arena
tabla$Tool <- tabla$`Tool / Software`
tabla$Tool[tabla$Tool == "Arena / Matlab"]   <- "Arena"
tabla$Tool[tabla$Tool == "Arena, ProModel"]  <- "Arena"

# Dataset filtrado: solo papers con software declarado
df_tool <- tabla %>%
  filter(!is.na(Tool), !Tool %in% c("Not specified", ""))

# Top 5 tools (n >= 2)
top_tools <- c("Arena", "AnyLogic", "Custom code", "Plant Simulation", "FlexSim")

# Colores fijos por tool (consistentes en todos los gráficos)
tool_colors <- c(
  "Arena"            = "#D7191C",
  "AnyLogic"         = "#2C7BB6",
  "Custom code"      = "#1A9641",
  "Plant Simulation" = "#E67E22",
  "FlexSim"          = "#8E44AD",
  "Other"            = "#BDBDBD"
)

# ── Geometría mundial ─────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>%
  mutate(
    continent_raw = countrycode(iso_a3, "iso3c", "continent"),
    region_label  = case_when(
      continent_raw == "Americas" &
        subregion %in% c("South America", "Central America",
                         "Caribbean")           ~ "Latin America",
      continent_raw == "Americas"               ~ "North America",
      continent_raw == "Asia" &
        name %in% c("Iran","Iraq","Saudi Arabia",
                    "United Arab Emirates","Kuwait","Qatar",
                    "Bahrain","Oman","Yemen","Jordan",
                    "Lebanon","Syria","Israel","Turkey") ~ "Middle East",
      continent_raw == "Asia"                   ~ "Asia-Pacific",
      continent_raw == "Oceania"                ~ "Asia-Pacific",
      continent_raw == "Europe"                 ~ "Europe",
      continent_raw == "Africa"                 ~ "Africa",
      TRUE ~ continent_raw
    )
  )

tema_mapa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, color = "grey40")
  )

# =============================================================
# GRÁFICO 1 — Distribución global de tools (con % de los 39)
# Punto de entrada: mostrar el panorama antes de los mapas
# =============================================================
tool_global <- df_tool %>%
  mutate(Tool_plot = ifelse(Tool %in% top_tools, Tool, "Other")) %>%
  count(Tool_plot) %>%
  mutate(
    pct      = round(n / nrow(df_tool) * 100, 1),
    pct_all  = round(n / nrow(tabla)   * 100, 1),
    Tool_plot = fct_reorder(Tool_plot, n)
  )

ggplot(tool_global,
       aes(y = Tool_plot, x = n,
           fill = Tool_plot)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(n, "  (", pct, "% of specified | ",
                               pct_all, "% of all)")),
            hjust = -0.05, size = 3.2) +
  scale_fill_manual(values = tool_colors, na.value = "#BDBDBD") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.55))) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        panel.grid.major.y = element_blank()) +
  labs(
    title    = "Simulation tools / software used",
    subtitle = paste0("39 of 87 papers (45%) specify a tool — 48 say 'Not specified'"),
    x = "Count", y = ""
  )
ggsave("outputs/SW01_tools_global_final.png",
       width = 11, height = 5, bg = "white")

# =============================================================
# GRÁFICO 2 — Stacked bar: Tool × Región
# Muestra cómo cada región prefiere distintas herramientas
# =============================================================
tool_region <- df_tool %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  mutate(Tool_plot = ifelse(Tool %in% top_tools, Tool, "Other")) %>%
  count(Region, Tool_plot) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

ggplot(tool_region,
       aes(x = reorder(Region, -n, sum),
           y = n, fill = Tool_plot)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n >= 1, Tool_plot, "")),
            position = position_stack(vjust = 0.5),
            size = 2.6, color = "white", fontface = "bold") +
  scale_fill_manual(values = tool_colors, na.value = "#BDBDBD") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right") +
  labs(
    title    = "Software tools by region",
    subtitle = "Asia-Pacific: Arena = AnyLogic | Europe: more Custom code | Labels inside bars",
    x = "", y = "Number of papers", fill = "Tool"
  )
ggsave("outputs/SW02_tools_by_region_bar_final.png",
       width = 11, height = 6, bg = "white")

# =============================================================
# GRÁFICO 3 — Heatmap: Top 5 tools × Región (n absolutos)
# Más legible que el stacked cuando hay muchas categorías
# =============================================================
heat_tool_region <- df_tool %>%
  filter(!is.na(Region), Region != "Not specified",
         Tool %in% top_tools) %>%
  count(Region, Tool) %>%
  complete(Region, Tool, fill = list(n = 0))

ggplot(heat_tool_region,
       aes(x = Tool, y = Region, fill = n)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = ifelse(n > 0, n, "–")), size = 4) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276",
                      name = "Papers") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank()) +
  labs(
    title    = "Heatmap: top 5 tools × region",
    subtitle = "Arena distributed across regions | AnyLogic concentrated in Asia-Pacific",
    x = "", y = ""
  )
ggsave("outputs/SW03_heatmap_tools_region_final.png",
       width = 10, height = 5, bg = "white")

# =============================================================
# GRÁFICO 4 — Heatmap: Top 5 tools × País (países con ≥1 tool)
# =============================================================
heat_tool_country <- df_tool %>%
  filter(!is.na(Country), Country != "Not specified", Country != "",
         Tool %in% top_tools) %>%
  count(Country, Tool) %>%
  complete(Country, Tool, fill = list(n = 0))

# Ordenar países por total de papers con tool
country_order <- heat_tool_country %>%
  group_by(Country) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  pull(Country)

heat_tool_country$Country <- factor(heat_tool_country$Country,
                                    levels = rev(country_order))

ggplot(heat_tool_country,
       aes(x = Tool, y = Country, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n > 0, n, "–")), size = 3.5) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276",
                      name = "Papers") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank()) +
  labs(
    title    = "Heatmap: top 5 tools × country",
    subtitle = "China uses AnyLogic (4) | Netherlands uses Custom code (2) | Italy uses Arena (2)",
    x = "", y = ""
  )
ggsave("outputs/SW04_heatmap_tools_country_final.png",
       width = 10, height = 8, bg = "white")

# =============================================================
# GRÁFICO 5 — Evolución temporal por tool (líneas)
# =============================================================
tool_year <- df_tool %>%
  filter(Tool %in% top_tools) %>%
  count(Year, Tool) %>%
  complete(Year, Tool, fill = list(n = 0))

ggplot(tool_year,
       aes(x = factor(Year), y = n,
           color = Tool, group = Tool)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  scale_color_manual(values = tool_colors) +
  scale_y_continuous(breaks = 0:5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Software tool evolution over time",
    subtitle = "Arena peaked 2016 | AnyLogic rising since 2018 | Custom code declining after 2021",
    x = "Year", y = "Papers", color = "Tool"
  )
ggsave("outputs/SW05_tools_evolution_final.png",
       width = 12, height = 6, bg = "white")

# =============================================================
# MAPA 1 — Tool dominante por país (mapa coropleta)
# Usa solo países con ≥1 paper con tool declarado
# =============================================================
tool_dominant_country <- df_tool %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_tool         = n(),
    dominant_tool  = Tool[which.max(tabulate(match(Tool, unique(Tool))))]
  ) %>%
  mutate(
    dominant_tool_plot = ifelse(dominant_tool %in% top_tools,
                                dominant_tool, "Other"),
    Country = recode(Country,
                     "USA"       = "United States",
                     "Hong Kong" = "China")
  )

tool_dominant_country$iso_a3 <- countrycode(
  tool_dominant_country$Country, "country.name", "iso3c", warn = FALSE)

map_tool_dominant <- world %>%
  left_join(tool_dominant_country, by = "iso_a3")

tool_colors_map <- c(tool_colors,
                     "Other" = "#BDBDBD")

ggplot(map_tool_dominant) +
  geom_sf(aes(fill = dominant_tool_plot),
          color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = tool_colors_map,
                    name     = "Dominant tool",
                    na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Dominant simulation tool by country",
    subtitle = "China → AnyLogic | Italy, UK, South Korea → Arena | Netherlands → Custom code\nGrey = no tool specified or no papers"
  )
ggsave("outputs/SW06_map_dominant_tool_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA 2 — % AnyLogic por país
# AnyLogic está claramente concentrado en Asia-Pacific
# =============================================================
anylogic_country <- df_tool %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_tool   = n(),
    pct_any  = round(sum(Tool == "AnyLogic") / n() * 100, 1)
  ) %>%
  mutate(Country = recode(Country,
                          "USA"       = "United States",
                          "Hong Kong" = "China"))

anylogic_country$iso_a3 <- countrycode(
  anylogic_country$Country, "country.name", "iso3c", warn = FALSE)

map_anylogic <- world %>%
  left_join(anylogic_country, by = "iso_a3")

ggplot(map_anylogic) +
  geom_sf(aes(fill = pct_any), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EBF5FB", high = "#2C7BB6",
                      name = "% papers\nusing AnyLogic",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "AnyLogic — share of papers by country",
    subtitle = "China: 4 of 11 papers with tool (36%) | Portugal, Sweden: 100% (n=1 each)\nGrey = no tool specified"
  )
ggsave("outputs/SW07_map_anylogic_share_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA 3 — % Arena por país
# Arena más distribuido geográficamente que AnyLogic
# =============================================================
arena_country <- df_tool %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  group_by(Country) %>%
  summarise(
    n_tool   = n(),
    pct_arena = round(sum(Tool == "Arena") / n() * 100, 1)
  ) %>%
  mutate(Country = recode(Country,
                          "USA"       = "United States",
                          "Hong Kong" = "China"))

arena_country$iso_a3 <- countrycode(
  arena_country$Country, "country.name", "iso3c", warn = FALSE)

map_arena <- world %>%
  left_join(arena_country, by = "iso_a3")

ggplot(map_arena) +
  geom_sf(aes(fill = pct_arena), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#FDEDEC", high = "#D7191C",
                      name = "% papers\nusing Arena",
                      limits = c(0, 100),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Arena — share of papers by country",
    subtitle = "Italy 67% | India, South Korea, Mexico: 100% (n=1 each) | Netherlands: 0%\nGrey = no tool specified"
  )
ggsave("outputs/SW08_map_arena_share_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA 4 — % Custom code por región
# Europa lidera en desarrollo de código propio
# =============================================================
custom_region <- df_tool %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  group_by(Region) %>%
  summarise(
    n_tool      = n(),
    pct_custom  = round(sum(Tool == "Custom code") / n() * 100, 1)
  ) %>%
  rename(region_label = Region)

map_custom <- world %>%
  left_join(custom_region, by = "region_label")

ggplot(map_custom) +
  geom_sf(aes(fill = pct_custom), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EAFAF1", high = "#1E8449",
                      name = "% papers using\nCustom code",
                      limits = c(0, 50),
                      labels = function(x) paste0(x, "%"),
                      na.value = "#EAECEE") +
  tema_mapa +
  labs(
    title    = "Custom code — share of papers by region",
    subtitle = "Europe 21% | Asia-Pacific 11% | Middle East 50% (n=2, 1 custom code)\nGrey = no tool specified or no papers"
  )
ggsave("outputs/SW09_map_custom_code_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# GRÁFICO 6 — Small multiples: un panel por tool, mapa de puntos
# Muestra distribución geográfica de cada tool individualmente
# Útil como figura de resumen para presentación/paper
# =============================================================
# Preparar coordenadas de centroides por país
tool_country_coords <- df_tool %>%
  filter(Tool %in% top_tools,
         !is.na(Country), Country != "Not specified", Country != "") %>%
  mutate(Country = recode(Country,
                          "USA"       = "United States",
                          "Hong Kong" = "China")) %>%
  group_by(Tool, Country) %>%
  summarise(n = n(), .groups = "drop")

tool_country_coords$iso_a3 <- countrycode(
  tool_country_coords$Country, "country.name", "iso3c", warn = FALSE)

# Obtener centroides del world
centroids <- world %>%
  st_centroid() %>%
  select(iso_a3, geometry) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lon = X, lat = Y) %>%
  bind_cols(world %>% st_centroid() %>% select(iso_a3) %>% st_drop_geometry())

tool_coords <- tool_country_coords %>%
  left_join(centroids, by = "iso_a3") %>%
  filter(!is.na(lon))

ggplot() +
  geom_sf(data = world, fill = "#F0F0F0", color = "white",
          linewidth = 0.2) +
  geom_point(data = tool_coords,
             aes(x = lon, y = lat, size = n, color = Tool),
             alpha = 0.85) +
  scale_size_continuous(range = c(3, 10), name = "Papers") +
  scale_color_manual(values = tool_colors) +
  facet_wrap(~Tool, ncol = 2) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    strip.text   = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  ) +
  labs(
    title    = "Geographic distribution of each simulation tool",
    subtitle = "Bubble size = number of papers | Each panel shows one tool",
    x = "", y = ""
  )
ggsave("outputs/SW10_map_small_multiples_tools_final.png",
       width = 12, height = 10, bg = "white")

# =============================================================
# FIN
# =============================================================
message("✓ 10 gráficos/mapas de software generados (SW01–SW10)")
message("✓ Guardados en outputs/ con sufijo _final")

# =============================================================
# MAPA — Top subprocess por región (coropleta)
# Usa la misma lógica de agrupación del script principal
# =============================================================

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# ── 1. Clasificar subprocesos (explode de entradas múltiples) ─
tabla_sub <- tabla %>%
  mutate(subprocess_raw = strsplit(as.character(`Specific subprocess`), ",\\s*")) %>%
  tidyr::unnest(subprocess_raw) %>%
  filter(!is.na(subprocess_raw),
         !trimws(subprocess_raw) %in% c("Not specified", ""))

tabla_sub$subprocess_grouped <- dplyr::case_when(
  grepl("AGV",                                          tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "AGV routing / dispatching",
  grepl("quay crane|crane assign|crane sched",          tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Quay crane scheduling",
  grepl("berth",                                        tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Berth allocation",
  grepl("traffic|congestion|vehicle traffic",           tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Traffic management",
  grepl("gate",                                         tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Gate operations",
  grepl("truck dispatch|yard truck",                    tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Truck / yard dispatching",
  grepl("stacking|retrieval|container stack|storage",   tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Stacking / storage",
  grepl("yard plan|yard layout|yard oper|yard crane",   tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Yard planning / layout",
  grepl("layout|process layout|design",                 tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Layout / design",
  grepl("container routing",                            tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Container routing",
  grepl("stowage",                                      tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Stowage planning",
  grepl("capacity planning|terminal capacity",          tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Capacity planning",
  grepl("intermodal",                                   tabla_sub$subprocess_raw, ignore.case = TRUE) ~ "Intermodal operations",
  TRUE ~ NA_character_
) 

# ── 2. Subprocess dominante por región ───────────────────────
# Subprocess dominante por región
dominant_sub <- tabla_sub %>%
  filter(!is.na(subprocess_grouped),
         !is.na(Region), Region != "Not specified") %>%
  count(Region, subprocess_grouped) %>%
  group_by(Region) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(region_label = Region, dominant_subprocess = subprocess_grouped)

# Subtítulo con n, % y nota de empate para Latin America
sub_stats <- tabla_sub %>%
  filter(!is.na(subprocess_grouped),
         !is.na(Region), Region != "Not specified") %>%
  count(Region, subprocess_grouped) %>%
  group_by(Region) %>%
  mutate(
    pct       = round(n / sum(n) * 100, 1),
    n_tied    = sum(n == max(n)),
    total_sub = sum(n)
  ) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

subtitle_text <- sub_stats %>%
  arrange(desc(total_sub)) %>%
  mutate(label = case_when(
    n_tied > 1 & Region == "Latin America" ~ paste0(Region, " → ", subprocess_grouped,
                                                    " (n=", n, "/", total_sub, ", ", pct,
                                                    "% — tied with Stacking / storage)"),
    n_tied > 1 ~ paste0(Region, " → ", subprocess_grouped,
                        " (n=", n, "/", total_sub, ", ", pct,
                        "% — tied)"),
    TRUE       ~ paste0(Region, " → ", subprocess_grouped,
                        " (n=", n, "/", total_sub, ", ", pct, "%)")
  )) %>%
  pull(label) %>%
  paste(collapse = " | ")

map_sub <- world %>% left_join(dominant_sub, by = "region_label")

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map_sub %>% filter(!is.na(dominant_subprocess)),
          aes(fill = dominant_subprocess), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values       = sub_colors,
    name         = "Dominant subprocess",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  tema_mapa +
  labs(
    title    = "Dominant specific subprocess by region",
    subtitle = stringr::str_wrap(subtitle_text, width = 130)
  )

ggsave("outputs/MAPA_SUB_dominant_subprocess_by_region_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# VALIDATION METHOD — Top 10 + evolución temporal + mapa
# =============================================================

# ── 1. Explotar entradas múltiples y agrupar ─────────────────
tabla_val <- tabla %>%
  filter(`Validated?` == "Yes",
         !is.na(`Validation method`),
         `Validation method` != "Not specified") %>%
  mutate(val_raw = strsplit(as.character(`Validation method`), ",\\s*")) %>%
  tidyr::unnest(val_raw) %>%
  mutate(val_raw = trimws(val_raw)) %>%
  mutate(val_grouped = case_when(
    grepl("historical data|real data|empirical data|observed data|real system",
          val_raw, ignore.case = TRUE)                        ~ "Historical / real data",
    grepl("statistical test|statistical anal|robustness",
          val_raw, ignore.case = TRUE)                        ~ "Statistical tests",
    grepl("expert judgment|expert judge|operations team|operators",
          val_raw, ignore.case = TRUE)                        ~ "Expert judgment",
    grepl("literature comparison",
          val_raw, ignore.case = TRUE)                        ~ "Literature comparison",
    grepl("model comparison|comparison with simulation|mathematical comparison",
          val_raw, ignore.case = TRUE)                        ~ "Model / simulation comparison",
    grepl("calibration",
          val_raw, ignore.case = TRUE)                        ~ "Calibration",
    grepl("animation",
          val_raw, ignore.case = TRUE)                        ~ "Animation",
    grepl("theoretical formulas",
          val_raw, ignore.case = TRUE)                        ~ "Theoretical formulas",
    grepl("replications",
          val_raw, ignore.case = TRUE)                        ~ "Statistical tests",
    TRUE ~ "Other"
  )) %>%
  filter(val_grouped != "Other")

# ── 2. Top 10 global (barras horizontales) ───────────────────
n_val_total <- nrow(tabla_val)

val_global <- tabla_val %>%
  count(val_grouped, sort = TRUE) %>%
  mutate(
    pct       = round(n / n_val_total * 100, 1),
    val_grouped = fct_reorder(val_grouped, n)
  )

ggplot(val_global, aes(y = val_grouped, x = n, fill = val_grouped)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")),
            hjust = -0.1, size = 3.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        panel.grid.major.y = element_blank()) +
  labs(
    title    = "Validation methods used (studies with validation, n=Yes only)",
    subtitle = "Multi-method entries exploded; grouped by category",
    x = "Count", y = ""
  )

ggsave("outputs/VAL01_validation_methods_global_final.png",
       width = 10, height = 5, bg = "white")

# ── 3. Evolución temporal (apilado) ──────────────────────────
val_year <- tabla_val %>%
  count(Year, val_grouped) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(val_year,
       aes(x = factor(Year), y = n, fill = val_grouped)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0,
                               paste0(val_grouped, "\n(", pct, "%)"),
                               "")),
            position = position_stack(vjust = 0.5),
            size = 2.4, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(
    title    = "Validation methods over time",
    subtitle = "Historical / real data consistently dominant; statistical tests emerging post-2018",
    x = "Year", y = "Number of papers", fill = "Validation method"
  )

ggsave("outputs/VAL02_validation_methods_by_year_final.png",
       width = 12, height = 6, bg = "white")

# ── 4. Mapa — método dominante por región ────────────────────
dominant_val <- tabla_val %>%
  filter(!is.na(Region), Region != "Not specified") %>%
  count(Region, val_grouped) %>%
  group_by(Region) %>%
  mutate(
    pct       = round(n / sum(n) * 100, 1),
    n_tied    = sum(n == max(n)),
    total_val = sum(n)
  ) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

val_subtitle <- dominant_val %>%
  arrange(desc(total_val)) %>%
  mutate(label = paste0(Region, " → ", val_grouped,
                        " (n=", n, "/", total_val, ", ", pct, "%",
                        ifelse(n_tied > 1, " — tied)", ")"))) %>%
  pull(label) %>%
  paste(collapse = " | ")

dominant_val_join <- dominant_val %>%
  rename(region_label = Region, dominant_val = val_grouped)

map_val <- world %>% left_join(dominant_val_join, by = "region_label")

val_colors <- c(
  "Historical / real data"       = "#2C7BB6",
  "Statistical tests"            = "#D7191C",
  "Expert judgment"              = "#1A9641",
  "Literature comparison"        = "#F4A442",
  "Model / simulation comparison"= "#8E44AD",
  "Calibration"                  = "#E67E22",
  "Animation"                    = "#16A085",
  "Theoretical formulas"         = "#7F8C8D"
)

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map_val %>% filter(!is.na(dominant_val)),
          aes(fill = dominant_val), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values       = val_colors,
    name         = "Dominant validation method",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  tema_mapa +
  labs(
    title    = "Dominant validation method by region",
    subtitle = stringr::str_wrap(val_subtitle, width = 130)
  )

ggsave("outputs/VAL03_validation_method_by_region_map_final.png",
       width = 12, height = 7, bg = "white")

tabla %>%
  filter(`Validated?` == "Partially",
         !is.na(`Validation method`),
         `Validation method` != "Not specified") %>%
  count(`Validation method`, sort = TRUE)

# =============================================================
# VAL04 — Validation method × Validated? (Yes + Partially)
# =============================================================

tabla_val_status <- tabla %>%
  filter(`Validated?` %in% c("Yes", "Partially"),
         !is.na(`Validation method`),
         `Validation method` != "Not specified") %>%
  mutate(val_raw = strsplit(as.character(`Validation method`), ",\\s*")) %>%
  tidyr::unnest(val_raw) %>%
  mutate(val_raw = trimws(val_raw)) %>%
  mutate(val_grouped = case_when(
    grepl("historical data|real data|empirical data|observed data|real system",
          val_raw, ignore.case = TRUE)                         ~ "Historical / real data",
    grepl("statistical test|statistical anal|robustness|replications",
          val_raw, ignore.case = TRUE)                         ~ "Statistical tests",
    grepl("expert judgment|expert judge|operations team|operators",
          val_raw, ignore.case = TRUE)                         ~ "Expert judgment",
    grepl("literature comparison",
          val_raw, ignore.case = TRUE)                         ~ "Literature comparison",
    grepl("model comparison|comparison with simulation|mathematical comparison",
          val_raw, ignore.case = TRUE)                         ~ "Model / simulation comparison",
    grepl("calibration",
          val_raw, ignore.case = TRUE)                         ~ "Calibration",
    grepl("animation",
          val_raw, ignore.case = TRUE)                         ~ "Animation",
    grepl("theoretical formulas",
          val_raw, ignore.case = TRUE)                         ~ "Theoretical formulas",
    TRUE ~ "Other"
  )) %>%
  filter(val_grouped != "Other")

# Orden por total de apariciones
order_val <- tabla_val_status %>%
  count(val_grouped, sort = TRUE) %>%
  pull(val_grouped)

tabla_val_status <- tabla_val_status %>%
  mutate(
    val_grouped  = factor(val_grouped, levels = rev(order_val)),
    `Validated?` = factor(`Validated?`, levels = c("Yes", "Partially"))
  )

# Calcular n y % por combinación
val_status_count <- tabla_val_status %>%
  count(val_grouped, `Validated?`) %>%
  group_by(val_grouped) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

ggplot(val_status_count,
       aes(y = val_grouped, x = n, fill = `Validated?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0,
                               paste0(n, " (", pct, "%)"),
                               "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Yes" = "#1A9641", "Partially" = "#ABD9E9"),
    name   = "Validated?"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "right",
        panel.grid.major.y = element_blank()) +
  labs(
    title    = "Validation method × validation status",
    subtitle = paste0("Yes = ", sum(tabla$`Validated?` == "Yes"),
                      " | Partially = ", sum(tabla$`Validated?` == "Partially"),
                      " | multi-method entries exploded"),
    x = "Count", y = ""
  )

ggsave("outputs/VAL04_validation_method_by_status_final.png",
       width = 10, height = 5, bg = "white")

# =============================================================
# SW_MAP_EVOL — Tool dominante por región × período temporal
# =============================================================

# ── 1. Normalizar tools y crear períodos ─────────────────────
top_tools <- c("Arena", "AnyLogic", "Custom code", "Plant Simulation", "FlexSim")

tabla_tool_evol <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "",
         !is.na(Region), Region != "Not specified") %>%
  mutate(
    Tool = case_when(
      grepl("Arena", `Tool / Software`, ignore.case = TRUE)  ~ "Arena",
      grepl("AnyLogic", `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE)            ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      TRUE ~ "Other"
    ),
    Period = case_when(
      Year <= 2017 ~ "2015–2017",
      Year <= 2020 ~ "2018–2020",
      Year <= 2022 ~ "2021–2022",
      TRUE         ~ "2023–2025"
    ),
    Period = factor(Period, levels = c("2015–2017", "2018–2020",
                                       "2021–2022", "2023–2025"))
  ) %>%
  filter(Tool != "Other")

# ── 2. Tool dominante por región × período ───────────────────
dominant_tool_evol <- tabla_tool_evol %>%
  count(Period, Region, Tool) %>%
  group_by(Period, Region) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(region_label = Region, dominant_tool = Tool)

# ── 3. Join con world para cada período ──────────────────────
map_tool_evol <- world %>%
  cross_join(distinct(dominant_tool_evol, Period)) %>%
  left_join(dominant_tool_evol, by = c("region_label", "Period"))

tool_colors <- c(
  "Arena"            = "#D7191C",
  "AnyLogic"         = "#2C7BB6",
  "Custom code"      = "#1A9641",
  "Plant Simulation" = "#E67E22",
  "FlexSim"          = "#8E44AD"
)

# ── 4. Mapa faceteado por período ────────────────────────────
ggplot() +
  geom_sf(data = map_tool_evol, fill = "#D0D3D4",
          color = "white", linewidth = 0.2) +
  geom_sf(data = map_tool_evol %>% filter(!is.na(dominant_tool)),
          aes(fill = dominant_tool), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values       = tool_colors,
    name         = "Dominant tool",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  facet_wrap(~ Period, ncol = 2) +
  tema_mapa +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  ) +
  labs(
    title    = "Dominant simulation tool by region — evolution over time",
    subtitle = "Arena dominant 2015–2020 | AnyLogic rising from 2018 | Custom code stable in Europe"
  )

ggsave("outputs/SW_MAP_EVOL_tool_by_region_period_final.png",
       width = 14, height = 9, bg = "white")

# 1. Paradigma × objetivo del modelo
tabla %>%
  count(`Main paradigm`, `Model objective`, sort = TRUE) %>%
  print(n = 30)

# 2. Optimización × time horizon
tabla %>%
  count(`Integrates optimization?`, `Time horizon`, sort = TRUE) %>%
  print(n = 20)

# 3. Validated? × Real terminal?
tabla %>%
  count(`Validated?`, `Real terminal?`, sort = TRUE) %>%
  print(n = 20)

# 4. Economic classification
tabla %>%
  count(`Economic classification`, sort = TRUE)

# =============================================================
# CRUCE 1 — Heatmap: Main paradigm × Model objective
# =============================================================
heat_par_obj <- tabla %>%
  filter(!is.na(`Main paradigm`), !is.na(`Model objective`)) %>%
  count(`Main paradigm`, `Model objective`) %>%
  tidyr::complete(`Main paradigm`, `Model objective`, fill = list(n = 0))

heat_par_obj$`Model objective` <- recode(
  heat_par_obj$`Model objective`,
  "Sustainability + Performance evaluation" = "Sust. + Perf. eval.",
  "Sustainability + Capacity planning"      = "Sust. + Cap. plan.",
  "Education and training"                  = "Education / training",
  "Performance evaluation"                  = "Performance eval."
)

ggplot(heat_par_obj,
       aes(x = `Main paradigm`, y = `Model objective`, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n > 0, n, "–")), size = 4) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276", name = "Count") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(
    title    = "Heatmap: simulation paradigm × model objective",
    subtitle = "DES dominates all objectives | ABM strong in Multiple + Performance eval.",
    x = "", y = ""
  )

ggsave("outputs/CRUCE01_heatmap_paradigm_objective_final.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# CRUCE 2 — Optimización × Time horizon (% apilado)
# =============================================================
opt_th <- tabla %>%
  filter(!is.na(`Integrates optimization?`),
         !is.na(`Time horizon`),
         `Time horizon` != "Not specified") %>%
  count(`Time horizon`, `Integrates optimization?`) %>%
  group_by(`Time horizon`) %>%
  mutate(
    pct          = round(n / sum(n) * 100, 1),
    total        = sum(n),
    `Time horizon` = paste0(`Time horizon`, "\n(n=", total, ")")
  ) %>%
  ungroup()

ggplot(opt_th,
       aes(x = reorder(`Time horizon`, -total),
           y = pct, fill = `Integrates optimization?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct >= 8,
                               paste0(pct, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Yes" = "#2C7BB6", "No" = "#BDBDBD"),
    name   = "Integrates optimization?"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x      = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()) +
  labs(
    title    = "Optimization integration by time horizon",
    subtitle = "Operational models optimize most (50%) | Strategic rarely integrates optimization (18%)",
    x = "", y = "Percentage (%)"
  )

ggsave("outputs/CRUCE02_optimization_by_timehorizon_final.png",
       width = 10, height = 5, bg = "white")

# =============================================================
# CRUCE 3 — Validated? × Real terminal? (heatmap con %)
# =============================================================
val_real <- tabla %>%
  filter(!is.na(`Validated?`), !is.na(`Real terminal?`)) %>%
  count(`Real terminal?`, `Validated?`) %>%
  group_by(`Real terminal?`) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    `Validated?`     = factor(`Validated?`,
                              levels = c("Yes", "Partially",
                                         "No", "Not specified")),
    `Real terminal?` = factor(`Real terminal?`,
                              levels = c("Yes", "Partial", "No"))
  )

ggplot(val_real,
       aes(x = `Validated?`, y = `Real terminal?`, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")), size = 3.8) +
  scale_fill_gradient(low = "#EAFAF1", high = "#1E8449",
                      name = "% of row") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title    = "Validation status × real terminal basis",
    subtitle = "Models on real terminals validate more (Yes 48%) | Generic terminals rarely validate (No 67%)",
    x = "Validated?", y = "Real terminal?"
  )

ggsave("outputs/CRUCE03_validated_by_real_terminal_final.png",
       width = 8, height = 5, bg = "white")

# =============================================================
# CRUCE 4 — Economic classification × variables clave
# =============================================================
econ_vars <- tabla %>%
  filter(`Economic classification` != "Not specified",
         !is.na(`Economic classification`)) %>%
  group_by(`Economic classification`) %>%
  summarise(
    n_total      = n(),
    pct_opt      = round(mean(`Integrates optimization?` == "Yes") * 100, 1),
    pct_val      = round(mean(`Validated?` == "Yes") * 100, 1),
    pct_sust     = round(mean(`Considers sustainability?` == "Yes") * 100, 1),
    pct_sens     = round(mean(`Sensitivity analysis?` == "Yes") * 100, 1),
    pct_real     = round(mean(`Real terminal?` == "Yes") * 100, 1)
  ) %>%
  tidyr::pivot_longer(
    cols      = starts_with("pct_"),
    names_to  = "variable",
    values_to = "pct"
  ) %>%
  mutate(variable = recode(variable,
                           "pct_opt"  = "Optimization",
                           "pct_val"  = "Validated (Yes)",
                           "pct_sust" = "Sustainability",
                           "pct_sens" = "Sensitivity analysis",
                           "pct_real" = "Real terminal (Yes)"
  ))

ggplot(econ_vars,
       aes(x = variable, y = pct,
           fill = `Economic classification`,
           group = `Economic classification`)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "white", linewidth = 0.3, width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(
    values = c("Developed"  = "#2C7BB6",
               "Developing" = "#D7191C"),
    name = "Economic classification"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x      = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()) +
  labs(
    title    = "Developed vs developing countries — methodological comparison",
    subtitle = paste0("Developed n=44 | Developing n=36 | Not specified excluded (n=7)"),
    x = "", y = "Percentage (%)"
  )

ggsave("outputs/CRUCE04_developed_vs_developing_final.png",
       width = 10, height = 5, bg = "white")

# =============================================================
# MAPA 1 — Tool dominante por país (mapa coropleta)
# =============================================================
top_tools <- c("Arena", "AnyLogic", "Custom code", "Plant Simulation", "FlexSim")

tool_colors <- c(
  "Arena"            = "#D7191C",
  "AnyLogic"         = "#2C7BB6",
  "Custom code"      = "#1A9641",
  "Plant Simulation" = "#E67E22",
  "FlexSim"          = "#8E44AD",
  "Other"            = "#BDBDBD"
)

tool_dominant_country <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "",
         !is.na(Country), Country != "Not specified", Country != "") %>%
  mutate(
    Tool = case_when(
      grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
      grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE) ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      TRUE ~ "Other"
    ),
    Country = recode(Country,
                     "USA"       = "United States",
                     "Hong Kong" = "China")
  ) %>%
  group_by(Country) %>%
  summarise(
    n_tool        = n(),
    dominant_tool = Tool[which.max(tabulate(match(Tool, unique(Tool))))]
  ) %>%
  mutate(
    dominant_tool_plot = ifelse(dominant_tool %in% top_tools,
                                dominant_tool, "Other")
  )

tool_dominant_country$iso_a3 <- countrycode(
  tool_dominant_country$Country, "country.name", "iso3c", warn = FALSE)

map_tool_dominant <- world %>%
  left_join(tool_dominant_country, by = "iso_a3")

tool_colors_map <- c(tool_colors, "Other" = "#BDBDBD")

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map_tool_dominant %>% filter(!is.na(dominant_tool_plot)),
          aes(fill = dominant_tool_plot), color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = tool_colors_map,
                    name     = "Dominant tool",
                    na.value = "#D0D3D4") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  tema_mapa +
  labs(
    title    = "Dominant simulation tool by country",
    subtitle = "China → AnyLogic | Italy, UK, South Korea → Arena | Netherlands → Custom code\nGrey = no tool specified or no papers"
  )

ggsave("outputs/SW06_map_dominant_tool_country_final.png",
       width = 12, height = 7, bg = "white")

# =============================================================
# MAPA — Tool dominante por región (coropleta)
# =============================================================
tool_dominant_region <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "",
         !is.na(Region), Region != "Not specified") %>%
  mutate(
    Tool = case_when(
      grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
      grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE) ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Tool != "Other") %>%
  group_by(Region) %>%
  mutate(
    total_region  = n(),
    n_tied        = sum(Tool == names(which.max(table(Tool))))
  ) %>%
  count(Region, Tool, total_region, n_tied) %>%
  group_by(Region) %>%
  mutate(pct = round(n / total_region * 100, 1)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

# Calcular empates detallados
tool_dominant_region <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "",
         !is.na(Region), Region != "Not specified") %>%
  mutate(
    Tool = case_when(
      grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
      grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE) ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Tool != "Other") %>%
  count(Region, Tool) %>%
  group_by(Region) %>%
  mutate(
    total_region = sum(n),
    pct          = round(n / total_region * 100, 1),
    max_n        = max(n),
    tied_tools   = paste(Tool[n == max_n], collapse = " & ")
  ) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

# Subtítulo con empates especificados
tool_region_subtitle <- tool_dominant_region %>%
  arrange(desc(total_region)) %>%
  mutate(label = case_when(
    n == max_n & sapply(strsplit(tied_tools, " & "), length) > 1 ~
      paste0(Region, " → ", Tool,
             " (n=", n, "/", total_region, ", ", pct,
             "% — tied with ", gsub(paste0(Tool, " & |& ", Tool), "", tied_tools), ")"),
    TRUE ~
      paste0(Region, " → ", Tool,
             " (n=", n, "/", total_region, ", ", pct, "%)")
  )) %>%
  pull(label) %>%
  paste(collapse = " | ")

dominant_tool_region_join <- tool_dominant_region %>%
  rename(region_label = Region, dominant_tool = Tool)

map_tool_region <- world %>%
  left_join(dominant_tool_region_join, by = "region_label")

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map_tool_region %>% filter(!is.na(dominant_tool)),
          aes(fill = dominant_tool), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values       = tool_colors,
    name         = "Dominant tool",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  tema_mapa +
  labs(
    title    = "Dominant simulation tool by region",
    subtitle = stringr::str_wrap(tool_region_subtitle, width = 130)
  )

ggsave("outputs/SW_MAP_tool_dominant_by_region_final.png",
       width = 12, height = 7, bg = "white")

#FINAL FINAL
th_region <- tabla %>%
  mutate(
    Region = ifelse(is.na(Region), "Not specified", Region),
    `Time horizon` = ifelse(is.na(`Time horizon`), "Not specified", `Time horizon`)
  ) %>%
  filter(Region != "Not specified") %>%
  count(Region, `Time horizon`) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(th_region,
       aes(x = reorder(Region, -n, sum),
           y = n, fill = `Time horizon`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 2.8) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title    = "Time horizon by region (frequency)",
       subtitle = "Operational dominates Asia-Pacific; Strategic + Multiple dominate Europe",
       x = "", y = "Number of papers", fill = "Time horizon")

ggsave("outputs/Final/FinalFinal/G09_time_horizon_by_region_frequency.png",
       width = 12, height = 6, bg = "white")
## ANIMACION

anim_year <- tabla %>%
  count(Year, `Includes animation?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(anim_year,
       aes(x = factor(Year), y = n, fill = `Includes animation?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(data = subset(anim_year, `Includes animation?` == "Yes" & n > 0),
            aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#E67E22", "No" = "#BDBDBD")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Includes animation? — evolution over time",
       subtitle = paste0("Overall: ", sum(tabla$`Includes animation?` == "Yes"), " Yes"),
       x = "Year", y = "Number of papers", fill = "Includes animation?")

ggsave("outputs/Final/FinalFinal/G13_animation_by_year_frequency.png",
       width = 11, height = 6, bg = "white")
## POR PAIS

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>%
  mutate(
    continent_raw = countrycode(iso_a3, "iso3c", "continent"),
    region_label  = case_when(
      continent_raw == "Americas" &
        subregion %in% c("South America", "Central America",
                         "Caribbean")           ~ "Latin America",
      continent_raw == "Americas"               ~ "North America",
      continent_raw == "Asia" &
        name %in% c("Iran", "Iraq", "Saudi Arabia",
                    "United Arab Emirates", "Kuwait", "Qatar",
                    "Bahrain", "Oman", "Yemen", "Jordan",
                    "Lebanon", "Syria", "Israel", "Turkey") ~ "Middle East",
      continent_raw == "Asia"                   ~ "Asia-Pacific",
      continent_raw == "Oceania"                ~ "Asia-Pacific",
      continent_raw == "Europe"                 ~ "Europe",
      continent_raw == "Africa"                 ~ "Africa",
      TRUE ~ continent_raw
    )
  )
dominant_th_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "",
         !is.na(`Time horizon`),
         `Time horizon` != "Not specified") %>%
  mutate(Country = recode(Country,
                          "USA"       = "United States",
                          "Hong Kong" = "China")) %>%
  count(Country, `Time horizon`) %>%
  group_by(Country) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

dominant_th_country$iso_a3 <- countrycode(
  dominant_th_country$Country, "country.name", "iso3c", warn = FALSE)

map20_country <- world %>% left_join(dominant_th_country, by = "iso_a3")

th_colors_map <- c(
  "Operational"            = "#D7191C",   # rojo
  "Strategic"              = "#2C7BB6",   # azul
  "Multiple"               = "#8E44AD",   # morado
  "Operational + Tactical" = "#27AE60",   # verde
  "Strategic + Tactical"   = "#E67E22",   # naranja
  "Tactical"               = "#F1C40F"    # amarillo
)

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map20_country %>% filter(!is.na(`Time horizon`)),
          aes(fill = `Time horizon`), color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = th_colors_map,
                    name     = "Dominant time horizon",
                    na.value = "#EAECEE") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  labs(title    = "Dominant time horizon by country",
       subtitle = "Grey = countries with no papers or time horizon not specified")

ggsave("outputs/Final/FinalFinal/G20_map_time_horizon_by_country_final.png",
       width = 12, height = 7, bg = "white")

#Intensidad 

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(dplyr)
library(ggplot2)

# ── Geometría mundial ─────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

# ── Datos por país ────────────────────────────────────────────
th_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "",
         !is.na(`Time horizon`), `Time horizon` != "Not specified") %>%
  mutate(Country = recode(Country, "USA" = "United States", "Hong Kong" = "China")) %>%
  count(Country, `Time horizon`) %>%
  group_by(Country) %>%
  mutate(
    total        = sum(n),
    dominant_th  = `Time horizon`[which.max(n)]
  ) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

th_country$iso_a3 <- countrycode(th_country$Country, "country.name", "iso3c", warn = FALSE)

# ── Centroides ────────────────────────────────────────────────
centroids <- world %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3, lon, lat)

th_bubbles <- th_country %>%
  left_join(centroids, by = "iso_a3") %>%
  filter(!is.na(lon))

# ── Paleta ────────────────────────────────────────────────────
th_colors_map <- c(
  "Operational"            = "#D7191C",
  "Strategic"              = "#2C7BB6",
  "Multiple"               = "#8E44AD",
  "Operational + Tactical" = "#27AE60",
  "Strategic + Tactical"   = "#E67E22",
  "Tactical"               = "#F1C40F"
)

# ── Mapa ──────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = world, fill = "#E8E8E8", color = "white", linewidth = 0.2) +
  geom_point(data = th_bubbles,
             aes(x = lon, y = lat,
                 size  = total,
                 color = dominant_th),
             alpha = 0.85) +
  scale_size_continuous(
    range  = c(4, 16),
    name   = "Number of papers",
    breaks = c(1, 5, 10, 20)
  ) +
  scale_color_manual(
    values = th_colors_map,
    name   = "Dominant time horizon"
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size  = guide_legend(override.aes = list(color = "grey40"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.position = "bottom",
    legend.box      = "horizontal",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Dominant time horizon by country",
    subtitle = "Bubble size = total papers per country | Color = dominant time horizon"
  )

ggsave("outputs/Final/FinalFinal/G20_map_time_horizon_bubbles_by_country.png",
       width = 12, height = 7, bg = "white")

#Incluyendo los no dominantes

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(dplyr)
library(ggplot2)

# ── Geometría mundial ─────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

# ── Todos los pares país × time horizon ──────────────────────
th_all <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "",
         !is.na(`Time horizon`), `Time horizon` != "Not specified") %>%
  mutate(Country = recode(Country, "USA" = "United States", "Hong Kong" = "China")) %>%
  count(Country, `Time horizon`)

th_all$iso_a3 <- countrycode(th_all$Country, "country.name", "iso3c", warn = FALSE)

# ── Centroides ────────────────────────────────────────────────
centroids <- world %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3, lon, lat)

# ── Spread de burbujas alrededor del centroide ────────────────
set.seed(42)

th_bubbles_all <- th_all %>%
  left_join(centroids, by = "iso_a3") %>%
  filter(!is.na(lon)) %>%
  group_by(Country) %>%
  mutate(
    n_th  = n(),
    idx   = row_number(),
    angle = (idx - 1) * (2 * pi / n_th),
    radio = ifelse(n_th == 1, 0, 2.5),
    lon   = lon + radio * cos(angle),
    lat   = lat + radio * sin(angle)
  ) %>%
  ungroup()

# ── Paleta ────────────────────────────────────────────────────
th_colors_map <- c(
  "Operational"            = "#D7191C",
  "Strategic"              = "#2C7BB6",
  "Multiple"               = "#8E44AD",
  "Operational + Tactical" = "#27AE60",
  "Strategic + Tactical"   = "#E67E22",
  "Tactical"               = "#F1C40F"
)

# ── Mapa ──────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = world, fill = "#E8E8E8", color = "white", linewidth = 0.2) +
  geom_point(data = th_bubbles_all %>% arrange(n),
             aes(x = lon, y = lat,
                 size  = n,
                 color = `Time horizon`),
             alpha = 0.85) +
  scale_size_continuous(
    range  = c(3, 14),
    name   = "Number of papers",
    breaks = c(1, 3, 6, 11)
  ) +
  scale_color_manual(
    values = th_colors_map,
    name   = "Time horizon"
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size  = guide_legend(override.aes = list(color = "grey40"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.position = "bottom",
    legend.box      = "horizontal",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Time horizon distribution by country",
    subtitle = "Each bubble = one time horizon | Size = number of papers | Bubbles spread around country centroid"
  )

ggsave("outputs/Final/FinalFinal/G20_map_time_horizon_spread_bubbles.png",
       width = 13, height = 7, bg = "white")

#sustain burbujas 

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(dplyr)
library(ggplot2)

# ── Geometría mundial ─────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

# ── % sustainability Yes por país (todos los países) ──────────
sust_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "",
         !is.na(`Considers sustainability?`)) %>%
  mutate(Country = recode(Country, "USA" = "United States", "Hong Kong" = "China")) %>%
  group_by(Country) %>%
  summarise(
    n_total = n(),
    pct_yes = round(mean(`Considers sustainability?` == "Yes") * 100, 1)
  )

sust_country$iso_a3 <- countrycode(sust_country$Country, "country.name", "iso3c", warn = FALSE)

# ── Centroides ────────────────────────────────────────────────
centroids <- world %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3, lon, lat)

sust_bubbles <- sust_country %>%
  left_join(centroids, by = "iso_a3") %>%
  filter(!is.na(lon))

# ── Mapa ──────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = world, fill = "#E8E8E8", color = "white", linewidth = 0.2) +
  geom_point(data = sust_bubbles %>% arrange(n_total),
             aes(x = lon, y = lat,
                 size  = n_total,
                 color = pct_yes),
             alpha = 0.85) +
  scale_color_gradient(low  = "#D5F5E3", high = "#1E8449",
                       name = "% sustainability Yes",
                       limits = c(0, 100),
                       labels = function(x) paste0(x, "%")) +
  scale_size_continuous(
    range  = c(3, 14),
    name   = "Number of papers",
    breaks = c(1, 5, 10, 21)
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  guides(
    color = guide_colorbar(barwidth = 8, barheight = 0.8),
    size  = guide_legend(override.aes = list(color = "grey40"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Considers sustainability? — % Yes by country",
    subtitle = "Bubble size = total papers | Color intensity = % of papers considering sustainability\nSmall bubbles = fewer papers (interpret with caution)"
  )

ggsave("outputs/Final/FinalFinal/G21_map_sustainability_bubbles_by_country.png",
       width = 13, height = 7, bg = "white")



#blabla

sust_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Considers sustainability?`)) %>%
  count(Region, `Considers sustainability?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(sust_region,
       aes(x = reorder(Region, -total),
           y = n, fill = `Considers sustainability?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Yes" = "#1E8449", "No" = "#BDBDBD"),
    name   = "Considers sustainability?"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Considers sustainability? by region",
    subtitle = paste0("Total: ", nrow(tabla), " studies | Ordered by total papers per region"),
    x = "", y = "Number of papers"
  )

ggsave("outputs/Final/FinalFinal/G21_sustainability_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

##asdssad
anim_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Includes animation?`)) %>%
  count(Region, `Includes animation?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(anim_region,
       aes(x = reorder(Region, -total),
           y = n, fill = `Includes animation?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Yes" = "#E67E22", "No" = "#BDBDBD"),
    name   = "Includes animation?"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Includes animation? by region",
    subtitle = paste0("Total: ", sum(anim_region$n), " studies | Ordered by total papers per region"),
    x = "", y = "Number of papers"
  )

ggsave("outputs/Final/FinalFinal/G13_animation_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA A — Integrates optimization? por región
# =============================================================
opt_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Integrates optimization?`)) %>%
  count(Region, `Integrates optimization?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(opt_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Integrates optimization?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#B7950B", "No" = "#BDBDBD"),
                    name   = "Integrates optimization?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Integrates optimization? by region",
       subtitle = "Asia-Pacific 51% vs Europe 18% — notable methodological gap",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_A_optimization_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA B — Sensitivity analysis? por región
# =============================================================
sens_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Sensitivity analysis?`)) %>%
  count(Region, `Sensitivity analysis?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(sens_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Sensitivity analysis?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#1A5276", "No" = "#BDBDBD"),
                    name   = "Sensitivity analysis?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Sensitivity analysis? by region",
       subtitle = "Germany 67% | Netherlands, UK → 0%",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_B_sensitivity_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA C — Validated? por región (Yes / Partially / No)
# =============================================================
val_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Validated?`)) %>%
  count(Region, `Validated?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(`Validated?` = factor(`Validated?`,
                               levels = c("Yes", "Partially", "No", "Not specified")))

ggplot(val_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Validated?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes"           = "#1E8449",
                               "Partially"     = "#ABD9E9",
                               "No"            = "#D7191C",
                               "Not specified" = "#BDBDBD"),
                    name   = "Validated?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Model validation by region",
       subtitle = "France 50% | Germany 67% | Netherlands 0%",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_C_validation_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA D — Includes animation? por región
# =============================================================
anim_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Includes animation?`)) %>%
  count(Region, `Includes animation?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(anim_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Includes animation?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#E67E22", "No" = "#BDBDBD"),
                    name   = "Includes animation?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Includes animation? by region",
       subtitle = "Italy 50% | Turkey 50% | Sweden 33% | UK, Netherlands, Germany → 0%",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_D_animation_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA E — Considers sustainability? por región
# =============================================================
sust_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Considers sustainability?`)) %>%
  count(Region, `Considers sustainability?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(sust_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Considers sustainability?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes" = "#1E8449", "No" = "#BDBDBD"),
                    name   = "Considers sustainability?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Considers sustainability? by region",
       subtitle = "Indonesia 80% | Netherlands 62% | France, India, Sweden, Turkey → 0%",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_E_sustainability_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA H — Real terminal? por región (Yes / Partial / No)
# =============================================================
real_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Real terminal?`)) %>%
  count(Region, `Real terminal?`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(`Real terminal?` = factor(`Real terminal?`,
                                   levels = c("Yes", "Partial", "No")))

ggplot(real_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Real terminal?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes"     = "#1A5276",
                               "Partial" = "#ABD9E9",
                               "No"      = "#D7191C"),
                    name   = "Real terminal?") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Real terminal basis by region",
       subtitle = "France, Malaysia, Poland, Sweden → 100% | Indonesia, South Korea → lower real terminal use",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_H_real_terminal_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

# =============================================================
# MAPA K — Model objective por región (dominante → barras)
# =============================================================
obj_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Model objective`)) %>%
  count(Region, `Model objective`) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

obj_colors <- c(
  "Multiple"                                = "#8E44AD",
  "Optimization"                            = "#B7950B",
  "Performance evaluation"                  = "#2C7BB6",
  "Sustainability"                          = "#1A9641",
  "Capacity planning"                       = "#D7191C",
  "Sustainability + Performance evaluation" = "#1F618D",
  "Education and training"                  = "#E67E22",
  "Scenario analysis"                       = "#95A5A6",
  "Sustainability + Capacity planning"      = "#148F77"
)

ggplot(obj_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = `Model objective`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = obj_colors,
                    name   = "Model objective") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Model objective by region",
       subtitle = "Asia-Pacific → Optimization | Europe → Multiple | Latin America → Performance eval.",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/MAPA_K_model_objective_by_region_stacked.png",
       width = 10, height = 6, bg = "white")
#toools
tool_region_bar <- tabla %>%
  filter(!is.na(Region), Region != "Not specified",
         !is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "") %>%
  mutate(Tool = case_when(
    grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
    grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
    grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
          `Tool / Software`, ignore.case = TRUE)                     ~ "Custom code",
    grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
    grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
    TRUE ~ "Other"
  )) %>%
  count(Region, Tool) %>%
  group_by(Region) %>%
  mutate(total = sum(n)) %>%
  ungroup()

tool_colors <- c(
  "Arena"            = "#D7191C",
  "AnyLogic"         = "#2C7BB6",
  "Custom code"      = "#1A9641",
  "Plant Simulation" = "#E67E22",
  "FlexSim"          = "#8E44AD",
  "Other"            = "#95A5A6"
)

ggplot(tool_region_bar,
       aes(x = reorder(Region, -total),
           y = n, fill = Tool)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = tool_colors, name = "Tool / Software") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey40")) +
  labs(title    = "Simulation tool by region",
       subtitle = "Only papers with tool specified | North America excluded: 1 paper with tool not specified",
       x = "", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/SW_tool_by_region_stacked.png",
       width = 10, height = 6, bg = "white")

#aaaa
econ_vars <- tabla %>%
  filter(`Economic classification` != "Not specified",
         !is.na(`Economic classification`)) %>%
  group_by(`Economic classification`) %>%
  summarise(
    n_total      = n(),
    Optimization        = sum(`Integrates optimization?` == "Yes"),
    `Real terminal (Yes)` = sum(`Real terminal?` == "Yes"),
    `Sensitivity analysis` = sum(`Sensitivity analysis?` == "Yes"),
    Sustainability      = sum(`Considers sustainability?` == "Yes"),
    `Validated (Yes)`   = sum(`Validated?` == "Yes")
  ) %>%
  tidyr::pivot_longer(
    cols      = c(Optimization, `Real terminal (Yes)`,
                  `Sensitivity analysis`, Sustainability, `Validated (Yes)`),
    names_to  = "variable",
    values_to = "n"
  )

ggplot(econ_vars,
       aes(x = variable, y = n,
           fill = `Economic classification`,
           group = `Economic classification`)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "white", linewidth = 0.3, width = 0.7) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(
    values = c("Developed"  = "#2C7BB6",
               "Developing" = "#D7191C"),
    name = "Economic classification"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey40")) +
  labs(
    title    = "Developed vs developing countries — methodological comparison",
    subtitle = paste0("Developed n=", sum(tabla$`Economic classification` == "Developed", na.rm = TRUE),
                      " | Developing n=", sum(tabla$`Economic classification` == "Developing", na.rm = TRUE),
                      " | Not specified excluded (n=",
                      sum(tabla$`Economic classification` == "Not specified", na.rm = TRUE), ")"),
    x = "", y = "Number of papers"
  )

ggsave("outputs/Final/FinalFinal/CRUCE04_developed_vs_developing_n.png",
       width = 10, height = 5, bg = "white")

#aaaaaaa
opt_th <- tabla %>%
  filter(!is.na(`Integrates optimization?`),
         !is.na(`Time horizon`),
         `Time horizon` != "Not specified") %>%
  count(`Time horizon`, `Integrates optimization?`) %>%
  group_by(`Time horizon`) %>%
  mutate(
    total        = sum(n),
    `Time horizon` = paste0(`Time horizon`, "\n(n=", total, ")")
  ) %>%
  ungroup()

ggplot(opt_th,
       aes(x = reorder(`Time horizon`, -total),
           y = n, fill = `Integrates optimization?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Yes" = "#2C7BB6", "No" = "#BDBDBD"),
    name   = "Integrates optimization?"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey40")) +
  labs(
    title    = "Optimization integration by time horizon",
    subtitle = "Operational models optimize most | Strategic rarely integrates optimization",
    x = "", y = "Number of papers"
  )

ggsave("outputs/Final/FinalFinal/CRUCE02_optimization_by_timehorizon_n.png",
       width = 10, height = 5, bg = "white")

#evolution

top_tools <- c("Arena", "AnyLogic", "Custom code", "Plant Simulation", "FlexSim")

tool_colors <- c(
  "Arena"            = "#D7191C",
  "AnyLogic"         = "#2C7BB6",
  "Custom code"      = "#1A9641",
  "Plant Simulation" = "#E67E22",
  "FlexSim"          = "#8E44AD"
)

tabla_tool_evol_country <- tabla %>%
  filter(!is.na(`Tool / Software`),
         `Tool / Software` != "Not specified",
         `Tool / Software` != "",
         !is.na(Country), Country != "Not specified", Country != "") %>%
  mutate(
    Tool = case_when(
      grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
      grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE)                     ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      TRUE ~ "Other"
    ),
    Period = case_when(
      Year <= 2017 ~ "2015–2017",
      Year <= 2020 ~ "2018–2020",
      Year <= 2022 ~ "2021–2022",
      TRUE         ~ "2023–2025"
    ),
    Period  = factor(Period, levels = c("2015–2017", "2018–2020", "2021–2022", "2023–2025")),
    Country = recode(Country, "USA" = "United States", "Hong Kong" = "China")
  ) %>%
  filter(Tool != "Other") %>%
  count(Period, Country, Tool) %>%
  group_by(Period, Country) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(dominant_tool = Tool)

tabla_tool_evol_country$iso_a3 <- countrycode(
  tabla_tool_evol_country$Country, "country.name", "iso3c", warn = FALSE)

map_tool_evol_country <- world %>%
  cross_join(distinct(tabla_tool_evol_country, Period)) %>%
  left_join(tabla_tool_evol_country, by = c("iso_a3", "Period"))

ggplot() +
  geom_sf(data = map_tool_evol_country, fill = "#D0D3D4",
          color = "white", linewidth = 0.2) +
  geom_sf(data = map_tool_evol_country %>% filter(!is.na(dominant_tool)),
          aes(fill = dominant_tool), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values       = tool_colors,
    name         = "Dominant tool",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  facet_wrap(~ Period, ncol = 2) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Dominant simulation tool by country — evolution over time",
    subtitle = "Arena dominant 2015–2020 | AnyLogic rising from 2018 | Custom code stable in Europe\nGrey = no tool specified or no papers"
  )

ggsave("outputs/Final/FinalFinal/SW_MAP_EVOL_tool_by_country_period_final.png",
       width = 14, height = 9, bg = "white")
## 

papers_country <- tabla %>%
  filter(!is.na(Country), Country != "Not specified", Country != "") %>%
  mutate(Country = recode(Country, "USA" = "United States", "Hong Kong" = "China")) %>%
  count(Country, sort = TRUE)

papers_country$iso_a3 <- countrycode(papers_country$Country,
                                     "country.name", "iso3c", warn = FALSE)

map_country <- world %>% left_join(papers_country, by = "iso_a3")

# Subtítulo automático con top países
top_subtitle <- papers_country %>%
  slice_head(n = 5) %>%
  mutate(label = paste0(Country, " = ", n)) %>%
  pull(label) %>%
  paste(collapse = " | ")

ggplot() +
  geom_sf(data = world, fill = "#D0D3D4", color = "white", linewidth = 0.2) +
  geom_sf(data = map_country,
          aes(fill = n), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low  = "#D6EAF8", high = "#1A5276",
                      name = "Papers",
                      na.value = "#D0D3D4") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40")
  ) +
  labs(
    title    = "Distribution of papers by country",
    subtitle = top_subtitle
  )

ggsave("outputs/Final/FinalFinal/G18_map_papers_by_country.png",
       width = 12, height = 7, bg = "white")

#

val_year <- tabla %>%
  count(Year, `Validated?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

val_colors <- c("Yes"           = "#1A9641",
                "Partially"     = "#ABD9E9",
                "No"            = "#D7191C",
                "Not specified" = "#BDBDBD")

ggplot(val_year,
       aes(x = factor(Year), y = n, fill = `Validated?`)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "grey20") +
  scale_fill_manual(values = val_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")) +
  labs(title    = "Validation status over time",
       subtitle = paste0("Overall: ",
                         sum(tabla$`Validated?` == "Yes"), " Yes / ",
                         sum(tabla$`Validated?` == "Partially"), " Partially / ",
                         sum(tabla$`Validated?` == "No"), " No"),
       x = "Year", y = "Number of papers", fill = "Validated?")

ggsave("outputs/Final/FinalFinal/G11_validated_by_year_n.png",
       width = 11, height = 6, bg = "white")

#sdadsa
real_term <- tabla %>%
  count(`Real terminal?`) %>%
  mutate(
    pct   = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", pct, "%)"),
    nota  = case_when(
      `Real terminal?` == "Yes"     ~ "Based on a specific\nreal terminal",
      `Real terminal?` == "Partial" ~ "Uses real terminal data\nbut with simplifications",
      `Real terminal?` == "No"      ~ "Generic / hypothetical\nterminal"
    )
  ) %>%
  mutate(`Real terminal?` = factor(`Real terminal?`,
                                   levels = c("Yes", "Partial", "No")))

ggplot(real_term, aes(y = fct_rev(`Real terminal?`),
                      x = n, fill = `Real terminal?`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), hjust = -0.3, size = 4, color = "grey25", fontface = "bold") +
  geom_text(aes(label = nota, x = 0.5), hjust = 0,
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Yes"     = "#2C7BB6",
                               "Partial" = "#ABD9E9",
                               "No"      = "#D7191C")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        panel.grid.major.y = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey40")) +
  labs(title    = "Real terminal basis",
       subtitle = '"Partial" = real data used but operations simplified or generalized',
       x = "Count", y = "")

ggsave("outputs/Final/FinalFinal/G03_real_terminal_n.png",
       width = 9, height = 4, bg = "white")

###
tabla_val <- tabla %>%
filter(`Validated?` == "Yes",
       !is.na(`Validation method`),
       `Validation method` != "Not specified") %>%
  mutate(val_raw = strsplit(as.character(`Validation method`), ",\\s*")) %>%
  tidyr::unnest(val_raw) %>%
  mutate(val_raw = trimws(val_raw)) %>%
  mutate(val_grouped = case_when(
    grepl("historical data|real data|empirical data|observed data|real system",
          val_raw, ignore.case = TRUE)                        ~ "Historical / real data",
    grepl("statistical test|statistical anal|robustness|replications",
          val_raw, ignore.case = TRUE)                        ~ "Statistical tests",
    grepl("expert judgment|expert judge|operations team|operators",
          val_raw, ignore.case = TRUE)                        ~ "Expert judgment",
    grepl("literature comparison",
          val_raw, ignore.case = TRUE)                        ~ "Literature comparison",
    grepl("model comparison|comparison with simulation|mathematical comparison",
          val_raw, ignore.case = TRUE)                        ~ "Model / simulation comparison",
    grepl("calibration",
          val_raw, ignore.case = TRUE)                        ~ "Calibration",
    grepl("animation",
          val_raw, ignore.case = TRUE)                        ~ "Animation",
    grepl("theoretical formulas",
          val_raw, ignore.case = TRUE)                        ~ "Theoretical formulas",
    TRUE ~ "Other"
  )) %>%
  filter(val_grouped != "Other")

val_year <- tabla_val %>%
  count(Year, val_grouped)

val_colors <- c(
  "Historical / real data"        = "#8E44AD",
  "Statistical tests"             = "#F39C12",
  "Expert judgment"               = "#E67E22",
  "Literature comparison"         = "#E91E8C",
  "Model / simulation comparison" = "#27AE60",
  "Calibration"                   = "#16A085",
  "Animation"                     = "#2980B9",
  "Theoretical formulas"          = "#795548"
)

ggplot(val_year,
       aes(x = factor(Year), y = n, fill = val_grouped)) +
  geom_bar(stat = "identity", position = "stack",
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_manual(values = val_colors, name = "Validation method") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 10, color = "grey40")) +
  labs(title    = "Validation methods over time",
       subtitle = "Historical / real data consistently dominant; statistical tests emerging post-2018",
       x = "Year", y = "Number of papers")

ggsave("outputs/Final/FinalFinal/VAL02_validation_methods_by_year_n.png",
       width = 12, height = 6, bg = "white")
#sadisakdsa
install.packages(c("igraph", "ggraph", "tidygraph"))

library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(tidyr)

# ── Preparar datos: cada paper como conjunto de atributos ─────
tabla_net <- tabla %>%
  mutate(
    Tool_clean = case_when(
      grepl("Arena",            `Tool / Software`, ignore.case = TRUE) ~ "Arena",
      grepl("AnyLogic",         `Tool / Software`, ignore.case = TRUE) ~ "AnyLogic",
      grepl("Custom code|Java|Python|PSIGHOS|SmartSim",
            `Tool / Software`, ignore.case = TRUE)                     ~ "Custom code",
      grepl("Plant Simulation", `Tool / Software`, ignore.case = TRUE) ~ "Plant Simulation",
      grepl("FlexSim|FlexTerm", `Tool / Software`, ignore.case = TRUE) ~ "FlexSim",
      `Tool / Software` == "Not specified" | is.na(`Tool / Software`) ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  select(ID,
         Paradigm    = `Main paradigm`,
         Tool        = Tool_clean,
         TimeHorizon = `Time horizon`,
         Objective   = `Model objective`,
         Subprocess  = `Specific subprocess`) %>%
  mutate(across(everything(), ~ na_if(as.character(.), "Not specified")))

# ── Explotar subprocesos múltiples ────────────────────────────
tabla_sub <- tabla_net %>%
  mutate(Subprocess = strsplit(as.character(Subprocess), ",\\s*")) %>%
  unnest(Subprocess) %>%
  mutate(Subprocess = trimws(Subprocess)) %>%
  mutate(Subprocess = case_when(
    grepl("AGV",                                        Subprocess, ignore.case = TRUE) ~ "AGV routing",
    grepl("quay crane|crane assign|crane sched",        Subprocess, ignore.case = TRUE) ~ "Quay crane scheduling",
    grepl("berth",                                      Subprocess, ignore.case = TRUE) ~ "Berth allocation",
    grepl("traffic|congestion",                         Subprocess, ignore.case = TRUE) ~ "Traffic management",
    grepl("gate",                                       Subprocess, ignore.case = TRUE) ~ "Gate operations",
    grepl("truck dispatch|yard truck",                  Subprocess, ignore.case = TRUE) ~ "Truck dispatching",
    grepl("stacking|retrieval|storage",                 Subprocess, ignore.case = TRUE) ~ "Stacking / storage",
    grepl("yard plan|yard layout|yard oper",            Subprocess, ignore.case = TRUE) ~ "Yard planning",
    grepl("layout|design",                              Subprocess, ignore.case = TRUE) ~ "Layout / design",
    grepl("Not specified|^$|NA",                        Subprocess, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ Subprocess
  )) %>%
  filter(!is.na(Subprocess))

# ── Construir pares de co-ocurrencia por paper ────────────────
make_pairs <- function(df, col1, col2, id_col = "ID") {
  df %>%
    select(all_of(c(id_col, col1, col2))) %>%
    filter(!is.na(.[[col1]]), !is.na(.[[col2]]),
           .[[col1]] != .[[col2]]) %>%
    group_by(across(all_of(c(id_col)))) %>%
    summarise(from = .[[col1]][1], to = .[[col2]][1], .groups = "drop") %>%
    count(from, to) %>%
    filter(from != to)
}

# Combinar todas las variables en formato largo por paper
tabla_long <- tabla_sub %>%
  pivot_longer(cols = c(Paradigm, Tool, TimeHorizon, Objective, Subprocess),
               names_to = "type", values_to = "node") %>%
  filter(!is.na(node), node != "NA", node != "Other")

# Generar pares de co-ocurrencia
edges <- tabla_long %>%
  group_by(ID) %>%
  filter(n() >= 2) %>%
  summarise(pairs = list(combn(node, 2, simplify = FALSE)), .groups = "drop") %>%
  unnest(pairs) %>%
  mutate(from = sapply(pairs, `[[`, 1),
         to   = sapply(pairs, `[[`, 2)) %>%
  select(from, to) %>%
  filter(from != to) %>%
  count(from, to, name = "weight") %>%
  filter(weight >= 2)  # solo conexiones con al menos 2 co-ocurrencias

# ── Nodos con frecuencia y tipo ───────────────────────────────
node_freq <- tabla_long %>%
  count(node, type, name = "freq") %>%
  group_by(node) %>%
  slice_max(freq, n = 1, with_ties = FALSE) %>%
  ungroup()

# ── Construir grafo ───────────────────────────────────────────
g <- graph_from_data_frame(edges, directed = FALSE,
                           vertices = node_freq %>% filter(node %in% c(edges$from, edges$to)))

g <- as_tbl_graph(g) %>%
  mutate(degree     = centrality_degree(weights = weight),
         community  = as.factor(group_louvain()))

# ── Colores por tipo de variable ─────────────────────────────
type_colors <- c(
  "Paradigm"    = "#2C7BB6",
  "Tool"        = "#D7191C",
  "TimeHorizon" = "#8E44AD",
  "Objective"   = "#E67E22",
  "Subprocess"  = "#1A9641"
)

# ── Grafo ─────────────────────────────────────────────────────
set.seed(42)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight),
                 color = "grey70") +
  geom_node_point(aes(size = freq, color = type)) +
  geom_node_text(aes(label = name, size = freq),
                 repel = TRUE, max.overlaps = 30,
                 fontface = "bold", color = "grey20") +
  scale_edge_width(range = c(0.3, 3), guide = "none") +
  scale_edge_alpha(range = c(0.2, 0.8), guide = "none") +
  scale_size_continuous(range = c(3, 14), name = "Frequency") +
  scale_color_manual(values = type_colors, name = "Variable type") +
  theme_graph(base_size = 12) +
  theme(legend.position = "right",
        plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(size = 10, color = "grey40")) +
  labs(title    = "Co-occurrence network of simulation study attributes",
       subtitle = "Node size = frequency | Edge width = co-occurrence strength | n ≥ 2 connections shown")

ggsave("outputs/Final/FinalFinal/NET_cooccurrence_network.png",
       width = 14, height = 10, bg = "white")

# =============================================================
# MAPA DE CO-OCURRENCIA DE TÉRMINOS — estilo VOSviewer
# Systematic Literature Review — Port Terminal Simulations
# Basado en abstracts de la hoja "Extracted Data + Decision"
# =============================================================

# ── Paquetes ──────────────────────────────────────────────────
# Instala los que falten con:
# install.packages(c("readxl","tidytext","tidyr","dplyr","ggplot2",
#                    "widyr","igraph","ggraph","tidygraph","MASS",
#                    "scales","ggrepel","stringr","forcats"))
install.packages("widyr")
library(readxl)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(widyr)       # pairwise_count
library(igraph)
library(ggraph)
library(tidygraph)
library(MASS)        # kde2d para la densidad
library(scales)
library(ggrepel)
library(stringr)
library(forcats)

# =============================================================
# PARÁMETROS AJUSTABLES
# =============================================================
RUTA_XLSX    <- "data/data.xlsx"      # <- cambia si tu archivo está en otro lugar
HOJA         <- "Extracted Data + Decision"
SKIP_ROWS    <- 2                     # filas de encabezado a saltar

MIN_TERM_FREQ  <- 8    # frecuencia mínima para que un término aparezca en el grafo
MIN_COOC       # se define abajo como función del corpus
N_TOP_LABELS   <- 40   # cuántos nodos reciben etiqueta de texto

PALETTE_CLUSTERS <- c(
  "1" = "#C0392B",   # rojo
  "2" = "#2980B9",   # azul
  "3" = "#27AE60",   # verde
  "4" = "#E67E22",   # naranja
  "5" = "#8E44AD",   # morado
  "6" = "#16A085",   # verde agua
  "7" = "#F39C12",   # amarillo
  "8" = "#7F8C8D"    # gris
)

# =============================================================
# 1. CARGA DE DATOS
# =============================================================
tabla_raw <- read_excel(RUTA_XLSX, sheet = HOJA, skip = SKIP_ROWS)

# Filtrar solo estudios incluidos y excluir Mathematical Analytical
tabla <- tabla_raw %>%
  filter(`Phase 2 Decision` == "Include",
         `Main paradigm`    != "Mathematical Analytical") %>%
  mutate(doc_id = row_number())

cat("✓ Papers incluidos:", nrow(tabla), "\n")

# Usar Abstract como fuente principal de texto.
# Si quieres sumar también el título, descomenta la línea alternativa.
tabla_text <- tabla %>%
  select(doc_id, text = Abstract)
# select(doc_id, text = Title)    # ← alternativa: solo títulos
# mutate(text = paste(Title, Abstract))  # ← alternativa: título + abstract

# =============================================================
# 2. TOKENIZACIÓN Y LIMPIEZA
# =============================================================

# Stopwords base en inglés + dominio portuario genérico
extra_stopwords <- tibble(word = c(
  "paper", "study", "model", "models", "simulation", "simulations",
  "based", "proposed", "results", "result", "show", "shows", "shown",
  "using", "used", "use", "also", "can", "due", "order", "set",
  "two", "three", "one", "time", "number", "different", "new",
  "approach", "method", "system", "systems", "process", "processes",
  "data", "case", "al", "et", "include", "including",
  "however", "thus", "furthermore", "therefore", "presented",
  "analyze", "analysis", "evaluated", "developed", "obtained",
  "applied", "consider", "considered", "provide", "provides",
  "conducted", "found", "make", "made", "may", "well", "various",
  "several", "many", "large", "high", "low", "specific",
  "overall", "total", "each", "per", "de", "la", "el",
  "2021","2022","2023","2024","2025","2020","2019","2018"
))

tokens <- tabla_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-z][a-z-]+$")) %>%   # solo letras (y guiones)
  anti_join(stop_words, by = "word") %>%
  anti_join(extra_stopwords, by = "word") %>%
  filter(nchar(word) >= 3)

# Frecuencia global de términos
term_freq <- tokens %>%
  count(word, sort = TRUE)

cat("✓ Vocabulario total tras limpieza:", nrow(term_freq), "términos\n")
cat("Top 20 términos:\n")
print(head(term_freq, 20))

# =============================================================
# 3. CO-OCURRENCIA POR DOCUMENTO
# =============================================================
MIN_COOC <- max(3, floor(nrow(tabla) * 0.04))   # ~4% de los papers
cat("✓ Umbral de co-ocurrencia:", MIN_COOC, "\n")

# Mantener solo términos suficientemente frecuentes
top_terms <- term_freq %>%
  filter(n >= MIN_TERM_FREQ) %>%
  pull(word)

tokens_filtered <- tokens %>%
  filter(word %in% top_terms)

# Pares de co-ocurrencia dentro del mismo documento
cooc <- tokens_filtered %>%
  pairwise_count(word, doc_id, sort = TRUE, upper = FALSE) %>%
  filter(n >= MIN_COOC)

cat("✓ Pares de co-ocurrencia (n >=", MIN_COOC, "):", nrow(cooc), "\n")

# =============================================================
# 4. GRAFO + DETECCIÓN DE COMUNIDADES
# =============================================================
g <- graph_from_data_frame(
  cooc %>% rename(from = item1, to = item2, weight = n),
  directed = FALSE,
  vertices  = term_freq %>% filter(word %in% unique(c(cooc$item1, cooc$item2)))
)

# Comunidades de Louvain (equivalente a VOSviewer clusters)
set.seed(42)
comm <- cluster_louvain(g, weights = E(g)$weight)
V(g)$cluster <- as.character(membership(comm))
V(g)$freq    <- V(g)$n        # columna n viene del data.frame de vértices

tg <- as_tbl_graph(g)

# Layout Fruchterman-Reingold ponderado
set.seed(42)
layout_fr <- create_layout(tg, layout = "fr",
                           weights = E(tg)$weight)

# =============================================================
# 5. MAPA DE DENSIDAD (imitando VOSviewer)
# =============================================================

# Coordenadas de nodos
xy <- layout_fr %>% select(x, y, freq)

# KDE suavizado
kde <- kde2d(xy$x, xy$y,
             h  = c(bandwidth.nrd(xy$x) * 2,
                    bandwidth.nrd(xy$y) * 2),
             n  = 200,
             lims = c(range(xy$x) + c(-1, 1),
                      range(xy$y) + c(-1, 1)))

kde_df <- expand.grid(x = kde$x, y = kde$y) %>%
  mutate(density = as.vector(kde$z))

# Nodos con etiqueta (los N_TOP_LABELS más frecuentes)
top_nodes <- layout_fr %>%
  arrange(desc(freq)) %>%
  slice_head(n = N_TOP_LABELS)

# =============================================================
# 6. VISUALIZACIÓN FINAL
# =============================================================
ggplot() +
  
  # Capa de densidad (fondo oscuro con halos de color)
  geom_raster(data = kde_df,
              aes(x = x, y = y, fill = density),
              interpolate = TRUE) +
  scale_fill_gradientn(
    colours = c("black", "#1a0a2e", "#2d0a4e",
                "#8B0000", "#CC2200", "#FF4500",
                "#FF8C00", "#FFD700", "#FFFACD"),
    values  = scales::rescale(c(0, 0.05, 0.12, 0.22,
                                0.35, 0.50, 0.65, 0.82, 1)),
    guide   = "none"
  ) +
  
  # Aristas (muy tenues sobre el fondo oscuro)
  geom_edge_link(data = layout_fr,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     alpha = weight, width = weight),
                 color = "white", show.legend = FALSE) +
  scale_edge_alpha(range = c(0.02, 0.12), guide = "none") +
  scale_edge_width(range = c(0.1, 0.8),  guide = "none") +
  
  # Nodos coloreados por clúster
  geom_point(data = layout_fr,
             aes(x = x, y = y, size = freq,
                 color = cluster),
             alpha = 0.85, show.legend = FALSE) +
  scale_size_continuous(range = c(1.5, 10), guide = "none") +
  scale_color_manual(values = PALETTE_CLUSTERS,
                     na.value = "grey70") +
  
  # Etiquetas de texto blancas
  geom_text_repel(data = top_nodes,
                  aes(x = x, y = y, label = word,
                      size = freq),
                  color        = "white",
                  bg.color     = NA,
                  fontface     = "bold",
                  max.overlaps = 40,
                  segment.color = "white",
                  segment.alpha = 0.3,
                  box.padding  = 0.25,
                  point.padding = 0.1,
                  show.legend  = FALSE) +
  scale_size_continuous(range = c(2.5, 6)) +
  
  # Tema oscuro imitando VOSviewer
  coord_fixed() +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.title       = element_text(color = "white", face = "bold",
                                    size = 15, hjust = 0.5),
    plot.subtitle    = element_text(color = "grey70", size = 9,
                                    hjust = 0.5),
    plot.caption     = element_text(color = "grey50", size = 7,
                                    hjust = 1),
    plot.margin      = margin(12, 12, 12, 12)
  ) +
  labs(
    title    = "Term Co-occurrence Density Map",
    subtitle = paste0("n = ", nrow(tabla),
                      " studies | min. term frequency = ", MIN_TERM_FREQ,
                      " | min. co-occurrence = ", MIN_COOC),
    caption  = "Node color = Louvain community | Node size = term frequency | Halo intensity = local term density"
  )

# =============================================================
# 7. GUARDAR
# =============================================================
ggsave("outputs/TERM_COOC_density_map.png",
       width = 13, height = 10, dpi = 300, bg = "black")

cat("✓ Gráfico guardado en outputs/TERM_COOC_density_map.png\n")

# =============================================================
# OPCIONAL: tabla de términos por clúster
# =============================================================
cluster_tbl <- tibble(
  word    = V(g)$name,
  freq    = V(g)$freq,
  cluster = V(g)$cluster
) %>%
  arrange(cluster, desc(freq))

cat("\n── Top 5 términos por clúster ──\n")
cluster_tbl %>%
  group_by(cluster) %>%
  slice_head(n = 5) %>%
  print(n = Inf)

install.packages(c("tidytext", "widyr", "igraph", "ggraph", "tidygraph", "MASS", "ggrepel"))

# =============================================================
# TERM CO-OCCURRENCE NETWORK — estilo VOSviewer
# Systematic Literature Review — Simulación en terminales portuarios
# Fuente: columna Abstract de la hoja "Data" (n = 87 incluidos)
# =============================================================

# ── Paquetes ──────────────────────────────────────────────────
# install.packages(c("readxl","tidytext","tidyr","dplyr","ggplot2",
#                    "widyr","igraph","ggraph","tidygraph","MASS",
#                    "scales","ggrepel","stringr","forcats"))

library(MASS)        # kde2d — cargar ANTES de dplyr para evitar conflicto con select()
library(readxl)
library(tidytext)
library(tidyr)
library(dplyr)       # dplyr::select queda activo al cargarse después de MASS
library(stringr)
library(forcats)
library(ggplot2)
library(widyr)       # pairwise_count
library(igraph)
library(ggraph)
library(tidygraph)
library(scales)
library(ggrepel)

# =============================================================
# PARÁMETROS
# =============================================================
RUTA_XLSX     <- "data/data.xlsx"   # ajusta si es necesario
MIN_TERM_FREQ <- 6    # frecuencia mínima de un término en los docs
MIN_COOC      <- 4    # co-ocurrencias mínimas entre un par
N_TOP_LABELS  <- 55   # nodos con etiqueta de texto visible

PALETTE <- c(
  "1" = "#E53935",   # rojo       — operaciones de patio / camiones
  "2" = "#1E88E5",   # azul       — grúas / optimización
  "3" = "#43A047",   # verde      — automatización / decisiones
  "4" = "#FB8C00",   # naranja    — equipos / evaluación
  "5" = "#8E24AA",   # morado     — diseño / throughput
  "6" = "#00ACC1",   # cyan
  "7" = "#F4511E",   # naranja oscuro
  "8" = "#546E7A"    # gris azul
)

# =============================================================
# 1. CARGA Y FILTRADO
# =============================================================
tabla_ext <- read_excel(RUTA_XLSX,
                        sheet = "Extracted Data + Decision",
                        skip  = 2)
tabla_ext <- subset(tabla_ext, `Phase 2 Decision` == "Include")
tabla_ext <- subset(tabla_ext, `Main paradigm`    != "Mathematical Analytical")
ids_incl  <- unique(as.character(tabla_ext$ID))

tabla_data <- read_excel(RUTA_XLSX, sheet = "Data")
tabla_data$ID <- as.character(tabla_data$ID)
tabla_abs  <- tabla_data %>%
  filter(ID %in% ids_incl, !is.na(Abstract)) %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, Abstract)

cat("✓ Papers con abstract:", nrow(tabla_abs), "\n")

# =============================================================
# 2. STOPWORDS — inglés + ruido editorial + dominio genérico
# =============================================================
custom_stop <- tibble(word = c(
  # ruido editorial / metadata
  "authors","elsevier","springer","wiley","ieee","copyright",
  "reserved","rights","journal","publisher","published","publication",
  # genérico inglés (no incluidos en tidytext)
  "paper","study","studies","research","approach","method","methods",
  "proposed","presents","present","propose","proposes",
  "used","using","results","result","work","problem","solution",
  "objective","objectives","number","time","different","new",
  "also","well","including","high","low","large","small",
  "various","several","real","case","shown","demonstrate","demonstrates",
  "however","therefore","thus","order","set","provide","provides",
  "increasing","increased","increase","reduce","reducing","reduction",
  "improve","improved","improving","improvement","significant","significantly",
  "achieve","achieved","existing","develop","developed","developing",
  "evaluate","evaluated","evaluating","consider","considering","considered",
  "address","addressing","compare","compared","comparison",
  "apply","applied","applying","based","make","made","take","taken",
  "give","given","show","indicate","indicates","indicated",
  # dominio genérico portuario
  "port","ports","terminal","terminals","container","containers",
  "ship","ships","vessel","vessels","shipping","marine",
  "cargo","logistics","transport","transportation","maritime",
  "simulation","simulations","simulate","simulated","simulator",
  "model","models","modeling","modelling","modeled",
  "discrete","event","agent","system","systems","dynamics",
  "performance","efficiency","operations","operational",
  "data","analysis","framework","approach"
))

# =============================================================
# 3. TOKENIZACIÓN
# =============================================================
tokens <- tabla_abs %>%
  unnest_tokens(word, Abstract) %>%
  filter(str_detect(word, "^[a-z][a-z-]{2,}$")) %>%   # solo letras
  anti_join(stop_words,   by = "word") %>%
  anti_join(custom_stop,  by = "word") %>%
  filter(nchar(word) >= 4)

# Frecuencia global
term_freq <- tokens %>%
  count(word, sort = TRUE)

cat("✓ Vocabulario tras limpieza:", nrow(term_freq), "términos\n")
cat("Top 25:\n"); print(head(term_freq, 25))

# =============================================================
# 4. CO-OCURRENCIA POR DOCUMENTO
# =============================================================
top_terms <- term_freq %>%
  filter(n >= MIN_TERM_FREQ) %>%
  pull(word)

cat("✓ Términos con freq >=", MIN_TERM_FREQ, ":", length(top_terms), "\n")

cooc <- tokens %>%
  filter(word %in% top_terms) %>%
  distinct(doc_id, word) %>%           # un token por doc (evita doble conteo)
  pairwise_count(word, doc_id,
                 sort = TRUE,
                 upper = FALSE) %>%
  filter(n >= MIN_COOC)

cat("✓ Pares con co-ocurrencia >=", MIN_COOC, ":", nrow(cooc), "\n")

# =============================================================
# 5. GRAFO + COMUNIDADES LOUVAIN
# =============================================================
g <- graph_from_data_frame(
  cooc %>% rename(from = item1, to = item2, weight = n),
  directed = FALSE,
  vertices  = term_freq %>%
    filter(word %in% unique(c(cooc$item1, cooc$item2))) %>%
    rename(name = word, freq = n)
)

# Mantener solo nodos con grado >= 2
g <- induced_subgraph(g, V(g)[degree(g) >= 2])

set.seed(42)
comm  <- cluster_louvain(g, weights = E(g)$weight)
V(g)$cluster <- as.character(membership(comm))
V(g)$freq    <- V(g)$freq   # ya estaba en los atributos

tg <- as_tbl_graph(g)
cat("✓ Nodos:", gorder(g), "| Aristas:", gsize(g), "\n")
cat("✓ Comunidades:", length(unique(membership(comm))), "\n")

# Top términos por comunidad
for (i in sort(unique(membership(comm)))) {
  nodes_i <- V(g)[membership(comm) == i]$name
  top5    <- head(nodes_i[order(-V(g)[membership(comm) == i]$freq)], 5)
  cat(sprintf("  Cluster %d: %s\n", i, paste(top5, collapse = ", ")))
}

# =============================================================
# 6. LAYOUT Fruchterman-Reingold ponderado
# =============================================================
set.seed(42)
layout_fr <- create_layout(tg, layout = "fr",
                           weights = E(tg)$weight)

# =============================================================
# 7. MAPA DE DENSIDAD TIPO VOSVIEWER
# =============================================================

# Coordenadas y pesos de nodos
xy <- layout_fr %>% select(x, y, freq)

# KDE 2D
kde <- kde2d(xy$x, xy$y,
             h    = c(bandwidth.nrd(xy$x) * 2.2,
                      bandwidth.nrd(xy$y) * 2.2),
             n    = 250,
             lims = c(range(xy$x) + c(-0.5, 0.5),
                      range(xy$y) + c(-0.5, 0.5)))

kde_df <- expand.grid(x = kde$x, y = kde$y) %>%
  mutate(density = as.vector(kde$z),
         density = density / max(density))

# Colormap VOSviewer: negro → azul oscuro → rojo → naranja → amarillo pálido
vos_colors <- c(
  "#000000", "#0a0a2e", "#1a0a5e",
  "#7b0000", "#cc2200", "#ff4500",
  "#ff8c00", "#ffd700", "#fffacd"
)
vos_values <- c(0, 0.12, 0.28, 0.44, 0.58, 0.70, 0.82, 0.92, 1.00)

# Nodos con etiqueta: top N por (grado × frecuencia)
deg_score <- degree(g) * V(g)$freq
label_nodes <- names(sort(deg_score, decreasing = TRUE))[seq_len(N_TOP_LABELS)]

layout_fr <- layout_fr %>%
  mutate(label_show = name %in% label_nodes)

# =============================================================
# 8. PLOT FINAL
# =============================================================
max_freq <- max(layout_fr$freq)

p <- ggplot() +
  
  # ── Fondo de densidad ──────────────────────────────────────
  geom_raster(data = kde_df,
              aes(x = x, y = y, fill = density),
              interpolate = TRUE) +
  scale_fill_gradientn(
    colours = vos_colors,
    values  = vos_values,
    guide   = "none"
  ) +
  
  # ── Aristas ────────────────────────────────────────────────
  geom_edge_link(
    data = layout_fr,
    aes(x = x, y = y, xend = xend, yend = yend,
        alpha = weight, linewidth = weight),
    color = "white",
    show.legend = FALSE
  ) +
  scale_edge_alpha(range    = c(0.02, 0.15), guide = "none") +
  scale_edge_linewidth(range = c(0.15, 0.9),  guide = "none") +
  
  # ── Nodos ──────────────────────────────────────────────────
  geom_node_point(
    data = layout_fr,
    aes(x = x, y = y,
        size  = freq,
        color = cluster),
    alpha = 0.88, stroke = 0.4,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_color_manual(values = PALETTE, na.value = "grey70") +
  
  # ── Etiquetas ──────────────────────────────────────────────
  geom_text_repel(
    data = layout_fr %>% filter(label_show),
    aes(x = x, y = y, label = name,
        size  = freq,
        color = cluster),
    fontface      = "bold",
    bg.color      = NA,
    max.overlaps  = 35,
    segment.color = "white",
    segment.alpha = 0.25,
    box.padding   = 0.20,
    point.padding = 0.08,
    show.legend   = FALSE
  ) +
  scale_size_continuous(range = c(2.5, 5.5)) +
  
  # ── Tema oscuro ────────────────────────────────────────────
  coord_fixed() +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.title       = element_text(color = "white", face = "bold",
                                    size = 14, hjust = 0.5,
                                    margin = margin(t = 8, b = 4)),
    plot.subtitle    = element_text(color = "grey65", size = 8,
                                    hjust = 0.5,
                                    margin = margin(b = 6)),
    plot.caption     = element_text(color = "grey45", size = 7,
                                    hjust = 1),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  labs(
    title    = "Term Co-occurrence Network — Port Terminal Simulation Studies",
    subtitle = paste0(
      "n = ", nrow(tabla_abs), " included studies",
      "  |  min. term freq = ", MIN_TERM_FREQ,
      "  |  min. co-occurrence = ", MIN_COOC,
      "  |  node size ∝ frequency  |  color = Louvain cluster"
    ),
    caption  = "Source: abstracts from included studies — Mathematical Analytical paradigm excluded"
  )

# =============================================================
# 9. GUARDAR
# =============================================================
ggsave("outputs/TERM_COOC_network_final.png",
       plot   = p,
       width  = 14, height = 10,
       dpi    = 300, bg = "black")

cat("✓ Guardado en outputs/TERM_COOC_network_final.png\n")

# =============================================================
# OPCIONAL: tabla de términos por clúster
# =============================================================
cluster_tbl <- tibble(
  word    = V(g)$name,
  freq    = V(g)$freq,
  cluster = V(g)$cluster
) %>%
  arrange(cluster, desc(freq))

cat("\n── Top 6 términos por clúster ──\n")
cluster_tbl %>%
  group_by(cluster) %>%
  slice_head(n = 6) %>%
  print(n = Inf)