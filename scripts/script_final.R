# =============================================================
# SCRIPT FINAL — Simulación en terminales portuarios
# Systematic Literature Review — Visualizaciones completas
# n = 87 estudios incluidos (excluye Mathematical Analytical)
# =============================================================

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