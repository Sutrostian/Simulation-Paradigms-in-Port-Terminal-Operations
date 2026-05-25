# =========================
# CONTINUACIÓN DEL SCRIPT PRINCIPAL
# Nuevos gráficos y análisis
# =========================
install.packages("rlang")
install.packages("maps")
library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)

# Cargar y filtrar datos (igual que antes)
tabla <- read_excel("data/data.xlsx", sheet = "Extracted Data + Decision", skip = 2)
tabla <- subset(tabla, `Phase 2 Decision` == "Include")

# Eliminar Mathematical Analytical del paradigma principal
tabla <- subset(tabla, `Main paradigm` != "Mathematical Analytical")

# Fusionar variantes de Container en Terminal type
tabla$`Terminal type merged` <- ifelse(
  grepl("Container", tabla$`Terminal type`, ignore.case = TRUE),
  "Container",
  tabla$`Terminal type`
)

# =========================
# HELPER: porcentajes en barras
# =========================
# Se usa geom_text con after_stat(count) para agregar % sobre cada barra

# =========================
# 1. PARADIGMA — con porcentajes
# =========================
ggplot(tabla, aes(x = fct_infreq(`Main paradigm`), fill = `Main paradigm`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = paste0(after_stat(count), "\n(",
    round(after_stat(count) / nrow(tabla) * 100, 1), "%)")),
    vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Main Paradigm", x = "", y = "Count")
ggsave("outputs/paradigms.png", width = 8, height = 5, bg = "white")

# =========================
# 2. TERMINAL TYPE (fusionado) — con porcentajes
# =========================
ggplot(tabla, aes(x = fct_infreq(`Terminal type merged`), fill = `Terminal type merged`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = paste0(after_stat(count), "\n(",
    round(after_stat(count) / nrow(tabla) * 100, 1), "%)")),
    vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Terminal Type (Containers merged)", x = "", y = "Count")
ggsave("outputs/terminal_type_merged.png", width = 8, height = 5, bg = "white")

# =========================
# 3. TERMINAL TYPE — análisis temporal (apilado, Container vs RoRo vs otros)
# =========================
terminal_year <- tabla %>%
  filter(!is.na(`Terminal type merged`)) %>%
  count(Year, `Terminal type merged`) %>%
  group_by(Year) %>%
  mutate(pct = n / sum(n))

ggplot(terminal_year, aes(x = factor(Year), y = n, fill = `Terminal type merged`)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  geom_text(data = subset(terminal_year, n > 0),
    aes(label = n), position = position_stack(vjust = 0.5), size = 2.8, color = "white") +
  theme_minimal() +
  labs(title = "Terminal Type by Year — Are RoRo publications growing?",
    x = "Year", y = "Number of papers", fill = "Terminal type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("outputs/terminal_type_by_year.png", width = 11, height = 6, bg = "white")

# =========================
# 4. INTEGRA OPTIMIZACIÓN — con porcentajes
# =========================
ggplot(tabla, aes(y = fct_infreq(`Integrates optimization?`), fill = `Integrates optimization?`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = paste0(after_stat(count), " (",
    round(after_stat(count) / nrow(tabla) * 100, 1), "%)")),
    hjust = -0.1, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Integrates Optimization?", x = "Count", y = "")
ggsave("outputs/integrates_optimization.png", width = 9, height = 4, bg = "white")

# =========================
# 5. OPTIMIZACIÓN: de los "Yes", ¿qué método usan?
# =========================
tabla_opt_yes <- subset(tabla, `Integrates optimization?` == "Yes" &
  !is.na(`Optimization method`) & `Optimization method` != "")

# Agrupar metaheurísticas similares para simplificar
tabla_opt_yes$`Opt method grouped` <- case_when(
  grepl("Metaheuristic|metaheuristic", tabla_opt_yes$`Optimization method`) ~ "Metaheuristic (all variants)",
  grepl("Reinforcement learning|Deep reinforcement", tabla_opt_yes$`Optimization method`) ~ "Reinforcement learning",
  grepl("Linear programming|Mathematical programming", tabla_opt_yes$`Optimization method`) ~ "Mathematical programming",
  grepl("Genetic algorithm", tabla_opt_yes$`Optimization method`) ~ "Genetic algorithm",
  TRUE ~ tabla_opt_yes$`Optimization method`
)

ggplot(tabla_opt_yes, aes(y = fct_infreq(`Opt method grouped`), fill = `Opt method grouped`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), hjust = -0.2, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Optimization Methods (only 'Yes' studies, n=30)",
    subtitle = "Metaheuristic variants grouped together",
    x = "Count", y = "")
ggsave("outputs/optimization_methods_yes_only.png", width = 12, height = 7, bg = "white")

# =========================
# 6. MAPA DE CALOR: país/continente × objetivo del modelo
# =========================
heat_region <- tabla %>%
  filter(!is.na(Region), !is.na(`Model objective`), Region != "Not specified") %>%
  count(Region, `Model objective`) %>%
  complete(Region, `Model objective`, fill = list(n = 0))

ggplot(heat_region, aes(x = `Model objective`, y = Region, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.2) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276", name = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap: Region × Model Objective", x = "", y = "")
ggsave("outputs/heatmap_region_objective.png", width = 13, height = 6, bg = "white")

# Versión por país (top 10)
top_countries <- names(sort(table(tabla$Country[tabla$Country != "Not specified" & tabla$Country != "" & !is.na(tabla$Country)]), decreasing = TRUE))[1:10]
heat_country <- tabla %>%
  filter(Country %in% top_countries, !is.na(`Model objective`)) %>%
  count(Country, `Model objective`) %>%
  complete(Country, `Model objective`, fill = list(n = 0))

ggplot(heat_country, aes(x = `Model objective`, y = reorder(Country, -n, sum), fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.2) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276", name = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap: Top 10 Countries × Model Objective", x = "", y = "")
ggsave("outputs/heatmap_country_objective.png", width = 13, height = 7, bg = "white")

# =========================
# 7. TIME HORIZON — pie chart con mapa por continente
# =========================
# Pie chart global (ya existía, se mantiene con Set3)

# Stacked por región — ¿qué time horizon predomina en cada continente?
th_region <- tabla %>%
  filter(!is.na(Region), !is.na(`Time horizon`), Region != "Not specified") %>%
  count(Region, `Time horizon`) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(th_region, aes(x = reorder(Region, -n, sum), y = pct, fill = `Time horizon`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct > 7, paste0(pct, "%"), "")),
    position = position_stack(vjust = 0.5), size = 2.8) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Time Horizon by Region (% proportional)",
    subtitle = "What planning horizon dominates in each continent?",
    x = "", y = "Percentage (%)", fill = "Time horizon")
ggsave("outputs/time_horizon_by_region.png", width = 12, height = 6, bg = "white")

# =========================
# 8. ANÁLISIS DE SENSIBILIDAD — evolución temporal (Yes/No por año)
# =========================
sens_year <- tabla %>%
  count(Year, `Sensitivity analysis?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(sens_year, aes(x = factor(Year), y = n, fill = `Sensitivity analysis?`)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0, paste0(n, "\n(", pct, "%)"), "")),
    position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_manual(values = c("Yes" = "#1ABC9C", "No" = "#BDC3C7")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sensitivity Analysis Over Time",
    subtitle = "Are 'Yes' studies concentrated in recent years?",
    x = "Year", y = "Number of papers", fill = "Sensitivity analysis")
ggsave("outputs/sensitivity_analysis_by_year.png", width = 11, height = 6, bg = "white")

# =========================
# 9. VALIDACIÓN — evolución temporal (apilado)
# =========================
val_year <- tabla %>%
  count(Year, `Validated?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(val_year, aes(x = factor(Year), y = n, fill = `Validated?`)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n > 0, paste0(n, "\n(", pct, "%)"), "")),
    position = position_stack(vjust = 0.5), size = 2.3) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Validation Status Over Time",
    subtitle = "Linking Yes / Partially / No / Not specified by year",
    x = "Year", y = "Number of papers", fill = "Validated?")
ggsave("outputs/validated_by_year.png", width = 11, height = 6, bg = "white")

# =========================
# 10. TOP 10 SUBPROCESOS — linkeado por continente (heatmap)
# =========================
top_subproc <- tabla %>%
  filter(!is.na(`Specific subprocess`), `Specific subprocess` != "") %>%
  count(`Specific subprocess`, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(`Specific subprocess`)

heat_subproc <- tabla %>%
  filter(`Specific subprocess` %in% top_subproc,
    !is.na(Region), Region != "Not specified") %>%
  count(Region, `Specific subprocess`) %>%
  complete(Region, `Specific subprocess`, fill = list(n = 0))

ggplot(heat_subproc, aes(x = Region, y = `Specific subprocess`, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.2) +
  scale_fill_gradient(low = "#FDFEFE", high = "#1F618D", name = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Top 10 Specific Subprocesses by Region", x = "", y = "")
ggsave("outputs/subprocess_by_region_heatmap.png", width = 12, height = 7, bg = "white")

# =========================
# 11. TOOL/SOFTWARE — evolución temporal (presencia por año)
# =========================
# Excluir "Not specified" y mostrar los 6 más frecuentes
top_tools <- tabla %>%
  filter(!is.na(`Tool / Software`), `Tool / Software` != "Not specified", `Tool / Software` != "") %>%
  count(`Tool / Software`, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(`Tool / Software`)

tools_year <- tabla %>%
  filter(`Tool / Software` %in% top_tools) %>%
  count(Year, `Tool / Software`)

ggplot(tools_year, aes(x = factor(Year), y = n, color = `Tool / Software`,
  group = `Tool / Software`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Tool / Software Evolution Over Time",
    subtitle = "Is AnyLogic gaining ground? Is Arena declining?",
    x = "Year", y = "Number of papers", color = "Tool / Software")
ggsave("outputs/software_evolution.png", width = 12, height = 6, bg = "white")

# =========================
# 12. VALIDATION METHOD — evolución temporal (top métodos)
# =========================
top_val_methods <- tabla %>%
  filter(!is.na(`Validation method`), `Validation method` != "Not specified", `Validation method` != "") %>%
  count(`Validation method`, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(`Validation method`)

valmethod_year <- tabla %>%
  filter(`Validation method` %in% top_val_methods) %>%
  count(Year, `Validation method`)

ggplot(valmethod_year, aes(x = factor(Year), y = n, color = `Validation method`,
  group = `Validation method`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Validation Method Evolution Over Time",
    subtitle = "Is there a trend toward more rigorous methods?",
    x = "Year", y = "Number of papers", color = "Validation method")
ggsave("outputs/validation_method_evolution.png", width = 12, height = 6, bg = "white")

# =========================
# 13. ANIMACIÓN — evolución temporal (% Yes por año)
# =========================
anim_year <- tabla %>%
  count(Year, `Includes animation?`) %>%
  group_by(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(anim_year, aes(x = factor(Year), y = n, fill = `Includes animation?`)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  geom_text(data = subset(anim_year, `Includes animation?` == "Yes" & n > 0),
    aes(label = paste0(n, " (", pct, "%)")),
    position = position_stack(vjust = 0.5), size = 2.8, color = "white") +
  scale_fill_manual(values = c("Yes" = "#E67E22", "No" = "#D5D8DC")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Includes Animation? — Evolution Over Time",
    subtitle = "'Yes' studies labeled with count and percentage",
    x = "Year", y = "Number of papers", fill = "Includes animation?")
ggsave("outputs/animation_by_year.png", width = 11, height = 6, bg = "white")

# =========================
# 14. SOSTENIBILIDAD — mezclar con región
# =========================
sust_region <- tabla %>%
  filter(!is.na(Region), Region != "Not specified", !is.na(`Considers sustainability?`)) %>%
  count(Region, `Considers sustainability?`) %>%
  group_by(Region) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(sust_region, aes(x = reorder(Region, -n, sum), y = pct, fill = `Considers sustainability?`)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(pct, "%")),
    position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = c("Yes" = "#27AE60", "No" = "#BDC3C7")) +
  theme_minimal() +
  labs(title = "Considers Sustainability? by Region",
    subtitle = "Proportional — are some regions more sustainability-focused?",
    x = "", y = "Percentage (%)", fill = "Sustainability")
ggsave("outputs/sustainability_by_region.png", width = 10, height = 5, bg = "white")

# =========================
# 15. PROPORCIÓN DE DOCUMENTOS POR AÑO — papers/año con % del total
# =========================
papers_year <- tabla %>%
  count(Year) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(papers_year, aes(x = factor(Year), y = n)) +
  geom_bar(stat = "identity", fill = "#4E79A7", color = "black", linewidth = 0.3, width = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.3, size = 3) +
  scale_y_continuous(breaks = seq(0, 20, 5), expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  labs(title = "Publications Over Time — Proportion of Total",
    subtitle = paste0("Total: ", nrow(tabla), " included studies"),
    x = "Year", y = "Number of papers")
ggsave("outputs/papers_per_year_pct.png", width = 10, height = 6, bg = "white")

# =========================
# 16. ESTUDIO HÍBRIDO — detalle
# =========================
tabla_hybrid <- subset(tabla, `Main paradigm` == "Hybrid")

ggplot(tabla_hybrid, aes(y = `Hybrid combination`, fill = `Hybrid combination`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), hjust = -0.3, size = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Hybrid Studies — Combination Types",
    subtitle = paste0("n = ", nrow(tabla_hybrid), " hybrid studies out of ", nrow(tabla), " total"),
    x = "Count", y = "")
ggsave("outputs/hybrid_detail.png", width = 9, height = 4, bg = "white")
