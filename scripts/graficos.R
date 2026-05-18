#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("RColorBrewer")
library(readxl)
library(ggplot2)
library(forcats)
library(RColorBrewer)
# =========================
# Cargar datos
# =========================
tabla <- read_excel("data/data.xlsx", skip = 2)

# =========================
# Primer gráfico (MAIN PARADIGM)
# =========================
ggplot(tabla, aes(x = `Main paradigm`, fill = `Main paradigm`)) + geom_bar() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) #comando para hacer grafico de barras
ggsave("outputs/paradigms.png", width = 10, height = 6) # comando para guardar en outputs

# =========================
# Segundo (HYBRID COMBINATION)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Hybrid combination`), fill = `Hybrid combination`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Hybrid Combinations", x = "Count", y = "")
ggsave("outputs/hybrid_combination.png", width = 10, height = 5)


# =========================
# Tercer gráfico (TOP 10 TOOL/SOFTWARE)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$`Tool / Software`), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Tools / Software", x = "Count", y = "")
ggsave("outputs/top_tools_software.png", width = 12, height = 7)

# =========================
# Cuarto gráfico (INTEGRATES OPTIMIZATION?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Integrates optimization?`), fill = `Integrates optimization?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Integrates Optimization?", x = "Count", y = "")
ggsave("outputs/integrates_optimization.png", width = 8, height = 5)

# =========================
# Quinto gráfico (TOP 10 OPTIMIZATION METHOD)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$`Optimization method`), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Optimization Methods", x = "Count", y = "")
ggsave("outputs/top_optimization_methods.png", width = 13, height = 7)

# =========================
# Sexto gráfico (TERMINAL TYPE)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Terminal type`), fill = `Terminal type`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Terminal Types", x = "Count", y = "")
ggsave("outputs/terminal_type.png", width = 10, height = 5)

# =========================
# Séptimo gráfico (REAL TERMINAL?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Real terminal?`), fill = `Real terminal?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Real Terminal?", x = "Count", y = "")
ggsave("outputs/real_terminal.png", width = 8, height = 5)

# =========================
# Octavo gráfico (TOP 10 TERMINALS / PORTS)
# =========================

ggplot(subset(tabla, !is.na(`Terminal / port name`)), aes(y = fct_lump_n(fct_infreq(`Terminal / port name`), n = 10), fill = fct_lump_n(fct_infreq(`Terminal / port name`), n = 10))) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Most Frequent Terminals / Ports", x = "Count", y = "")
ggsave("outputs/top_terminals_ports.png", width = 12, height = 8)

# =========================
# Noveno gráfico (MAIN PROCESS)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Main process`), fill = `Main process`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Main Processes", x = "Count", y = "")
ggsave("outputs/main_process.png", width = 11, height = 6)

# =========================
# Décimo gráfico (TOP 10 SPECIFIC SUBPROCESSES)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$`Specific subprocess`), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Specific Subprocesses", x = "Count", y = "")
ggsave("outputs/top_specific_subprocess.png", width = 12, height = 7)

# =========================
# Undécimo gráfico (TIME HORIZON)
# =========================

ggplot(tabla, aes(x = `Time horizon`, fill = `Time horizon`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Time Horizon", x = "", y = "Count")
ggsave("outputs/time_horizon.png", width = 8, height = 5)

# PIE CHART
ggplot(tabla, aes(x = "", fill = `Time horizon`)) + geom_bar(width = 1, color = "black", size = 0.7) + coord_polar(theta = "y") + theme_void() + scale_fill_brewer(palette = "Set3") + labs(title = "Time Horizon") + theme(plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
ggsave("outputs/time_horizon.png", width = 8, height = 8, bg = "white")

# =========================
# Duodécimo gráfico (MODEL OBJECTIVE)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$`Model objective`), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Model Objectives", x = "Count", y = "")
ggsave("outputs/model_objective.png", width = 13, height = 8)

# =========================
# Decimotercer gráfico (VALIDATED?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Validated?`), fill = `Validated?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Validated Models", x = "Count", y = "")
ggsave("outputs/validated.png", width = 8, height = 5)

# PIE CHART
ggplot(tabla, aes(x = "", fill = `Validated?`)) + geom_bar(width = 1, color = "black", size = 0.7) + coord_polar(theta = "y") + theme_void() + scale_fill_brewer(palette = "Set2") + labs(title = "Validated Models") + theme(plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
ggsave("outputs/validated.png", width = 8, height = 8, bg = "white")

# =========================
# Decimocuarto gráfico (TOP 10 VALIDATION METHODS)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$`Validation method`), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Validation Methods", x = "Count", y = "")
ggsave("outputs/top_validation_methods.png", width = 13, height = 7)

# =========================
# Decimoquinto gráfico (SENSITIVITY ANALYSIS)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Sensitivity analysis?`), fill = `Sensitivity analysis?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Sensitivity Analysis", x = "Count", y = "")
ggsave("outputs/sensitivity_analysis.png", width = 8, height = 5)

# =========================
# Decimosexto gráfico (INCLUDES ANIMATION?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Includes animation?`), fill = `Includes animation?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Includes Animation?", x = "Count", y = "")
ggsave("outputs/includes_animation.png", width = 8, height = 5)

# =========================
# Decimoséptimo gráfico (CONSIDERS SUSTAINABILITY?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Considers sustainability?`), fill = `Considers sustainability?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Considers Sustainability?", x = "Count", y = "")
ggsave("outputs/considers_sustainability.png", width = 8, height = 5)

# =========================
# Decimoctavo gráfico (REGION)
# =========================

ggplot(tabla, aes(x = "", fill = Region)) + geom_bar(width = 1, color = "black", size = 0.7) + coord_polar(theta = "y") + theme_void() + scale_fill_brewer(palette = "Set2") + labs(title = "Regions") + theme(plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
ggsave("outputs/regions.png", width = 8, height = 8, bg = "white")

# =========================
# Decimonoveno gráfico (ECONOMIC CLASSIFICATION)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Economic classification`), fill = `Economic classification`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Economic Classification", x = "Count", y = "")
ggsave("outputs/economic_classification.png", width = 10, height = 6)

# =========================
# Vigésimo gráfico (LATIN AMERICAN COUNTRY?)
# =========================

ggplot(tabla, aes(y = fct_infreq(`Latin American country?`), fill = `Latin American country?`)) + geom_bar() + theme_minimal() + theme(legend.position = "none") + labs(title = "Latin American Country?", x = "Count", y = "")
ggsave("outputs/latin_american_country.png", width = 8, height = 5)

# =========================
# Vigésimo primer gráfico (TOP 10 COUNTRIES)
# =========================

ggplot(head(subset(as.data.frame(sort(table(tabla$Country), decreasing = TRUE)), Var1 != ""), 10), aes(y = reorder(Var1, Freq), x = Freq, fill = Var1)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + labs(title = "Top 10 Countries", x = "Count", y = "")
ggsave("outputs/top_countries.png", width = 12, height = 7)