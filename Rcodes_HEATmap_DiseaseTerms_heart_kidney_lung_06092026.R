library(dplyr)
library(ggplot2)

setwd("D:/Shutdown/FHS_03182026/")
df <- read.delim("D:/Shutdown/FHS_03182026/Jensen_DISEASE_terms_for_hypertension_nephritis_respiratory_input_06092026.txt")

# Define cell-type order
cell_order <- c(
  "B","B.1", "B.2",
  "T", "T.1","T.2","T.3","T.4",
  "Mono", "Mono.1", "Mono.2", "Mono.3",
  "Neut","Neut.1", "Neut.2", "Neut.3", "Neut.4", "Neut.5",
  "RBC","RBC.1","RBC.2", "NK", "DC", "Proliferating", "Platelets"
)



df2 <- df %>%
  mutate(
    Cell = factor(Cell, levels = cell_order),
    Term = factor(
      Term,
      levels = c(
        "RESPIRATORY SYSTEM DISEASE",
        "NEPHRITIS",
        "HYPERTENSION"
      )
    ),
    log10_adjP = -log10(Adj.pval)
  )


###
p <- ggplot(df2, aes(
  x = Cell,
  y = Term,
  color = log10_adjP
)) +
  geom_point(size = 6) +   # fixed bubble size
  
  scale_color_gradient(
    low  = "grey98",
    high = "darkred",
    name = "-log10(FDR)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  
  labs(
    x = "",
    y = ""
  )

print(p)




###
library(ggplot2)
png(
  filename = "D:/Shutdown/MS figures/Disease_enrichement_bubbleplot.png",
  width = 12,
  height = 5,
  units = "in",
  res = 600,
  type = "cairo"
)

print(p)

dev.off()
