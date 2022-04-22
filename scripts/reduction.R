# załadowanie bibliotek


# zmiana katalogu roboczego
work_dir <- "L:/lato21na22/PJNN11"
setwd(work_dir)

# zdefiniowanie lokalizacji katalogu ze skryptami
scripts_dir <- "./scripts"

# wykonanie skryptu frequency_matrix.R
source_file <- paste(
  scripts_dir,
  "frequency_matrix.R",
  sep = "/"
)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# utworzenie katalogu na wykresy
plots_dir <- create_path(
  output_dir,
  "plots"
)
dir.create(plots_dir, showWarnings = F)

# analiza głównych składowych
pca <- prcomp(dtm_tfidf_2_16)

# wykres dokumentów w przestrzeni dwuwymiarowej
x <- pca$x[,1]
y <- pca$x[,2]
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  rownames(dtm_tfidf_2_16),
  sep = " - "
)

plot_file <- create_path(
  plots_dir,
  "pca.png"
)
png(plot_file)
plot(
  x, 
  y
)
text(
  x, 
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4
)
legend(
  "bottom",
  legend,
  cex = 0.6
)

dev.off()
