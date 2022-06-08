# załadowanie bibliotek
library(tm)
library(hunspell)
library(lsa)
library(topicmodels)
library(wordcloud)
# zmiana katalogu roboczego
work_dir <- "C:/Users/vpnt64/Desktop/PlantVillage/PJNN11"
setwd(work_dir)

# zdefiniowanie katalogów funkcjonalnych
input_dir <- "./data"
output_dir <- "./projektOutput"
scripts_dir <- "./scripts"
workspaces_dir <- "./workspaces"
topics_dir <- create_path(output_dir,"topics")
plots_dir <- create_path(output_dir,"plots")
clouds_dir <- create_path(output_dir,"clouds")
clusters_dir <- create_path(output_dir,"clusters")

# utworzenie katalogów wynikowych
dir.create(output_dir, showWarnings = F)
dir.create(workspaces_dir, showWarnings = F)
dir.create(plots_dir, showWarnings = F)
dir.create(topics_dir, showWarnings = F)
dir.create(clouds_dir, showWarnings = F)
dir.create(clusters_dir, showWarnings = F)
# wykonaie skryptu z definicjami funkcji
source_file <- paste(
  scripts_dir,
  "functions.R",
  sep = "/"
)
source(source_file)

# utworzenie korpusu dokumentów
corpus_dir <- create_path(
	input_dir,
	"new dataset"
)

corpus <- VCorpus(
	DirSource(
		corpus_dir,
#		pattern = "*.txt",
		encoding = "UTF-8"
	),
	readerControl = list(
		language = "pl_PL"
	)
)

# wstępne przetwarzanie
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, cut_extensions)

stoplist_file <- create_path(
	input_dir,
	"stopwords_pl.txt"
)
stoplist <- readLines(
	stoplist_file, 
	encoding = "UTF-8"
)
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

# wywołanie własnych funkcji transformujących
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, remove_char, intToUtf8(190))
corpus <- tm_map(corpus, content_transformer(paste_paragraphs))
corpus <- tm_map(corpus, cut_extensions)
corpus <- tm_map(corpus, stripWhitespace)

# zdefiniowanie funkcji do lematyzacji
polish <- dictionary("pl_PL")
lemmatize <- function(text){
  vectorized_text <- unlist(hunspell_parse(text, dict = polish))
  lemmatized_vectorized_text <- hunspell_stem(vectorized_text, dict = polish)
  for (i in 1:length(lemmatized_vectorized_text)) {
    if (length(lemmatized_vectorized_text[[i]]) == 0){
      lemmatized_vectorized_text[i] <- vectorized_text[i]
    }
    if (length(lemmatized_vectorized_text[[i]])  > 1){
      lemmatized_vectorized_text[i] <- lemmatized_vectorized_text[[i]][1]
    }
  }
  lemmatized_vectorized_text <- unlist(lemmatized_vectorized_text)
  lemmatized_text <- paste(lemmatized_vectorized_text, collapse = " ")
  return(lemmatized_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

# eksport zawartości kurpusu do plików tekstowych
preprocessed_dir <- create_path(
  input_dir,
  "new_dataset_przetworzone"
)
dir.create(preprocessed_dir, showWarnings = F)
writeCorpus(corpus, preprocessed_dir)


## frequency_matrix




# utworzenie macierzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
tdm_tfidf_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdm_bin_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdm_tf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdm_tfidf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
#our
tdm_tfidf_4_18 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(4,18)
    )
  )
)
tdm_bin_1_20 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin,
    bounds = list(
      global = c(1,20)
    )
  )
)


dtm_tf_all <- DocumentTermMatrix(corpus)
dtm_tfidf_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtm_bin_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
dtm_tf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tfidf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

#our
dtm_tfidf_4_18 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(4,18)
    )
  )
)
dtm_bin_1_20 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin,
    bounds = list(
      global = c(1,20)
    )
  )
)

# przekształcenie macierzy rzadkich do macierzy klasycznych
tdm_tf_all_m <- as.matrix(tdm_tf_all)
tdm_tfidf_all_m <- as.matrix(tdm_tfidf_all)
tdm_bin_all_m <- as.matrix(tdm_bin_all)
tdm_tf_2_16_m <- as.matrix(tdm_tf_2_16)
tdm_tfidf_2_16_m <- as.matrix(tdm_tfidf_2_16)
#our
tdm_tfidf_4_18_m <- as.matrix(tdm_tfidf_4_18)
tdm_bin_1_20_m <- as.matrix(tdm_bin_1_20)


dtm_tf_all_m <- as.matrix(dtm_tf_all)
dtm_tfidf_all_m <- as.matrix(dtm_tfidf_all)
dtm_bin_all_m <- as.matrix(dtm_bin_all)
dtm_tf_2_16_m <- as.matrix(dtm_tf_2_16)
dtm_tfidf_2_16_m <- as.matrix(dtm_tfidf_2_16)
#our
dtm_tfidf_4_18_m <- as.matrix(dtm_tfidf_4_18)
dtm_bin_1_20_m <- as.matrix(dtm_bin_1_20)

# utworzenie katalogu na macierze
matrixes_dir <- create_path(
  output_dir,
  "matrixes"
)
dir.create(matrixes_dir, showWarnings = F)

# eksport macierzy do pliku
matrix_file <- create_path(
matrixes_dir,
"tdm_tf_all.csv"
)
 write.table(
   tdm_tf_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_tf_all.csv"
 )
 write.table(
   dtm_tf_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 ########
 matrix_file <- create_path(
   matrixes_dir,
   "tdm_tfidf_all_m.csv"
 )
 write.table(
   tdm_tfidf_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_tfidf_all_m.csv"
 )
 write.table(
   dtm_tfidf_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 ########
 matrix_file <- create_path(
   matrixes_dir,
   "tdm_tfidf_2_16_m.csv"
 )
 write.table(
   tdm_tfidf_2_16_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_tfidf_2_16_m.csv"
 )
 write.table(
   dtm_tfidf_2_16_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 ########
 matrix_file <- create_path(
   matrixes_dir,
   "tdm_bin_all_m.csv"
 )
 write.table(
   tdm_bin_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_bin_all_m.csv"
 )
 write.table(
   dtm_bin_all_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 
 ########
 matrix_file <- create_path(
   matrixes_dir,
   "tdm_tfidf_4_18_m.csv"
 )
 write.table(
   tdm_tfidf_4_18_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_tfidf_4_18_m.csv"
 )
 write.table(
   dtm_tfidf_4_18_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 
 ########
 matrix_file <- create_path(
   matrixes_dir,
   "tdm_bin_1_20.csv"
 )
 write.table(
   tdm_bin_1_20_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 matrix_file <- create_path(
   matrixes_dir,
   "dtm_bin_1_20_m.csv"
 )
 write.table(
   dtm_bin_1_20_m,
   matrix_file,
   sep = ";",
   dec = ",",
   col.names = NA
 )
 
 
 ######Reduction
 
# analiza głównych składowych
pca <- prcomp(dtm_tfidf_4_18)

# wykres dokumentów w przestrzeni dwuwymiarowej
x <- pca$x[,1]
y <- pca$x[,2]
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_4_18)),
    sep = ""
  ),
  rownames(dtm_tfidf_4_18),
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
    1:length(rownames(dtm_tfidf_4_18)),
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

# analiza ukrytych wymiarów semantycznych
# dekompozycja wg wartości osobliwych
lsa <- lsa(tdm_tfidf_4_18)

# wykres dokumentów w przestrzeni dwuwymiarowej
coord_docs <- lsa$dk%*%diag(lsa$sk)
coord_terms <- lsa$tk%*%diag(lsa$sk)
terms_importance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
important_terms <- names(tail(sort(terms_importance),30))
coord_important_terms <- coord_terms[important_terms,]
own_terms <- c("chałupa", "ser", "mistrz", "studia", "jeno", "kiej", "lord", "polski")
coord_own_terms <- coord_terms[own_terms,]
coord_plot_terms <- coord_own_terms

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]

x2 <- coord_plot_terms[,1]
y2 <- coord_plot_terms[,2]

legend <- paste(
  paste(
    "d",
    1:length(colnames(tdm_tfidf_4_18)),
    sep = ""
  ),
  colnames(tdm_tfidf_4_18),
  sep = " - "
)

plot_file <- create_path(
  plots_dir,
  "lsa.png"
)
png(plot_file)
plot(
  x1, 
  y1,
  xlim = c(-5,0)
)
text(
  x1, 
  y1, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_4_18)),
    sep = ""
  ),
  pos = 4
)
points(
  x2,
  y2,
  pch = 2
)
text(
  x2,
  y2,
  rownames(coord_plot_terms)
)
legend(
  "topleft",
  legend,
  cex = 0.6
)

dev.off()


#### Topic_modeling

# analiza ukrytej alokacji Dirichlet'a
topics_count <- 4
lda <- LDA(
  dtm_tf_2_16,
  k = topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000, 
    thin = 100,
    iter = 3000
  )
)

results <- posterior(lda)
topics_colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")

for (t in 1:topics_count) {
  topic_file <- create_path(
    topics_dir,
    paste("temat_", t, ".png", sep = "")
  )
  png(topic_file)
  par(mai=c(1,2,1,1))
  topic <- tail(sort(results$terms[t,]),20)
  barplot(
    topic,
    horiz = T,
    las = 1,
    main = paste("Temat", t),
    xlab = "Prawdopodobieństwo",
    col = topics_colors[t]
  )
  dev.off()
}

for (d in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_dir,
    paste(rownames(results$topics)[d],".png", sep = "")
  )
  png(doc_file)
  par(mai=c(1,2,1,1))
  document <- results$topics[d,]
  barplot(
    document,
    las = 1,
    main = rownames(results$topics)[d],
    xlab = "Prawdopodobieństwo",
    col = topics_colors
  )
  dev.off()
}

par(mai=c(1,4,1,1))
barplot(
  t(results$topics),
  horiz = T,
  col = topics_colors,
  las = 1
)
## keywords
# waga tf jako miara istotności słów
for (d in 1:length(corpus)) {
  print(rownames(dtm_tf_2_16_m)[d])
  print(head(sort(dtm_tf_2_16_m[d,], decreasing = T)))
}

# waga tfidf jako miara istotności słów
for (d in 1:length(corpus)) {
  print(rownames(dtm_tfidf_2_16_m)[d])
  print(head(sort(dtm_tfidf_2_16_m[d,], decreasing = T)))
}

# waga tfidf jako miara istotności słów
for (d in 1:length(corpus)) {
  print(rownames(dtm_tfidf_4_18_m)[d])
  print(head(sort(dtm_tfidf_4_18_m[d,], decreasing = T)))
}

# prawdopodobieństwo w LDA jako miara istotności słów
for (d in 1:length(corpus)) {
  terms_importance <- c(results$topics[d,]%*%results$terms)
  names(terms_importance) <- colnames(results$terms)
  print(rownames(results$topics)[d])
  print(head(sort(terms_importance, decreasing = T)))
}

# chmury tagów
for (d in 1:length(corpus)) {
  cloud_file <- create_path(
    clouds_dir,
    paste(corpus[[d]]$meta$id,".png", sep = "")
  )
  png(cloud_file)
  par(mai=c(0,0,0,0))
  wordcloud(
    corpus[d],
    max.words = 200,
    colors = brewer.pal(8,"Spectral"),
  )
  dev.off()
}


##clustering

# analiza skupień
# metoda hierarchiczna
# parametry:
# 1. macierz częstości
# a. waga (tf, tfidf, bin, log)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, manhatan, jaccard, cosine)
# 3. sposób wyznaczania odległości skupień (single, complete, ward.D2)

# przygotowanie
doc_names <- rownames(dtm_tf_all)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  doc_names,
  sep = " - "
)
rownames(dtm_tf_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_bin_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tfidf_2_16_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)

clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink", "khaki")
colors_pattern <- c()
for (d in 1:doc_count) {
  colors_pattern[d] <- colors[clusters_pattern[d]]
}
names(clusters_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)

names(colors_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
par(mai=c(1,1,1,1))

# eksperyment 1
dist_matrix_1 <- dist(dtm_tf_all_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")
plot(h_clust_1)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_1 <- as.dendrogram(h_clust_1)
clust_count_1 <- find_k(dendrogram_1)$k
colored_dendrogram_1 <- color_branches(
  dendrogram_1,
  k = clust_count_1
)
plot(colored_dendrogram_1)
clusters_1 <- cutree(h_clust_1, k = clust_count_1)
clusters_matrix_1 <- matrix(
  0,
  doc_count,
  clust_count_1
)
rownames(clusters_matrix_1) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_1[doc_no, clusters_1[doc_no]] <- 1
}
corrplot(clusters_matrix_1)

# eksperyment 2
dist_matrix_2 <- dist(dtm_bin_all_m, method = "jaccard")
h_clust_2 <- hclust(dist_matrix_2, method = "single")
plot(h_clust_2)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_2 <- as.dendrogram(h_clust_2)
clust_count_2 <- find_k(dendrogram_2)$k
colored_dendrogram_2 <- color_branches(
  dendrogram_2,
  k = clust_count_2
)
plot(colored_dendrogram_2)
clusters_2 <- cutree(h_clust_2, k = clust_count_2)
clusters_matrix_2 <- matrix(
  0,
  doc_count,
  clust_count_2
)
rownames(clusters_matrix_2) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_2[doc_no, clusters_2[doc_no]] <- 1
}
corrplot(clusters_matrix_2)

# eksperyment 3
dist_matrix_3 <- dist(dtm_tf_all_m, method = "cosine")
h_clust_3 <- hclust(dist_matrix_3, method = "ward.D2")
plot(h_clust_3)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_3 <- as.dendrogram(h_clust_3)
clust_count_3 <- find_k(dendrogram_3)$k
colored_dendrogram_3 <- color_branches(
  dendrogram_3,
  k = clust_count_3
)
plot(colored_dendrogram_3)
clusters_3 <- cutree(h_clust_3, k = clust_count_3)
clusters_matrix_3 <- matrix(
  0,
  doc_count,
  clust_count_3
)
rownames(clusters_matrix_3) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_3[doc_no, clusters_3[doc_no]] <- 1
}
corrplot(clusters_matrix_3)

# metoda niehierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. liczba skupień

# eksperyment 4
clust_count_4 <- 3
k_means_4 <- kmeans(dtm_tfidf_2_16, centers = clust_count_4)
clusters_4 <- k_means_4$cluster
clusters_matrix_4 <- matrix(
  0,
  doc_count,
  clust_count_4
)
rownames(clusters_matrix_4) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_4[doc_no, clusters_4[doc_no]] <- 1
}
corrplot(clusters_matrix_4)

# porównanie wyników eksperymentów
Bk_plot(
  dendrogram_1,
  dendrogram_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dendrogram_1,
  dendrogram_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dendrogram_2,
  dendrogram_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)

rand_exp1_pattern <- comPart(clusters_1, clusters_pattern)
rand_exp2_pattern <- comPart(clusters_2, clusters_pattern)
rand_exp3_pattern <- comPart(clusters_3, clusters_pattern)
rand_exp4_pattern <- comPart(clusters_4, clusters_pattern)
rand_exp1_exp2 <- comPart(clusters_1, clusters_2)
rand_exp1_exp4 <- comPart(clusters_1, clusters_4)
rand_exp2_exp4 <- comPart(clusters_2, clusters_4)
