# libraries needed ####

# data cleaning
library(tidyverse)
library(reshape2)

# parallelisation
library(doParallel)
library(foreach)

# plot related
library(extrafont)
# font_import()
library(ggrepel)
library(packcircles)
library(ggraph)
library(ggforce)
library(igraph)
library(tidygraph)
library(Polychrome)
library(scico)

# helper functions ####

preprocess_corpus <- function(x) {
  research_corpus <- tolower(x)  # force to lowercase
  research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
  research_corpus <- gsub("-", "", research_corpus)  # remove hyphens
  research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
  research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
  research_corpus <- trimws(research_corpus)
  research_corpus <-str_replace_all(research_corpus, "[\r\n]" , "")
  research_corpus <- gsub("^ *|(?<= ) | *$", "", research_corpus, perl = TRUE) # Remove multiple whitespace
  research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
  return(research_corpus)
}

randString <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

findX <- function(urn) {
  orgX <- workNodeCloud %>% filter(work == urn) %>% pull(x)
  rad <- workNodeCloud %>% filter(work == urn) %>% pull(radius)
  xcord <- orgX + runif(1, -rad, rad)
  return(xcord)
}

findY <- function(urn, xcord) {
  orgX <- workNodeCloud %>% filter(work == urn) %>% pull(x)
  orgY <- workNodeCloud %>% filter(work == urn) %>% pull(y)
  rad <- workNodeCloud %>% filter(work == urn) %>% pull(radius)
  margin <- abs(sqrt(rad^2 - (xcord - orgX)^2))
  ycord <- orgY + runif(1, -margin, margin)
  return(ycord)
}

getName <- function(x){
  newName <- genremap %>% filter(id == x) %>% pull(name)
  newName <- newName[1]
  if (is.na(newName)) {
    newName <- x
  }
  return(newName)
}

getGenre <- function(x){
  newName <- genremap %>% filter(id == x) %>% pull(mainGenre)
  newName <- newName[1]
  if (is.na(newName)) {
    newName <- x
  }
  return(newName)
}

fun.1 <- function(x) -x
fun.2 <- function(x) -x
fun.3 <- function(x) -log(x)

extractAuthorID <- function(x) {
  strsplit(strsplit(x, ":")[[1]][4], ".", fixed = T)[[1]][1]
}

getAuthor <- function(x){
  newName <- catalog %>% filter(workID == x) %>% pull(groupName)
  newName <- newName[1]
  if (is.na(newName)) {
    newName <- x
  }
  return(newName)
}

# read in corpus and words ####

corpus <- read_csv("TMData/theta.csv")
words <- read_csv("TMData/phi.csv")

# addressing ID bug in corpus

doubleIDs <- corpus %>% group_by(identifier) %>% filter(n() > 1) %>% pull(identifier) %>% unique

corpus <- corpus %>%  
  mutate(identifier = map_chr(identifier, function(x) {
    if (!(x %in% doubleIDs)) {
      x
    } else {paste0(x, ".", randString())}
  })) 


# Topic Visualisations ####

wordColumnNames <- colnames(words) # save topic names for later
colnames(words) <- c("word", paste0("Topic", formatC(1:(length(wordColumnNames)-1), flag = "0", width = 4))) # normalise topic names
topTopic <- names(words[,2:101])[apply(words[,2:101], 1, which.max)] # find topic with biggest phi for each word
words$wordTop <- paste0(words$word, "^", gsub("Topic", "", topTopic)) # add it
words$Top <- topTopic

# choose col palette

col <- createPalette(100, c("#010101", "#ff0000"), M=10000)
col <- sample(col) # change order randomly
names(col) <- names(words[,2:101])
# add top color
words$Col <- col[words$Top]

topicColors <-
  setNames(col, levels(as.factor(paste0(
    "Topic", formatC(1:(length(wordColumnNames) - 1), flag = "0", width = 4)
  ))))

topicColors["remainder"] <- "#A9A9A9"

# normalise corpus ####

max_cores <- detectCores() - 1 
registerDoParallel(cores = max_cores)
normalisedTexts <- foreach(i = corpus$text, .combine = c) %dopar% preprocess_corpus(i)
registerDoSEQ()

thetaWordCounts <- map_int(normalisedTexts, function(x) str_count(x, boundary("word")))
wordcountTibble <- corpus %>% select(identifier) %>% mutate(words = thetaWordCounts)
wordcountTibble <- wordcountTibble %>% mutate(workgroup = map_chr(identifier, function(x) str_split(str_split(x, ":")[[1]][4], "\\.")[[1]][1]),
                           work = map_chr(identifier, function(x) str_split(str_split(x, ":")[[1]][4], "\\.")[[1]][2])) %>%
  group_by(workgroup, work)

# Network Corpus ####

workNodes <- wordcountTibble %>% 
  summarise(words = sum(words), n = n()) %>% 
  mutate(from = workgroup, to = map2_chr(workgroup, work, function(x,y) paste(x,y, sep = "."))) %>% 
  ungroup() %>% 
  select(from, to, words, n)

rootNodes <- workNodes %>% 
  group_by(from) %>% 
  summarise(words = sum(words), n = n()) %>% 
  rename(to = from) %>%
  mutate(from = "OGL") %>%
  select(from, everything())

smallerEdges <- rbind(rootNodes,workNodes)

smallerNodes <- smallerEdges %>% group_by(from) %>% summarise(wordCount = sum(words), nodeCount = sum(n)) %>% rename(id = from)
smallerWorkNodes <- workNodes %>% rename(id = to, wordCount = words, nodeCount = n) %>% select(-from)
smallerNodes <- rbind(smallerNodes, smallerWorkNodes)
smallerNodes <- smallerNodes %>% mutate(group = map_chr(id, function(x) str_split(x, "\\.")[[1]][1])) %>% arrange(id)

nodeGraph <- graph_from_data_frame(smallerEdges, vertices = smallerNodes)

set_graph_style(plot_margin = margin(1,1,1,1))

dendLay <- create_layout(nodeGraph, layout = 'dendrogram', circular = TRUE)

workgroupCol <- createPalette(length(unique(smallerNodes$group)), c("#010101", "#ff0000"), M=10000)
workgroupCol <- sample(workgroupCol)
names(workgroupCol) <- unique(smallerNodes$group)
workgroupCol["OGL"] <- "#A9A9A9"

# ColFlower
ggraph(nodeGraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf, col = group), show.legend = F) + 
  scale_color_manual(values = workgroupCol) +
  theme_void() +
  coord_fixed()

ggsave("corpusViz/colFlower.png")

# Flower
ggraph(nodeGraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf), show.legend = F) + 
  theme_void() +
  coord_fixed()

ggsave("corpusViz/flower.png")

bigNodeCloud <- wordcountTibble %>% ungroup
packing <- circleProgressiveLayout(bigNodeCloud, "words", "area")
packing <- as_tibble(packing)
bigNodeCloud <- bind_cols(bigNodeCloud, packing)

allWorks <- wordcountTibble %>% 
  summarise(nodes = n()) %>% 
  ungroup %>% 
  mutate(work = map2_chr(workgroup, work, function(x,y) paste(x,y,sep = ".")))
packingWorks <- circleProgressiveLayout(allWorks, "nodes", "area")
packingWorks <- as_tibble(packingWorks)
workNodeCloud <- bind_cols(allWorks, packingWorks)

bigNodeCloud <- bigNodeCloud %>% 
  mutate(work = map2_chr(workgroup, work, function(x,y) paste(x,y,sep = ".")))

# 15min to 30min
bigNodeCloud <- bigNodeCloud %>% mutate(workX = map_dbl(work, findX),
                        workY = map2_dbl(work, workX, findY))

workNodeCloud %>%
ggplot() +
  geom_point(aes(x,y,col=workgroup,size = radius), stroke = 0, shape = 16, show.legend = F) + 
  scale_color_manual(values = workgroupCol) +
  scale_size(range = c(0.1,20)) +
  coord_fixed() +
  theme_void()

workNodeCloud %>%
  ggplot() +
  geom_circle(aes(x0=x,y0=y,r=radius),show.legend = F) +
  coord_fixed()

ggsave("corpusViz/workNodesBW.png")

workNodeCloud %>%
  ggplot() +
  geom_circle(aes(x0=x,y0=y,r=radius,fill=workgroup),show.legend = F) +
  scale_fill_manual(values = workgroupCol) +
  coord_fixed()

ggsave("corpusViz/workNodes.png")

bigNodeCloud %>%
ggplot() +
  geom_point(aes(workX,workY,col=workgroup,size = words), stroke = 0, shape = 16, show.legend = F) + 
  # geom_point(aes(x,y,col=workgroup,size = radius), show.legend = F) +
  scale_color_manual(values = workgroupCol) +
  scale_size(range = c(0.1,1)) +
  coord_fixed() +
  theme_void()

ggsave("corpusViz/passageNodes.png")

bigNodeCloud %>%
  ggplot() +
  geom_circle(data = workNodeCloud, mapping = aes(x0=x,y0=y,r=radius), size = 0.1, show.legend = F) +
  geom_point(aes(workX,workY,col=workgroup,size = words), stroke = 0, shape = 16, show.legend = F) + 
  # geom_point(aes(x,y,col=workgroup,size = radius), show.legend = F) +
  scale_color_manual(values = workgroupCol) +
  scale_size(range = c(0.1,1)) +
  coord_fixed() +
  theme_void()

ggsave("corpusViz/passageWorkNodes.png")

wordcountTibble %>% 
  ungroup %>% 
  group_by(workgroup) %>%
  summarise(wordCount = sum(words)) %>% 
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, amount = wordCount, fill = workgroup), stat = "pie", show.legend = F, color = NA) +
  scale_fill_manual(values = workgroupCol) +
  coord_fixed() +
  theme_void()

ggsave("corpusViz/pieCorpus.png")

corpusNodes <- wordcountTibble %>% 
  ungroup %>% 
  group_by(workgroup) %>%
  summarise(wordCount = sum(words)) %>%
  rename(id = workgroup)

corpusEdges <- corpusNodes %>% transmute(from = "OGL",
                                         to = id,
                                         weight = wordCount)

corpusNodes <- rbind(corpusNodes, tibble(id = "OGL", wordCount = sum(corpusEdges$weight)))

corpusGraph <- graph_from_data_frame(corpusEdges, vertices = corpusNodes)

ggraph(as_tbl_graph(corpusGraph), 'treemap', weight = wordCount) + 
  geom_node_tile(aes(fill = name), show.legend = F) +
  scale_fill_manual(values = workgroupCol) +
  coord_fixed()

ggsave("corpusViz/treeCorpus.png")

unset_graph_style()

# back to topics ####

words.in.corpus <- unlist(strsplit(normalisedTexts, " ", fixed = TRUE))
table.words <- sort(table(words.in.corpus), decreasing = T)

wordLists <- list()

for (i in 1:100) {
  topiccol <- as.name(paste0("Topic", formatC(i, flag = "0", width = 4)))
  wordLists[[i]] <- words %>% select(word, !!topiccol) %>% arrange(desc(!!topiccol)) %>% select(word) %>% slice(1:100) %>% pull(word)
}

wordsUnlisted <- unlist(wordLists)
wordsUnlisted <- unique(wordsUnlisted)
wordsImportant <- words %>% filter(word %in% wordsUnlisted)

phiSummary <- wordsImportant %>% 
  gather(topic, phi, Topic0001:Topic0100) %>% 
  select(word, Top, topic, phi) %>% 
  group_by(word, topic) %>% 
  summarise(Top=Top, phi = phi) %>% 
  mutate(phiPer = phi / sum(phi)) 

phiSummary <- phiSummary %>% ungroup() %>% select(word,Top, topic,phiPer) %>% spread(topic, phiPer)
phiSummary <- phiSummary %>% mutate(occurrence = as.numeric(unname(table.words[word])))

termcount <- 20
for (i in 3:(length(words)-2)) {
  print(i - 2)
  filename <- paste0("topicViz/NewTopic", i - 2, ".png")
  tempTop <- paste0("Topic", formatC(i-2, flag = "0", width = 4))
  wordCounts <- phiSummary %>% arrange(.[[i]]) %>% head(n = termcount) %>% pull(occurrence)
  topWordCount <- max(wordCounts) + 3000
  print(topWordCount)
  p <- phiSummary %>%
    mutate(selectedTopic = occurrence * .[[i]]) %>%
    mutate(remainder = occurrence - selectedTopic) %>%
    mutate(word = factor(word, levels=unique(word[order(selectedTopic)]), ordered=TRUE)) %>%
    arrange(desc(selectedTopic)) %>%
    head(n = termcount) %>%
    select(word, selectedTopic, remainder, occurrence, Top) %>%
    gather(type, count, selectedTopic:remainder) %>%
    mutate(Top = ifelse(type == "remainder", "remainder", Top)) %>%
    ggplot(aes(x = word, y = count, fill = Top)) + 
    geom_bar(stat="identity", show.legend = T) +
    scale_fill_manual(breaks = names(topicColors)[-which(names(topicColors) == "remainder")], 
                      values = topicColors) +
    theme_bw() + 
    theme(legend.position = c(.99, .99),
          legend.background = element_rect(linetype = "solid", colour = "black"),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          text=element_text(family="Times New Roman"),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=12),
          axis.text.y = element_text(size=14, color = "black"),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18, colour = topicColors[paste0("Topic", formatC(i-2, flag = "0", width = 4))]),
          plot.subtitle = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip()
  p + labs(title = paste0("Topic ", i-2), subtitle = paste("Top", termcount,"Terms")) + 
      labs(x = "Terms", y = "Term occurrence in corpus (coloured for calculated occurrence in topic)") +
      expand_limits(y = topWordCount) +
      guides(fill = guide_legend(title = "Term Most Common in:"))
  ggsave(filename)
}

# just the theta and tm ####
thetaData <- corpus %>% select(identifier, 4:length(corpus))
colnames(thetaData) <-
  c("identifier", paste0("Topic", formatC(
    1:(length(wordColumnNames) - 1), flag = "0", width = 4
  )))

thetasums <-
  thetaData %>% select(starts_with("Topic")) %>% summarise_all(sum)
thetasum <- sum(thetasums)
thetaweights <- thetasums %>%
  select(starts_with("Topic")) %>%
  mutate_all(function(x) {
    x / thetasum
  }) %>%
  slice(1) %>%
  unlist()

pwords <- words %>%
  mutate(!!!imap(thetaweights, function(weight, name, data) {
    data[[name]] * weight
  }, data = words)) %>%
  mutate(pword = rowSums(select(., Topic0001:Topic0100)))

lambdawords <- words %>%
  mutate(pword = pwords$pword) %>%
  mutate_at(names(thetaweights), ~ (0.6 * log(.) + 0.4 * log(. / pword))) %>%
  rename_at(names(thetaweights), ~ paste0("lambda_", .)) %>%
  select(word, pword, starts_with("lambda"))

enrichedWords <- inner_join(words, lambdawords, by = "word")
topicVector <-
  thetaData %>% select(starts_with("Topic")) %>% colnames()

count <- 1
for (i in topicVector) {
  filename <- paste0("topicRankViz/", i, ".png")
  p <- enrichedWords %>%
    mutate(
      rankLambda = rank(desc(!!sym(
        paste0("lambda_", i)
      )), ties.method = "min"),
      rankPhi = rank(desc(!!sym(i)), ties.method = "min")
    ) %>%
    filter(rankLambda <= 30) %>%
    filter(rankPhi <= 30) %>%
    ggplot() +
    geom_point(aes(rankLambda, rankPhi, color = Top), size = 2) +
    geom_label_repel(aes(rankLambda, rankPhi, label = word, fill = Top), size = 6, show.legend = F) +
    scale_color_manual(values = topicColors) +
    scale_fill_manual(values = topicColors) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_line(arrow = arrow(
        ends = "last", length = unit(3, 'mm')
      )),
      axis.line.x = element_line(arrow = arrow(
        ends = "last", length = unit(3, 'mm')
      )),
      legend.background = element_rect(linetype = "solid", colour = "black"),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.key = element_rect(fill = NA),
      text = element_text(family = "Times New Roman", size = 20),
      plot.title = element_text(size = 18, colour = topicColors[paste0("Topic", formatC(count, flag = "0", width = 4))])
    ) +
    labs(x = "λ-Ranking (descending)", y = "φ-Ranking (descending)") +
    labs(color = "Token most\ncommon for:") +
    labs(title = paste0(i)) +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 5))) +
    coord_fixed()
  
  ggsave(
    plot = p,
    file = filename,
    type = "cairo-png",
    bg = "white",
    width = 12,
    height = 8,
    dpi = 300
  )
  count <- count + 1
}

# Polar Topic Viz ####

count <- 1
for (i in topicVector) {
  filename <- paste0("topicRankPolarViz/", i, ".png")
  p <- enrichedWords %>%
    mutate(
      rankLambda = rank(desc(!!sym(
        paste0("lambda_", i)
      )), ties.method = "min"),
      rankPhi = rank(desc(!!sym(i)), ties.method = "min")
    ) %>%
    mutate(r = sqrt(rankLambda^2 + rankPhi^2),
           theta = rankLambda / rankPhi) %>%
    filter(r < 30) %>%
    ggplot() +
    geom_hline(yintercept = seq(0, 30, by = 10), colour = "darkgrey", size = 0.35) +
    geom_point(aes(0,0), colour = "darkgrey", size = 5, shape = 3) +
    geom_point(aes(theta, r, color = Top), size = 2) +
    geom_label_repel(aes(theta, r, label = word, fill = Top, size = 1 / r), show.legend = F) +
    scale_color_manual(values = topicColors) +
    scale_fill_manual(values = topicColors) +
    scale_size_continuous(range = c(2,10)) +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title =element_blank(),
      legend.background = element_rect(linetype = "solid", colour = "black"),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.key = element_rect(fill = NA),
      plot.title = element_text(size = 18, colour = topicColors[i])
    ) +
    labs(color = "Token most\ncommon for:") +
    labs(title = paste0(i), subtitle = "Lower radiant distance and bigger font size represent higher combined λφ-rank.") +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 5))) +
    coord_polar()
  
  ggsave(
    plot = p,
    file = filename,
    type = "cairo-png",
    bg = "white",
    width = 12,
    height = 8,
    dpi = 300
  )
  count <- count + 1
}

# Corpus Visualisations II ####

corpus <- mutate(corpus, WorkID = sapply(strsplit(corpus$identifier, ":"), "[[", 4))
corpus <- mutate(corpus, AuthorID = sapply(strsplit(WorkID, ".", fixed = T), "[[",1))

topicCorpus <- corpus %>%
  mutate(WordCount = unname(sapply((corpus %>% pull(text)), function(x){str_count(x, "\\S+")}))) %>%
  select(-(X1:text)) %>%
  select(-WorkID) %>%
  select(AuthorID, WordCount, everything())
colnames(topicCorpus) <- c("AuthorID", "MeanWordCount", names(topicColors)[-which(names(topicColors) == "remainder")])

# calculate means
mean39 <- topicCorpus %>% pull(Topic0039) %>% mean()
mean29 <- topicCorpus %>% pull(Topic0029) %>% mean()
mean13 <- topicCorpus %>% pull(Topic0013) %>% mean()
mean24 <- topicCorpus %>% pull(Topic0024) %>% mean()
mean47 <- topicCorpus %>% pull(Topic0047) %>% mean()
mean85 <- topicCorpus %>% pull(Topic0085) %>% mean()
# for nomalisation
mean9 <- topicCorpus %>% pull(Topic0009) %>% mean()

# calculate sds

sd39 <- topicCorpus %>% pull(Topic0039) %>% sd()
sd29 <- topicCorpus %>% pull(Topic0029) %>% sd()
sd13 <- topicCorpus %>% pull(Topic0013) %>% sd()
sd24 <- topicCorpus %>% pull(Topic0024) %>% sd()
sd47 <- topicCorpus %>% pull(Topic0047) %>% sd()
sd85 <- topicCorpus %>% pull(Topic0085) %>% sd()
# for nomalisation
sd9 <- topicCorpus %>% pull(Topic0009) %>% mean()

meanWord <- topicCorpus %>% pull(MeanWordCount) %>% mean()

genremap <-read_csv("metadata/genremap.csv")

# max 13/29 39/24 philosopy score
# takes a bit
topicCorpus2 <- topicCorpus %>% 
  mutate(Topic13_29 = ifelse(Topic0013 / mean13 >= Topic0029 / mean29, log2(Topic0013 / mean13), log2(Topic0029 / mean29)), 
         Topic24_39 = ifelse(Topic0039 / mean39 >= Topic0024 / mean24, log2(Topic0039 / mean39), log2(Topic0024 / mean24)),
         AuthorID = map_chr(topicCorpus$AuthorID, getName),
         Genre = map_chr(topicCorpus$AuthorID, getGenre),
         Philosophy = ifelse(Genre == "philosophy", "true", "false")) 

# uncorrelated? ####
topicCorpus %>% 
  select(AuthorID, Topic0029, Topic0039) %>% 
  filter(Topic0029 >= 0.1 & Topic0039 >= 0.1) %>%
  ggplot(aes(x=Topic0029,y=Topic0039)) + 
  geom_hex() 

topicCorpus %>% 
  select(AuthorID, Topic0024, Topic0039) %>% 
  filter(Topic0024 >= 0.1 & Topic0039 >= 0.1) %>%
  ggplot(aes(x=Topic0024,y=Topic0039)) + 
  geom_hex() 

modelA <- read_csv("TMData/theta.csv")
topicNumber <- length(modelA) - 3
matColNamesA <- paste0("A_Topic", formatC(1:topicNumber, flag = "0", width = 4))
matrixA <- modelA %>% select(-c(1:3)) %>% as.matrix
colnames(matrixA) <- matColNamesA

vizName <- "internalTopicCorrelation/"
dir.create(vizName)

corMatA <- cor(matrixA, matrixA)
melted_cormatA <- melt(corMatA) %>% as_tibble

melted_cormatA %>% transmute(
    `Model A` = mapply(function(x)
      gsub("A_", "", x), Var1),
    `Model B` = mapply(function(x)
      gsub("A_", "", x), Var2),
    `Pearson coefficient` = value
  ) %>%
  ggplot() +
  geom_tile(aes(x = `Model A`, y = `Model B`, fill = `Pearson coefficient`), col = "black") +
  scale_fill_scico(palette = "vik", direction = -1, limits=c(-1,1)) +
  theme(
    legend.background = element_rect(linetype = "solid", colour = "grey"),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    text = element_text(family="Times New Roman", size = 16),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  labs(
    title = "Correlation of Topics in the Model"
  ) +
  labs(x = "Topics", y = "Topics")

ggsave("internalTopicCorrelation/correlation.png", scale = 3)

# Topic0013 (reality & living beings) positive correlation with Topic0047 (black & white, opposites)
# Topic0024 (good & bad) positive correlation with Topic0085 (pleasure & pain)
# Topic0029 (philosophical argument) no correlation
# Topic0039 (virtue) no correlation

ggsave("internalTopicCorrelation/correlation.png", scale = 3)

melted_cormatA %>% transmute(
  `Model A` = mapply(function(x)
    gsub("A_", "", x), Var1),
  `Model B` = mapply(function(x)
    gsub("A_", "", x), Var2),
  `Pearson coefficient` = value
) %>%
  filter(`Model A` %in% c("Topic0013", 
                          "Topic0047", 
                          "Topic0024",
                          "Topic0039", 
                          "Topic0029",
                          "Topic0085")) %>%
  filter(`Pearson coefficient` > 0.1 | `Pearson coefficient` < -0.1) %>%
  ggplot() +
  geom_tile(aes(x = `Model A`, y = `Model B`, fill = `Pearson coefficient`), col = "black") +
  scale_fill_scico(palette = "vik", direction = -1, limits=c(-1,1)) +
  theme(
    legend.background = element_rect(linetype = "solid", colour = "white"),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    text = element_text(family="Times New Roman", size = 16),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  labs(
    title = "Correlation of Topics in the Model"
  ) +
  labs(x = "Topics", y = "Topics")

ggsave("internalTopicCorrelation/correlationZoom.png", scale = 1)

# summarise workgroups
topicCorpus <- topicCorpus %>%
  group_by(AuthorID) %>%
  summarise_all(funs(n(), mean, sd, median)) %>%
  mutate(NodeCount = Topic0001_n) %>%
  select(-ends_with("_n"))

# catalog data
catalog <- read_csv("metadata/generatedCatalogData.csv")
catalog <- catalog %>% mutate(workID = map_chr(catalog$urn, extractAuthorID))

authorMap <- map_chr(topicCorpus$AuthorID, getAuthor)
philosopherMap <- tibble(id = topicCorpus$AuthorID, name = authorMap)

genresNames <- unique(genremap$mainGenre)

# Plato and Arisdtotle with Topic 9 ####

myStyle <- theme_bw() + 
  theme(text=element_text(family="serif"),
        legend.background = element_rect(linetype = "solid", colour = "black"),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text = element_text(size=12),
        # axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 18))

ArTotal <- topicCorpus2 %>% 
  filter(AuthorID %in% c("Aristotle")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  group_by(AuthorID, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  pull(Theta) %>% sum

PlaTotal <- topicCorpus2 %>% 
  filter(AuthorID %in% c("Plato")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  group_by(AuthorID, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  pull(Theta) %>% sum

topicCorpus2 %>% 
  filter(AuthorID %in% c("Aristotle", "Plato")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  rename(Author = AuthorID) %>%
  group_by(Author, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  mutate(`Topic in %`= ifelse(Author == "Plato", Theta / PlaTotal * 100, Theta/ArTotal  * 100)) %>% 
  mutate(Label = ifelse(`Topic in %` > 5, Topic, NA)) %>%
  ggplot(aes(Author, `Topic in %`)) +
  geom_col(aes(fill = Topic), show.legend = F) +
  geom_label(aes(label = Label, group = Topic), size = 3, position = position_stack(vjust = .5)) +
  scale_fill_manual(values = topicColors) +
  myStyle

ggsave("PlatoAristotle/comparison.png")

# Plato and Arisdtotle without Topic 9 ####

ArTotal2 <- topicCorpus2 %>% 
  filter(AuthorID %in% c("Aristotle")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  filter(Topic != "Topic0009") %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  group_by(AuthorID, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  pull(Theta) %>% sum

PlaTotal2 <- topicCorpus2 %>% 
  filter(AuthorID %in% c("Plato")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  filter(Topic != "Topic0009") %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  group_by(AuthorID, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  pull(Theta) %>% sum

topicCorpus2 %>% 
  filter(AuthorID %in% c("Aristotle", "Plato")) %>%
  select(1:102) %>%
  gather(Topic, Theta, 3:102) %>%
  filter(Topic != "Topic0009") %>%
  mutate(Theta = Theta / MeanWordCount) %>%
  rename(Author = AuthorID) %>%
  group_by(Author, Topic) %>%
  summarise(Theta = mean(Theta)) %>%
  mutate(`Topic in %`= ifelse(Author == "Plato", Theta / PlaTotal2 * 100, Theta/ArTotal2  * 100)) %>% 
  mutate(Label = ifelse(`Topic in %` > 5, Topic, NA)) %>%
  ggplot(aes(Author, `Topic in %`)) +
  geom_col(aes(fill = Topic), show.legend = F) +
  geom_label(aes(label = Label, group = Topic), size = 3, position = position_stack(vjust = .5)) +
  scale_fill_manual(values = topicColors) +
  myStyle

ggsave("PlatoAristotle/comparisonNoTopic0009.png")

# Trace Topic0029 and Topic0039 through corpus ####

topicCorpus2 %>% 
  mutate(Author = str_wrap(AuthorID, 15)) %>% 
  ggplot(aes(Author, Topic0039)) + 
  geom_jitter(aes(col = AuthorID), show.legend = F) + 
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()

ggsave(filename = "topictrendsCorpus/topic0039.png", width = 10, height = 80, limitsize = F)

topicCorpus2 %>% 
  mutate(Author = str_wrap(AuthorID, 15)) %>% 
  ggplot(aes(Author, Topic0029)) + 
  geom_jitter(aes(col = AuthorID), show.legend = F) + 
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()

ggsave(filename = "topictrendsCorpus/topic0029.png", width = 10, height = 80, limitsize = F)

# genre vizs ####

genreVector <- topicCorpus2 %>% pull(Genre) %>% unique() %>% sort(decreasing = T)
genreCol <- palette36.colors(length(genreVector))
names(genreCol) <- genreVector
genreCount <- topicCorpus2 %>% pull(Genre) %>% table
genreCount <- setNames(as.numeric(genreCount), names(genreCount))

genrePer <- function(x,y) {
  paste0(format((x / genreCount[y]) * 100, digits = 2L), "%")
}

for (i in topicVector) {
  filen <- paste0("topictrendsJitter/Genre", i, ".png")
  p <- topicCorpus2 %>% 
    ggplot(aes_string("Genre", i)) + 
    geom_jitter(aes(col = Genre), show.legend = F) + 
    scale_x_discrete(drop=FALSE) +
    scale_color_manual(values = genreCol) +
    theme(
      text=element_text(family="serif"),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) +
    coord_flip() 
  ggsave(plot = p, filename = filen)
}

for (i in topicVector) {
  filen <- paste0("topictrendsViolin/Genre", i, ".png")
  p <- topicCorpus2 %>% 
    ggplot(aes_string("Genre", i)) + 
    geom_violin(aes(col = Genre), show.legend = F) + 
    scale_x_discrete(drop=FALSE) +
    scale_color_manual(values = genreCol) +
    theme(
      text=element_text(family="serif"),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) +
    coord_flip() 
  ggsave(plot = p, filename = filen)
}

for (i in topicVector) {
  filen <- paste0("topictrendsBoxplot/Genre", i, ".png")
  tempTable <- topicCorpus2 %>% filter(.data[[i]] > 0.1) %>% group_by(Genre) %>% summarise(count = n(), max = max(.data[[i]], na.rm = T))
  p <- topicCorpus2 %>% 
    filter(.data[[i]] > 0.1) %>%
    mutate(Genre = factor(Genre, levels = genreVector)) %>%
    ggplot(aes_string("Genre", i)) + 
    geom_boxplot(aes(col = Genre), varwidth = TRUE, show.legend = F) + 
    geom_text(data = tempTable, 
              mapping = aes(factor(Genre, levels = genreVector), 
                            max, label = paste0("n=", format(count, big.mark = ","), "\n(", map2_chr(count,Genre,genrePer),")"), 
                            col = Genre), 
              nudge_y = .05, show.legend = F, size = 2, lineheight = .7) +
    scale_x_discrete(drop=FALSE) +
    scale_color_manual(values = genreCol) +
    theme(
      text=element_text(family="serif"),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) +
    coord_flip(ylim = c(0, 1.2)) 
  ggsave(plot = p, filename = filen)
}

# just for Topic0029 and Topic0039

corpus2939 <- topicCorpus2 %>% 
  filter(Topic0029 > 0.1 | Topic0039 > 0.1) %>%
  select(Genre,Topic0029,Topic0039) %>%
  mutate(maxVal = ifelse(Topic0029 > Topic0039, Topic0029, Topic0039)) %>%
  pivot_longer(starts_with("Topic"), names_to = "Topic", values_to = "θ")
  
p <- corpus2939 %>% 
  mutate(Genre = factor(Genre, levels = genreVector)) %>%
  ggplot(aes(Genre, `θ`)) + 
  geom_boxplot(aes(col = Topic), varwidth = TRUE) + 
  scale_x_discrete(drop=FALSE) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()

ggsave(p, filename = "topictrendsCorpus/topic002939BoxPlot.png")

corpus2939 <- topicCorpus2 %>% 
  select(Genre,Topic0029,Topic0039) %>%
  mutate(maxVal = ifelse(Topic0029 > Topic0039, Topic0029, Topic0039)) %>%
  pivot_longer(starts_with("Topic"), names_to = "Topic", values_to = "θ")

p <- corpus2939 %>% 
  mutate(Genre = factor(Genre, levels = genreVector)) %>%
  ggplot(aes(Genre, θ)) + 
  geom_jitter(aes(col = Topic), size = .1) + 
  scale_x_discrete(drop=FALSE) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()

ggsave(p, filename = "topictrendsCorpus/topic002939Jitter.png")

# testing different scores ####

philTopics <- topicCorpus %>% select(AuthorID, 
                                     Topic0009_mean,
                                     Topic0013_mean, 
                                     Topic0024_mean, 
                                     Topic0029_mean, 
                                     Topic0039_mean, 
                                     Topic0047_mean, 
                                     Topic0085_mean, 
                                     MeanWordCount_mean, 
                                     NodeCount)

philTopics <- philTopics %>% 
  mutate(AuthorID = map_chr(AuthorID, getName)) %>%
  mutate(Genre = map_chr(topicCorpus$AuthorID, getGenre)) %>%
  mutate(Philosophy = ifelse(Genre == "philosophy", T, F)) %>%
  mutate(meanTopic0013 = Topic0013_mean / mean13,
         meanTopic0024 = Topic0024_mean / mean24,
         meanTopic0029 = Topic0029_mean / mean29,
         meanTopic0039 = Topic0039_mean / mean39,
         meanTopic0047 = Topic0047_mean / mean47,
         meanTopic0085 = Topic0085_mean / mean85,
         meanTopic0009 = Topic0009_mean / mean9) %>%
  select(AuthorID, Genre, starts_with("mean"), MeanWordCount_mean, NodeCount, Philosophy)

numPhil <- philTopics %>% filter(Genre == "philosophy") %>% pull(Genre) %>% length()
numWorkgroup <- philTopics %>% pull(Genre) %>% length()

# precision, recall, fmeasure ####

library(grid)
library(gridExtra)

testname_vector <- vector(mode = "character")
precision_vector <- vector(mode = "numeric")
recall_vector <- vector(mode = "numeric")
fmeasure_vector <- vector(mode = "numeric")

# Topic0013 ####

testTable <- philTopics %>%
  filter(meanTopic0013 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t13],mu[t13])>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# Topic0047 ####

testTable <- philTopics %>%
  filter(meanTopic0047 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t47],mu[t47])>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# 47, 9 ####

testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0047^2 + meanTopic0009^2),
         theta = meanTopic0047 / meanTopic0009) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t47]^2+bar(theta)[t9]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# 13, 47 ####

testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2),
         theta = meanTopic0013 / meanTopic0047) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# (13, 47), (24, 85) ####
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2 + meanTopic0024^2 + meanTopic0085^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2+bar(theta)[t24]^2+bar(theta)[t85]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# philscore ####

testTable <- topicCorpus %>% 
  mutate(Genre = map_chr(AuthorID, getGenre)) %>%
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  filter(Topic13_29 + Topic24_39 > 0) %>%
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, 'italic(max)~bgroup("(",list(italic(log)[2](frac(bar(theta)[t29],mu[t29])),italic(log)[2](frac(bar(theta)[t13],mu[t13]))),")")+italic(max)~bgroup("(",list(italic(log)[2](frac(bar(theta)[t39],mu[t39])),italic(log)[2](frac(bar(theta)[t24],mu[t24]))),")")>0')
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# results ####

results <- tibble(Condition = testname_vector,
       Precision = format(precision_vector, digits = 2),
       Recall = format(recall_vector, digits = 2),
       `F1 Score` = format(fmeasure_vector, digits = 2)) %>%
  arrange(desc(`F1 Score`))

grid.newpage()
tt <- ttheme_default(core=list(fg_params=list(parse=TRUE)))
tg_results <- tableGrob(d = results, rows = NULL, theme=tt)

pdf("testresults.pdf", height = 11, width = 8.5, paper = "a4")
grid.draw(tg_results)
dev.off()

# philScore Plots ####

topicCorpus %>% 
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  select(AuthorID, Topic13_29, Topic24_39, MeanWordCount_mean, NodeCount) %>% 
  mutate(AuthorID = map_chr(topicCorpus$AuthorID, getName)) %>%
  mutate(Genre = map_chr(topicCorpus$AuthorID, getGenre)) %>%
  mutate(Philosophy = ifelse(Genre == "philosophy", T, F)) %>%
  mutate(Label = ifelse(Genre == "philosophy", AuthorID, "")) %>%
  # mutate(Label = ifelse((Topic13_29 > 1 | Topic24_39 > 1), AuthorID, "")) %>%
  # filter(Philosopher == TRUE) %>%
  ggplot(aes(x=Topic13_29,y=Topic24_39)) +
  geom_abline(slope = -1) +
  geom_hline(yintercept=0, linetype = 3) +
  geom_vline(xintercept=0, linetype = 3) +
  geom_point(aes(shape = Philosophy, color = Philosophy), size = 2) +
  geom_label_repel(aes(label = Label)) +
  labs(title = 'Authors\' "Philosophicalness" Scores', x = '"Scientific Inquiry" Score', y = '"Good & Virtue" Score') +
  theme_bw() + 
  theme(text=element_text(family="serif"),
        legend.background = element_rect(linetype = "solid", colour = "black"),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text = element_text(size=12),
        # axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_y_continuous(expand = c(0.02,0.02))

ggsave("philScorePlots/philscorelabels.png", scale = 1.5)


# philScore Passage ####

p <- topicCorpus2 %>% 
  mutate(PhilScore = Topic13_29 + Topic24_39) %>%
  ggplot(aes(Genre, PhilScore)) + 
  geom_jitter(aes(col = Genre), show.legend = F, alpha = .5, size = .2) + 
  geom_hline(yintercept=0, linetype = 3) +
  scale_x_discrete(drop=FALSE) +
  scale_color_manual(values = genreCol) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip() 
ggsave(plot = p, filename = "philScorePlots/philScorePassage.png")

topicCorpus %>% 
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  select(AuthorID, Topic13_29, Topic24_39, MeanWordCount_mean, NodeCount) %>% 
  mutate(AuthorID = map_chr(topicCorpus$AuthorID, getName)) %>%
  mutate(Genre = map_chr(topicCorpus$AuthorID, getGenre)) %>%
  mutate(Philosophy = ifelse(Genre == "philosophy", T, F)) %>%
  mutate(Label = ifelse(Genre == "philosophy", AuthorID, "")) %>%
  mutate(PhilScore = Topic13_29 + Topic24_39) %>%
  ggplot(aes(Genre, PhilScore)) + 
  geom_boxplot(aes(col = Genre), show.legend = F) + 
  geom_hline(yintercept=0, linetype = 3) +
  scale_x_discrete(drop=FALSE) +
  scale_color_manual(values = genreCol) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip() 
ggsave(filename = "philScorePlots/philScoreWG.png")

# Top100 Passage ####

p <- topicCorpus2 %>% 
  mutate(PhilScore = Topic13_29 + Topic24_39) %>%
  arrange(desc(PhilScore)) %>%
  slice(1:100) %>%
  ggplot() +
  geom_bar(aes(AuthorID, fill = Genre)) +
  scale_fill_manual(values = genreCol) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()
ggsave(plot = p, filename = "philScorePlots/philScore100.png")

# Top200 Passage ####

p <- topicCorpus2 %>% 
  mutate(PhilScore = Topic13_29 + Topic24_39) %>%
  arrange(desc(PhilScore)) %>%
  slice(1:200) %>%
  ggplot() +
  geom_bar(aes(AuthorID, fill = Genre)) +
  scale_fill_manual(values = genreCol) +
  theme(
    text=element_text(family="serif"),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  coord_flip()
ggsave(plot = p, filename = "philScorePlots/philScore200.png")

# no philosophy philosophy ####

topicCorpus %>% 
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  select(AuthorID, Topic13_29, Topic24_39, MeanWordCount_mean, NodeCount) %>% 
  mutate(philscore = Topic13_29 + Topic24_39) %>%
  mutate(AuthorID = map_chr(topicCorpus$AuthorID, getName)) %>%
  mutate(Genre = map_chr(topicCorpus$AuthorID, getGenre)) %>%
  mutate(Philosophy = ifelse(Genre == "philosophy", T, F)) %>%
  mutate(Label = ifelse(Genre != "philosophy" & philscore > 0, AuthorID, "")) %>%
  mutate(Genre = ifelse(Genre %in% c("unknown", "philosophy") | philscore <= 0, "excluded", Genre)) %>%
  filter(Genre != "excluded") %>%
  # mutate(Label = "") %>%
  ggplot(aes(x=Topic13_29,y=Topic24_39)) +
  geom_abline(slope = -1) +
  geom_hline(yintercept=0, linetype = 3) +
  geom_vline(xintercept=0, linetype = 3) +
  geom_point(aes(color = Genre), size = 2) +
  # geom_label_repel(aes(label = Label)) +
  labs(title = 'Authors\' "Philosophicalness" Scores', x = '"Scientific Inquiry" Score', y = '"Good & Virtue" Score') +
  scale_color_manual(values = genreCol) +
  theme_bw() + 
  theme(text=element_text(family="serif"),
        legend.background = element_rect(linetype = "solid", colour = "black"),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text = element_text(size=12),
        # axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  coord_cartesian(ylim = c(-6,6), xlim = c(-5,5))

ggsave("philScorePlots/philscorelabelsNoPhil.png", scale = 1)

# Works example ####

topicCorpus3 <- corpus %>%
  mutate(WordCount = unname(sapply((corpus %>% pull(text)), function(x){str_count(x, "\\S+")}))) %>%
  select(-(X1:text)) %>%
  select(AuthorID, WorkID, WordCount, everything())
colnames(topicCorpus3) <- c("AuthorID", "WorkID", "MeanWordCount", names(topicColors)[-which(names(topicColors) == "remainder")])

# for (i in names(topicColors)[-which(names(topicColors) == "remainder")]) {
#   index <- which(colnames(topicCorpus3) == i)
#  topicCorpus3 <- topicCorpus3 %>% mutate(!!i :=  topicCorpus3[,index] %>% pull / MeanWordCount)
#}

# topicCorpus3 <- topicCorpus3 %>% select(-MeanWordCount)

topicCorpus3 <- topicCorpus3 %>%
  group_by(AuthorID, WorkID) %>%
  summarise_all(funs(n(), mean, sum)) %>%
  mutate(NodeCount = Topic0001_n, WordCount = MeanWordCount_sum) %>%
  select(-ends_with("_n"))

topicCorpus3 <- topicCorpus3 %>% 
  select(AuthorID, WorkID, NodeCount, WordCount, everything()) %>%
  ungroup

topicCorpus3 %>% 
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  select(AuthorID, Topic13_29, Topic24_39, NodeCount, WorkID) %>% 
  mutate(AuthorID = map_chr(topicCorpus3$AuthorID, getName)) %>%
  filter(AuthorID %in% c("Plato")) %>%
  ggplot(aes(x=Topic13_29,y=Topic24_39)) +
  geom_abline(slope = -1) +
  geom_hline(yintercept=0, linetype = 3) +
  geom_vline(xintercept=0, linetype = 3) +
  geom_point(size = 2) +
  geom_label_repel(aes(label = WorkID)) +
  labs(title = 'Authors\' "Philosophicalness" Scores', x = '"Scientific Inquiry" Score', y = '"Good & Virtue" Score') +
  theme_bw() + 
  theme(text=element_text(family="serif"),
        legend.background = element_rect(linetype = "solid", colour = "black"),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text = element_text(size=12),
        # axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_y_continuous(expand = c(0.02,0.02))

ggsave("philScorePlots/PlatoWorks.png", scale = 2)

# read meta ####
worksmeta <- read_csv("metadata//WorksMetadata.csv")
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

worksmeta <- worksmeta %>% 
  mutate(OCDWork = map_chr(OCDWork, simpleCap),
         OCDAuthor = map_chr(OCDAuthor, simpleCap))

worksmeta <- worksmeta %>%
  mutate(OCDWork = gsub(" ", "", worksmeta$OCDWork),
         OCDAuthor = gsub(" ", "", worksmeta$OCDAuthor),
         ShortTitle = paste(OCDAuthor, OCDWork, sep = "_"))

pullShortTitle <- function(x) {
  search <- strsplit(x, ".", fixed = T) %>% unlist
  search <- paste(search[1:2],collapse = "_")
  result <- worksmeta %>% filter(ShortID == search) %>% pull(ShortTitle)
  if (length(result) == 0) {
    x
  } else {gsub("_", "", result, fixed = T)}
}

topicCorpus3 %>% 
  mutate(Topic13_29 = ifelse(Topic0013_mean / mean13 >= Topic0029_mean / mean29, log2(Topic0013_mean / mean13), log2(Topic0029_mean / mean29))) %>% 
  mutate(Topic24_39 = ifelse(Topic0039_mean / mean39 >= Topic0024_mean / mean24, log2(Topic0039_mean / mean39), log2(Topic0024_mean / mean24))) %>% 
  select(AuthorID, Topic13_29, Topic24_39, NodeCount, WorkID, WordCount) %>% 
  mutate(AuthorID = map_chr(AuthorID, getName)) %>%
  mutate(WorkID = map_chr(WorkID, pullShortTitle)) %>%
  mutate(PhilScore = Topic13_29 + Topic24_39) %>%
  filter(AuthorID %in% c("Plato")) %>%
  ggplot(aes(x=Topic13_29,y=Topic24_39)) +
  geom_abline(slope = -1) +
  geom_hline(yintercept=0, linetype = 3) +
  geom_vline(xintercept=0, linetype = 3) +
  geom_point(aes(size = WordCount, col=PhilScore)) +
  geom_label_repel(aes(label = WorkID)) +
  labs(title = 'Authors\' "Philosophicalness" Scores', x = '"Scientific Inquiry" Score', y = '"Good & Virtue" Score') +
  theme_bw() + 
  theme(text=element_text(family="serif"),
        legend.background = element_rect(linetype = "solid", colour = "black"),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text = element_text(size=12),
        # axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_color_viridis_c() +
  scale_y_continuous(expand = c(0.02,0.02)) +
  coord_cartesian(xlim = c(-6,6), ylim = c(-5,5))

ggsave("philScorePlots/PlatoWorksTitle.png", scale = 1.5)

# tsne vis ####

library(Rtsne)
normalisedMatrix <- topicCorpus %>% 
  select(AuthorID, ends_with("mean")) %>% 
  select(-c(1:2)) 

datamatrix <- as.matrix(normalisedMatrix)
rownames(datamatrix) <- topicCorpus$AuthorID

set.seed(9)
tsne_model_1 <- Rtsne(datamatrix, initial_dims = 100,  check_duplicates=FALSE, pca=T, perplexity=10, theta=0.0, dims=2)
tsneMat <- tsne_model_1$Y

rownames(tsneMat) <- topicCorpus$AuthorID
coordinates <- as_tibble(tsneMat)
coordinates$AuthorID <- topicCorpus$AuthorID


coordinates %>% 
  mutate(AuthorID = map_chr(topicCorpus$AuthorID, getName)) %>%
  mutate(Genre = map_chr(topicCorpus$AuthorID, getGenre)) %>%
  ggplot() +
  geom_point(aes(x = V1, y = V2, colour = Genre), show.legend = F) +
  geom_label_repel(aes(x = V1, y = V2, colour = Genre, label = Genre), show.legend = F, box.padding = 0.1, fill = "whitesmoke")  +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "darkslategrey"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank())

ggsave("tsneViz/tsneGenre.png", scale = 2)


