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

# Topic0024 ####

testTable <- philTopics %>%
  filter(meanTopic0024 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t24],mu[t24])>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# Topic0085 ####

testTable <- philTopics %>%
  filter(meanTopic0085 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t85],mu[t85]>1)")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# Topic0029 ####

testTable <- philTopics %>%
  filter(meanTopic0029 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t29],mu[t29])>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# Topic0039 ####

testTable <- philTopics %>%
  filter(meanTopic0039 > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "frac(bar(theta)[t39],mu[t39])>1")
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

# 24, 85 ####

testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0024^2 + meanTopic0085^2),
         theta = meanTopic0024 / meanTopic0085) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t24]^2+bar(theta)[t85]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# 29, 39 ####

testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0029^2 + meanTopic0039^2),
         theta = meanTopic0029 / meanTopic0039) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t29]^2+bar(theta)[t39]^2)>1")
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

# (13, 47), (29, 39) ####
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2 + meanTopic0029^2 + meanTopic0039^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2+bar(theta)[t29]^2+bar(theta)[t39]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# (13, 29), (24, 39) ####
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0029^2 + meanTopic0024^2 + meanTopic0039^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t29]^2+bar(theta)[t24]^2+bar(theta)[t39]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)


# (13, 47), (24, 85), 39 ####
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2 + meanTopic0024^2 + meanTopic0085^2 + meanTopic0039^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2+bar(theta)[t24]^2+bar(theta)[t85]^2+bar(theta)[t39]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# (13, 47), (24, 85), 29 ####
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2 + meanTopic0024^2 + meanTopic0085^2 + meanTopic0029^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2+bar(theta)[t24]^2+bar(theta)[t85]^2+bar(theta)[t29]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# (13, 47), (24, 85), 39, 29
testTable <- philTopics %>%
  mutate(r = sqrt(meanTopic0013^2 + meanTopic0047^2 + meanTopic0024^2 + meanTopic0085^2 + meanTopic0029^2 + meanTopic0029^2)) %>%
  filter(r > 1) %>% 
  pull(Genre) %>%
  table()

precision <- testTable["philosophy"] / sum(testTable)
recall <- testTable["philosophy"] / numPhil
fmeasure <- 1/mean(1/c(precision,recall))

testname_vector <- c(testname_vector, "sqrt(bar(theta)[t13]^2+bar(theta)[t47]^2+bar(theta)[t24]^2+bar(theta)[t85]^2+bar(theta)[t29]^2+bar(theta)[t39]^2)>1")
precision_vector <- c(precision_vector, precision)
recall_vector <- c(recall_vector, recall)
fmeasure_vector <- c(fmeasure_vector, fmeasure)

# baseline score ####

# testname_vector <- vector(mode = "character")
# precision_vector <- vector(mode = "numeric")
# recall_vector <- vector(mode = "numeric")
# fmeasure_vector <- vector(mode = "numeric")

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