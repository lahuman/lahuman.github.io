#! /usr/bin/env Rscript
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")

# get arguments of cli
args <- commandArgs(trailingOnly = TRUE);

# arguments to JSON
searching <- fromJSON(args)$searching


library(rvest)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library(dplyr)
library(networkD3)
library(rjson)

useNIADic()


ko_words <- function(doc) {
  d <- as.character(doc)
  pos <- unlist(SimplePos22(d))

  extracted <- str_match(pos, '([가-힣]+)/[NP][A-Z]')

  keyword <- extracted[, 2]
  keyword[!is.na(keyword)]
}

texts <- searching %>%
  str_replace_all(pattern="\r", replacement="") %>%
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[ㄱ-ㅎㅏ-ㅣ]+", replacement="") %>%
  str_replace_all(pattern="/", replacement=" ") %>%
  str_trim(side="both")

texts <- texts[texts != ""]

pos <- Map(ko_words, texts)

corpus <- Corpus(VectorSource(pos))
stopWord <- c("텍스트", "분석")
tdm <- TermDocumentMatrix(corpus, control=list(
  removePunctuation=TRUE, stopwords=stopWord,
  removeNumbers=TRUE, wordLengths=c(4, 10), weighting=weightBin))
tdm.matrix <- as.matrix(tdm)

word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing=TRUE)
freq.words <- tdm.matrix[word.order[1:30], ]

co.matrix <- freq.words %*% t(freq.words)

tm1<-co.matrix[,-which(apply(co.matrix,2,function(x)all(is.na(x))))]
tm2<-tm1[-which(apply(co.matrix,1,function(x)all(is.na(x)))),]

node_df <- data_frame(node=rownames(tm2), value=as.numeric(diag(tm2))) %>%
  mutate(idx=row_number()-1)

node_json <- toJSON(unname(split(node_df, 1:nrow(node_df))))

link_df <- as.data.frame(as.table(tm2))  %>%
  filter(Freq > 0) %>%
  rename(source=`Terms`, target=`Terms.1`) %>%
  left_join(node_df %>% rename(source_idx=idx) %>% select(-value), by=c('source'='node')) %>%
  left_join(node_df %>% rename(target_idx=idx) %>% select(-value), by=c('target'='node'))

link_json <- toJSON(unname(split(link_df, 1:nrow(link_df))))

result  <- paste0('{"nodes":', node_json, ',"links":', link_json, '}' )

print(result)