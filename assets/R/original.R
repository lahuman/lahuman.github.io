#! /usr/bin/env Rscript
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

library(rvest)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library(dplyr)
library(networkD3)
library(rjson)

useNIADic()

# keyword <- "텍스트+분석"
# url <- paste0("https://search.naver.com/search.naver?where=post&sm=tab_pge&query=", keyword, "&st=sim&date_option=0&date_from=&date_to=&dup_remove=1&post_blogurl=&post_blogurl_without=&srchby=all&nso=&ie=utf8&start=")
# searching <- c()

# for(page in seq(1, 100, by=10)) {
#  page_url <- paste0(url, page)
#  result <- read_html(page_url) %>%
#    html_nodes(css='.sh_blog_passage') %>%
#    html_text()
  # result <- iconv(result, "UTF-8", "CP949") # 윈도우의 경우, R 작업환경에 맞춰 CP949로 변경할 필요가 있습니다.
#  searching <- c(searching, result)
#  Sys.sleep(runif(1, 0, 1))
#}


searching <- read_html(paste0(args[1])) %>%
	html_nodes(css='.zoombtn') %>%
	html_text()

# searching <- paste0(args[1])

print(searching)

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

print(co.matrix)

node_df <- data_frame(node=rownames(co.matrix), value=as.numeric(diag(co.matrix))) %>%
  mutate(idx=row_number()-1)

node_json <- toJSON(unname(split(node_df, 1:nrow(node_df))))

link_df <- as.data.frame(as.table(co.matrix))  %>%
  filter(Freq > 0) %>%
  rename(source=`Terms`, target=`Terms.1`) %>%
  left_join(node_df %>% rename(source_idx=idx) %>% select(-value), by=c('source'='node')) %>%
  left_join(node_df %>% rename(target_idx=idx) %>% select(-value), by=c('target'='node'))

link_json <- toJSON(unname(split(link_df, 1:nrow(link_df))))


result  <- paste0('{"nodes":', node_json, ',"links":', link_json, '}' )
print(result)