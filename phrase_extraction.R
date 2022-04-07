library(spacyr)
library(udpipe)
library(dplyr)
library(tidyr)

## NLP 
# install spacyr and NLP algorithm
spacy_install()
spacy_initialize(model = "en_core_web_sm")

# operate NLP using spacyr 
parse <- spacy_parse("YOUR TEXT CONTENT HERE", dependency = T, lemma = T, pos = T, entity = T, nounphrase = T)
parse$doc_id <- as.numeric(sub('text', "", parse$doc_id))

# remove DETs as stopwords (Stopwords were retrieved from SMART, snowball, and onix. Refer to https://github.com/igorbrigadir/stopwords for details)
stop_words <- read.csv("YOUR DIRECTORY FOR STOPWORDS", header = TRUE, fileEncoding="UTF-8-BOM") 
stop_words <- spacy_parse(stop_words$word, lemma = T, pos = T)
stop_words_DET <- stop_words[stop_words$pos == "DET", ]
colnames(stop_words_DET)[5] <- "word"

parse$word <- parse$lemma
parse_1 <- parse %>%
  anti_join(stop_words_DET, by = "word")

# remove punctuations ","
parse_1$word <- gsub(',', "\\1", parse_1$word)

# operate Named Entity Recognition to filter place names 
entity_names <- entity_extract(parse_1, type = "named", concatenator = " ")
entity_names <- entity_names %>%
  count(entity) %>% 
  arrange(desc(n))
entity_names$ngram <- lengths(gregexpr("\\w+", entity_names$entity)) #+ 1

# consolidate place names using udpipe
entity_names_merge <- data.frame(txt_recode_ngram(parse_1$word, compound = entity_names$entity, 
                                                  ngram = entity_names$ngram, sep = " "))
colnames(entity_names_merge) <- c("word1")
parse_1 <- cbind(parse_1, entity_names_merge)
parse_1 <- parse_1 %>% mutate(word1 = na_if(word1, "")) %>% drop_na()

## phrase parsing using udpipe 
# phrase parsing: verb phrases 
parse_verb <- keywords_phrases(parse_1$pos, term = parse_1$word1,
                               pattern = c("(VERB*ADV$)|(VERB*ADP$)|(VERB*ADV*(ADP|ADJ|NOUN)*NOUN$)|(VERB*PART*VERB*((ADP|ADJ|VERB)*NOUN))|(VERB*(ADP|ADJ|NOUN|PRON)*NOUN$)|(VERB*(ADJ|NOUN)*CCONJ*(ADJ|NOUN)*NOUN$)"),
                               is_regex = TRUE, detailed = T)
parse_verb <-as.data.frame(table(parse_verb$keyword)) %>%
  arrange(desc(Freq))
parse_verb$ngram <- lengths(gregexpr("\\W+", parse_verb$Var1)) +1

# phrase parsing: noun phrases
parse_noun <- keywords_phrases(parse_1$pos, term = parse_1$word1,
                               pattern = "((ADJ|NOUN)*NOUN$)|(NOUN*ADP*NOUN$)|(ADJ*CCONJ*ADJ*NOUN$)|(NOUN*CCONJ*NOUN$)|((ADJ|NOUN)*NOUN*PART*VERB*(ADP|ADJ|NOUN)*NOUN$)|(ADJ*NOUN*ADP*PROPN$)", 
                               is_regex = TRUE, detailed = T) 
parse_noun <- parse_noun[parse_noun$ngram != 1, ] #the POS library for noun phrases may return single noun results; remove them
parse_noun <-as.data.frame(table(parse_noun$keyword))%>%
  arrange(desc(Freq))
parse_noun$ngram <- lengths(gregexpr("\\W+", parse_noun$Var1)) +1
