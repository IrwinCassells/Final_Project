# IC
# 20 March 2018
# Next Word Predictor

rm(list = ls())

if(!require(rJava)){install.packages("rJava")}
if(!require(RWeka)){install.packages("RWeka")}
if(!require(R.utils)){install.packages("R.utils")}
if(!require(stringi)){install.packages("stringi")}
if(!require(stringr)){install.packages("stringr")}
if(!require(textcat)){install.packages("textcat")}
if(!require(tm)){install.packages("tm")}
if(!require(markovchain)){install.packages("markovchain")}

require("rJava")
require("RWeka")
require("R.utils")
require("stringi")
require("stringr")
require("textcat")
require("tm")
require("markovchain")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(541)

data = c(readLines("en_US.twitter.txt"),readLines("en_US.blogs.txt"),readLines("en_US.news.txt"))
data = sample(data,floor(length(data)/2000))
data = iconv(tolower(data),"latin1","ASCII",sub = "")
data = gsub(pattern = "bitch|fuck|shit|nigger|nigga|cocksucker|piss|motherfucker",replacement = "",x = data)
data = gsub(pattern = "[[:punct:]]",replacement = "",x = data)
data = gsub(pattern = "[[:digit:]]",replacement = "",x = data)
data = trimws(data)

start_ = Sys.time()
s = str_split(data," ",n = Inf)
f = createSequenceMatrix(stringchar = s,toRowProbs = T,sanitize = T) # creates matrix of words

textPredictor = new("markovchain", 
                    states = colnames(f),
                    transitionMatrix = f,
                    name = "state t0")

# check for words here

sentence = "This is a test"

predictNext = function(sen_Now,terms,pred)
{
    # First we clean the sentence we want to predict
    sen_Now = iconv(tolower(sen_Now),"latin1","ASCII",sub = "")
    sen_Now = gsub(pattern = "bitch|fuck|shit|nigger|nigga|cocksucker|piss|motherfucker",replacement = "",x = sen_Now)
    sen_Now = gsub(pattern = "[[:punct:]]",replacement = "",x = sen_Now)
    sen_Now = gsub(pattern = "[[:digit:]]",replacement = "",x = sen_Now)
    sen_Now = trimws(sen_Now)
    sen_Now = unlist(strsplit(sen_Now," "))
    
    # Create a list to store the list of answers 
    ans = list()
    ans$A = character() # arbitary assignment of A (keeps list of all the next word in sentence), B and C
    
    sen_length = length(sen_Now)
    word_Now = sen_Now[1] # Get the first word in the sentence
    vocab = names(pred) # all the words in the markovchain matrix
    
    if (!word_Now %in% vocab)
    {
        word_Now = vocab[sample(1:length(vocab),1)] # just in case we don't have that first word
    }
    
    ans$A = append(ans$A, word_Now) # add to the list of words (same as c() but we want to look a bit more fancy)
    
    # loop through all the words in the sentence and do the same as we did above with the first word
    for (n in 2:sen_length) 
    {
        word_next = sen_Now[n]
        
        if (!word_next %in% vocab) 
        {
            word_next = sort(conditionalDistribution(pred,word_Now),decreasing = T) # this function looks at the most likely next word from the text used
            word_next = word_next[word_next>0] # just eliminates all 0.0 probability terms
            word_next = names(word_next)[1]
        }
        
        word_Now = word_next
        
        ans$A = append(ans$A, word_Now)
    }
    
    ans$C = sort(conditionalDistribution(pred, word_Now),decreasing=T)[1:terms] 
    returnValues = ans$C[ans$C>0]
    returnValues = names(returnValues)
    #returnValues = ifelse(returnCheck,hunspell_suggest(returnValues)[1],returnValues)
    return(returnValues)
}

s = predictNext("The last week pole",1,textPredictor)

if(!hunspell_check(s))
{
    gh = unlist(hunspell_suggest(s))
    
    gh = gh[grepl(pattern = paste0("^",substring(s,1,1)),x = gh)]
    
    s = gh[1]
}

print(s)