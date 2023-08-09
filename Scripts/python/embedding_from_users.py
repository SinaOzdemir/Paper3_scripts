# -*- coding: utf-8 -*-
"""
Created on Wed Feb 22 13:00:33 2023

@author: sinaoz
"""

#libraries

import pandas as pd
import numpy as np
import nltk as tk
import gensim as gs
import re
from stopwordsiso import stopwords
from nltk.tokenize import word_tokenize
import os
from gensim.models.fasttext import FastText
import random
#paths
data_paths = os.getcwd()

tweets_dir = os.path.join(data_paths,"User_embedding_sample_dt")

save_dir = os.path.join(data_paths,"User_embeddings")

data_dirs= os.listdir(tweets_dir)

file_dir = list()

for i in range(len(data_dirs)):
    path = os.path.join(tweets_dir, data_dirs[i])
    file_dir.append(path)

#functions

## preprocessing function


def text_cleaner(text,lg):
    stop_words = set(stopwords(lg))
    stop_words.add("amp")
    replace_by_space = re.compile('[/(){}/[/]/|@,#;]')
    bad_symbols = re.compile('[^0-9a-z +_]')
    text = text.lower()#lowercase everything
    text = replace_by_space.sub(" ",text)#removes symbols
    text = bad_symbols.sub(" ",text)#removes non-letter symbols
    text = " ".join(w for w in text.split() if w not in stop_words)#remove stopwords but keep the spaces for tokenization
    text = tk.tokenize.word_tokenize(text)#tokenize
    return text

## building the embedding
    #currently returns an array of floats without vocabulary
    #I should either extract vocabulary+features from the model
    # average vectors to tweet level then combine user_name and status_id

def document_vectorizer(corpus, model, num_features):
    vocabulary = set(model.wv.index_to_key)
    
    def average_word_vectors(words, model, vocabulary, num_features):
        feature_vector = np.zeros((num_features,), dtype="float64")
        nwords = 0.
        
        for word in words:
            if word in vocabulary: 
                nwords = nwords + 1.
                feature_vector = np.add(feature_vector, model.wv[word])
        if nwords:
            feature_vector = np.divide(feature_vector, nwords)

        return feature_vector
    #for embeddings I shouldn't take the average 
    features = np.array([average_word_vectors(tokenized_sentence, model, vocabulary, num_features)
                    for tokenized_sentence in corpus]) #.mean(axis = 0).reshape(1,num_features)
    features = pd.DataFrame().from_records(features)
    return features


#accessing user data

#test


for i in range(len(file_dir)):
    random.seed(230223)
    language = file_dir[i].split("//")[-1].replace(".txt","").split("_")[-1]
    file_name = language+"_embeddings"+".vec"
    file_dest = os.path.join(save_dir, file_name)
    language_vocab = set()
    with open(file_dir[i],encoding = "utf8") as file:#connect to the file
        nrow = 1
                
        for item in file:
           print("processing tweet:",nrow)
           line = item.split(",")# read a line, split it into a list
           #user_id = line[0] #select user id
           #status_id = line[1] # selet tweet id
           tweet = line[2] # select tweet
           pp_tweet = text_cleaner(tweet, lg = language)#preprocess and convert
           vocab_length = list(dict.fromkeys(pp_tweet))
           if len(vocab_length)<=1:
               print("tweet is too short to generate embeddings")
               next
           else:
               tweet_embedding_model = FastText(pp_tweet,
                                      vector_size=100,
                                      sg = 1,
                                      workers=8,
                                      min_count=1,
                                      window = 3) #need to adjust hyperparameters to align with fast-textwiki embeddings
               for vocab in pp_tweet: language_vocab.add(vocab)
               for vocab in pp_tweet:
                   if vocab in language_vocab:
                       pp_tweet.remove(vocab)
               tweet_embeddings = document_vectorizer(pp_tweet, tweet_embedding_model, num_features = 100)
               tweet_embeddings["features"] = pp_tweet
           #tweet_embeddings["user_id"] = user_id
           #tweet_embeddings["status_id"] = status_id
               if not os.path.exists(file_dest):
                   save_file = open(file_dest,mode = "x")
                   save_file.close()
                   tweet_embeddings.to_csv(file_dest,sep = "\t",na_rep="NA",mode = "w",index = False,header = False,lineterminator="\n",encoding = "utf-8")#should save to bin
               else:
                   tweet_embeddings.to_csv(file_dest,sep = "\t",na_rep="NA",mode = "a",index = False, header = False,lineterminator="\n",encoding = "utf-8")
           nrow = nrow+1
           

#ok so the problem is df shape of the user language file#CLOSED: whole problem was file encoding
#So what I need is to find a way:
    #to read row 1,
    #get column 3 (tweet)
    #parse the tweet
    #learn the embedding for the tweet
    #write the embedding of the tweet along with user_id,status_id

#reading a .vec file into python

# import io
# import os

# def load_vector(fname):
#     fin = io.open(fname,"r",encoding = "utf-8",newline = "\n", errors = "ignore")
#     n,d = map(int,fin.readline().split())
#     data = {}
#     for line in fin:
#         tokens = line.rstrip().split(" ")
#         data[tokens[0]] = map(float,tokens[1:])
#     return(data)

# parent_dir = os.path.normpath("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/User_embedding_sample_dt")

# file = "wiki.no.align.vec"

# nr_embedding = os.path.join(parent_dir, file)

# test_embedding = load_vector(fname = nr_embedding)

# # examine a single line in .vec

# test_embedding = io.open(nr_embedding, mode = "r",encoding = "utf-8",newline = "\n",errors = "ignore")

# te_line_1 = test_embedding.readline(20)#first line is the dimension
#written as tab seperated string

#set test

a = ["a","b","d"]

b = set()

for element in a: b.add(element)

for item in a:
    if item in b:
        a.remove(item)
