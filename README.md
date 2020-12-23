<p align="center" width="100%"><img width="33%" src="https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/Sk%C3%A6rmbillede%202020-12-03%20kl.%2011.45.47.png"></p>

# Cultural Data Science Exam Project 2020 | Aarhus University
￼This repository contains the contents of the final exam project in the Cultural Data Science course at Aarhus University conducted in the fall of 2020. The main goal of this project was to contribute to the field of Danish Natural Language Processing (NLP). This was accomplished in two ways: 
1. A neural network model was trained on the Danish Gigaword Corpus (DAGW) containing over 1 billion words. 
2. A web-based interactive Shiny application was developed to provide the user with different tools such as exploring semantic relations between words as predicted by the model, generating a word cloud, and performing sentiment analysis. 

Hence, this repository contains two main elements: 

# 1. NEURAL NETWORK MODEL
I have trained a neural network word2vec model on the Danish Gigaword Corpus and you will find all relevant scripts for this process. First, there is a script demonstrating the preprocessing process of a single datafile to illustrate what has been done to the entire corpus. Second you will find a Python script demonstrating how the model was trained on the preprocessed data

- Preprocessing script: https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/RScript_Preprocessing_data.Rmd
- Training the word2vec model on the preprocessed data: https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/PythonScript_Training_word2vec_model_DAGW.py 

# 2. R SHINY APPLICATION
I have developed a R Shiny application that enables you to explore the semantic similarity between words as predicted by the neural network model, generate a word cloud, and perform sentiment analysis. 

- Prerequisites for Shiny application: https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/RSHINY_PREREQUISITES.R
- Shiny Application: https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/RShiny_Application.R


# PROOF OF CONCEPT 
Click to watch the proof of concept video:
[![Watch the video](https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/Sk%C3%A6rmbillede%202020-12-03%20kl.%2011.39.22.png)](https://youtu.be/k36jzNu2fNI)

# LICENSE 
[MIT](https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/LICENSE.md)
