<p align="center" width="100%"><img width="33%" src="https://github.com/TheNLPlayPlatform/NLPlay/blob/main/NLPlayLogo.png"></p>

This repository contains the scripts and other resources it takes to reproduce the NLPlay Platform developed by Sofie Ditmer and Astrid Nørgaard Fonager. The main goal of this project was to contribute to the field of Danish Natural Language Processing (NLP) and enable Danish speakers wihtout preexisting programming experience to benefit from quantitative text-mining tools in their work with Danish texts. This was accomplished in two ways: 

##### 1. The NLPlay Platform: a web-based interactive Shiny application developed to provide the user with different text mining tools: topic modeling, sentiment analysis, and a word cloud generator.
##### 2. The DAGW-model: a neural network model trained on the largest existing Danish corpus, The Danish Gigaword Corpus (DAGW), using the word2vec framework.

## THE NLPLAY PLATFORM
The NLPlay platform is a Shiny application developed in R. The platform provides the user with multiple text mining tools. Within the platform the user is able to explore the semantic similarity between words as predicted by the neural network model, generate a word cloud based on a self-chosen text, perform topic modeling on a collection of documents, and perform sentiment analysis on self-chosen texts. 
NLPlay is online! https://sofieditmer.shinyapps.io/NLPlay/

NB! The DAGW-model functionalities are not available online. This is something we plan to change.

## THE DAGW-MODEL
The DAGW-model is a neural network model trained on the largest existing Danish corpus, The Danish Gigaword Corpus (DAGW), using the word2vec framework. 

## HOW TO REPRODUCE THE NLPLAY PLATFORM
1. Download the DAGW-model here: https://drive.google.com/uc?export=download&id=1eHTn4vrtjLq6CLDu3WHHVBqN0u6VBB03

2. Download the NLPlay folder that contains a setup-script and the Shiny application.

3. Place the DAGW-model file in the NLPlay folder where the Shiny application is located.

4. Run the setup-script.

5. Run the Shiny application.


## PROOF OF CONCEPT 
Click to watch the proof of concept video:
[![Watch the video](https://github.com/sofieditmer/CulturalDataScienceExamProject2020/blob/main/billede.png)](https://www.youtube.com/watch?v=H15M2p28p9Y)

If you want to watch the video in English click here: https://www.youtube.com/watch?v=ezeK7Fv_MVc 

## LICENSE 
[MIT](https://github.com/TheNLPlayPlatform/NLPlay/blob/main/LICENSE)
