import pandas as pd
import matplotlib.pyplot as plt
import re
from collections import Counter
import csv

# -----------------------------
# Load and deduplicate dataset
# -----------------------------
df = pd.read_excel("Copy of Systematic mapping data extraction form (Responses).xlsx")

#Remove NaNs
df = df.fillna("NaN")

#-------
# Trying long form
#-------
fileID_col = "Study ID (format: first author_year_letterIfNeeded )"

long_algos = []

def normalize_algo(a: str):
    if "cnn" in a or "convolution" in a:
        return "CNNs"
    if "svm" in a or "support vector" in a:
        return "SVMs"
    if "random forest" in a:
        return "Random forests"
    if "rnn" in a or "recurrent" in a:
        return "RNNs"
    if "decision tree" in a:
        return "Decision trees"
    if "boost" in a:
        return "Boosted trees"
    if "transformer" in a or "llm" in a or "transofrmer" in a or "foundation model" in a:
        return "Transformers / LLMs"
    if "knn" in a or "KNN" in a or "kNN" in a:
        return "kNN"
    if "Autoencoder" in a or "autoencoder" in a or "autoencode" in a:
        return "Autoencoder"
    if "kmeans" in a:
        return "kmeans"
    if "hidden markov model" in a or "Hidden markov model" in a or "Hidden Markov models" in a or "markov chain monte carlo" in a or "markov chain models" in a or "markov chains" in a:
        return "Hidden Markov models"
    if "ann" in a:
        return "ANNs"
    if "genetic algorithms" in a or "genetic algorithm" in a or "genetic programming" in a or "evolutionary algorithms" in a or "evolutionary algorithm" in a or "evolutionalry algorithms" in a or "genertic algorithms" in a:
        return "Genetic / Evolutionary algorithms"
    if "gnn" in a or "graph neural networks" in a or "graph neural network" in a:
        return "GNNs"
    if "other" in a or "nlp" in a or "generalized annealing" in a or "attention network" in a:
        return None
    if a == "nan":
        return None
    return a

for index, row in df.iterrows():
    fileID = row[fileID_col]
    algos = []
    
    algo_col = "Which specific AI algorithms are mentioned?"
    for p in re.split(r",|;|/|\n|\.", str(row[algo_col]).lower()):
        p = p.strip()
        if p:
            algos.append(p)
    
    algo_col = "If other algorithm, specify:"        
    for p in re.split(r",|;|/|\n|\.", str(row[algo_col]).lower()):
        p = p.strip()
        if p:
            algos.append(p)    
                
    normed_algos = [normalize_algo(a) for a in algos]
    normed_algos = list(set(normed_algos))
    normed_algos = [a for a in normed_algos if a is not None]
    
    for algo in normed_algos:
        long_algos.append([fileID,algo])
        
#print(long_algos)

for row in long_algos:
    #row = list(row)
    if "CNNs" in row[1]:
        row.append("Modern")
    if "Random forests" in row[1]:
        row.append("Classic")
    if "SVMs" in row[1]:
        row.append("Classic")
    if "kNN" in row[1]:
        row.append("Classic")
    if "RNNs" in row[1]:
        row.append("Modern")
    if "Decision trees" in row[1]:
        row.append("Classic")
    if "Genetic / Evolutionary algorithms" in row[1]:
        row.append("Classic")
    if "Boosted trees" in row[1]:
        row.append("Classic")
    if "ANNs" in row[1]:
        row.append("Modern")
    if "Transformers / LLMs" in row[1]:
        row.append("Contemporary")
    if "kmeans" in row[1]:
        row.append("Classic")
    if "Hidden Markov models" in row[1]:
        row.append("Classic")
    if "Autoencoder" in row[1]:
        row.append("Modern")
    if "GNNs" in row[1]:
        row.append("Modern")

with open('algorithmsLong.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['FileID','Algorithm','Era']) #Headers
    writer.writerows(long_algos)

#algo_counts = [list(elem) for elem in algo_counts]
#-------
# Data modality
#-------

long_mods = []

def normalize_mod(a: str):
    if "images" in a:
        return "Images"
    if "numeric" in a:
        return "Numeric"
    if "audio" in a:
        return "Audio"
    if "video" in a:
        return "Video"
    return a

for index, row in df.iterrows():
    fileID = row[fileID_col]
    modality_col = "Categories of the data used for the AI models"
    mod = []
    
    for p in re.split(r",|;|/|\n", str(row[modality_col]).lower()):
        p = p.strip()
        if p:
            mod.append(p)    
    
    normed_mod = [normalize_mod(a) for a in mod]
    normed_mod = list(set(normed_mod))
    normed_mod = [a for a in normed_mod if a is not None]
            
    for algo in normed_mod:
        long_mods.append([fileID,algo])    
        
with open('dataModalityLong.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['FileID','Data Modality']) #Headers
    writer.writerows(long_mods)
    
#--------
# Data source
#--------

long_source = []

def normalize_source(a: str):
    if "field-collected" in a:
        return "Field"
    if "public repository" in a:
        return "Repository"
    if "lab-generated" in a:
        return "Lab"
    return a

for index, row in df.iterrows():
    fileID = row[fileID_col]
    source_col = "Source of data used in primary studies reviewed"
    sources = []
    
    for p in re.split(r",|;|/|\n", str(row[source_col]).lower()):
        p = p.strip()
        if p:
            sources.append(p)    
    
    normed_source = [normalize_source(a) for a in sources]
    normed_source = list(set(normed_source))
    normed_source = [a for a in normed_source if a is not None]
            
    for source in normed_source:
        long_source.append([fileID,source])    
        
with open('dataSourceLong.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['FileID','Source of Data']) #Headers
    writer.writerows(long_source)
    
#----------
# Task type
#----------

long_task = []

def normalize_task(a: str):
    if "classification" in a:
        return "Classification"    
    if "prediction" in a:
        return "Prediction/Regression"
    if "clustering" in a:
        return "Clustering"
    if "generation" in a:
        return "Generation"
    if "dimensionality reduction" in a:
        return "Dimensionality reduction"
    if "other" in a:
        return "Other"
    return None

for index, row in df.iterrows():
    fileID = row[fileID_col]
    task_col = "Broad category of the task or goal the AI models are used for"
    tasks = []
    
    for p in re.split(r",|;|/|\n", str(row[task_col]).lower()):
        p = p.strip()
        if p:
            tasks.append(p)    
    
    normed_task = [normalize_task(a) for a in tasks]
    normed_task = list(set(normed_task))
    normed_task = [a for a in normed_task if a is not None]
            
    for task in normed_task:
        long_task.append([fileID,task])    
        
with open('taskLong.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['FileID','Task']) #Headers
    writer.writerows(long_task)

#-----------
# Training paradigm
#-----------
long_train = []

def normalize_train(a: str):
    if "unsupervised" in a:
        return "Unsupervised"    
    if "supervised" in a:
        return "Supervised"
    if "reinforcement" in a:
        return "Reinforcement Learning"
    if "none" in a:
        return None
    return None

for index, row in df.iterrows():
    fileID = row[fileID_col]
    train_col = "Broad category of AI models discussed"
    trains = []
    
    for p in re.split(r",|;|/|\n", str(row[train_col]).lower()):
        p = p.strip()
        if p:
            trains.append(p)    
    
    normed_train = [normalize_train(a) for a in trains]
    normed_train = list(set(normed_train))
    normed_train = [a for a in normed_train if a is not None]
            
    for train in normed_train:
        long_train.append([fileID,train])    
        
with open('trainingLong.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['FileID','Training paradigm']) #Headers
    writer.writerows(long_train)