import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_predict, train_test_split, GridSearchCV
from sklearn.svm import SVC
# from sklearn.feature_extraction.text import CountVectorizer, BERTTransformer
from sklearn.pipeline import Pipeline
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import StratifiedKFold

from nltk.corpus import wordnet as wn
from nltk.stem.wordnet import WordNetLemmatizer
from nltk import word_tokenize, pos_tag
from collections import defaultdict
import re
import json
from sklearn.metrics import confusion_matrix
import pickle

tag_map = defaultdict(lambda: wn.NOUN)
tag_map['J'] = wn.ADJ
tag_map['V'] = wn.VERB
tag_map['R'] = wn.ADV
tag_map['AS'] = wn.ADJ_SAT

# filepath = "finalized_8K_accounts.csv"
# filepath = "finalized_8K_accounts_emojis_replaced.csv"
# filepath = "FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_REPLACED.csv"
filepath = "FINALIZED_Training_Data_ALL_Available_Descriptions_EMOJIS_UNCHANGED.csv"

hand_label = "hand.label_simplified"

df = pd.read_csv(filepath)

# Removing all the "-int" (international, non-English, descriptions)
#dict.fromkeys(df[hand_label])
df = df[((df[hand_label] == 'media') | (df[hand_label] == 'tourbiz') |(df[hand_label] == 'acad') | (df[hand_label] == 'gov') | (
        df[hand_label] == 'other'))]

df = df[['username', 'description', hand_label]]  # keep only relevant columns

lemmatizer = WordNetLemmatizer()
words_not_changed = ['media']

# Lemmatization (preprocessing)
def preprocessing(row):
    if str(row) == "nan":
        lemma = ""
    else:
        row = str(row).lower()
        row = word_tokenize(row)  # tokenize
        lemma = [lemmatizer.lemmatize(token, tag_map[tag[0]]) if token not in words_not_changed else token for
                 token, tag in pos_tag(row)]  # lemmatization, depending on part-of-speech
        lemma = ["" if re.search(r'\b[0-9]+\b\s*', lem) else lem for lem in lemma]  # removing
    return str(lemma)


df['description_lemmatized'] = df['description'].apply(preprocessing)

# Remove all the empty descriptions
df = df[df['description_lemmatized'] != ""]
#df[hand_label]
#print(df.shape)
#df[df['description_lemmatized'] != ""].shape


from sentence_transformers import SentenceTransformer
model = SentenceTransformer('all-MiniLM-L6-v2')  # Smaller, faster model (maybe not as good)
# model = SentenceTransformer('all-mpnet-base-v2') # Larger, 5x slower model (best performance, supposedly)



#Sentences are encoded by calling model.encode()
#print(type(df[['description']]))
embeddings = model.encode(df['description'].tolist())  # If we use the NON-preprocessed text (typical for BERT)
# embeddings = model.encode(df['description_lemmatized'].tolist())  # If we use the LEMMATIZED, hence PREPROCESSED, text (not as typical for BERT)



# split my data into training, and test sets
scaler = StandardScaler()

# X = df['description_lemmatized']
X = embeddings
y_labels = df[hand_label]

X_train, X_test, y_train, y_test = train_test_split(X, y_labels, test_size=0.2, random_state=42, stratify=y_labels)

# X2 = df2['description_lemmatized']
# Y2 = df2[hand_label]

# X_train = pd.concat([X_train, X2])
# y_train = pd.concat([y_train, Y2])



# BERT_transformer = BERTTransformer()

# n_gram_ranges = [(1,1), (1,2), (2,2)]
n_gram_ranges = [(1,1)]

result = {}
result_cv = {}

for n_gram_range in n_gram_ranges:
    # count_vectorizer = CountVectorizer(stop_words="english", ngram_range=n_gram_range)
    BERT_pipeline = Pipeline([
        # ('vectorizer', count_vectorizer),
        # ('transformer', BERT_transformer),
        # ('normalize', StandardScaler(with_mean=False)),
        ('classifier', SVC(probability=True))
    ])

    print(n_gram_range)
    
    BERT_param_grid = [
        {
            # 'vectorizer__min_df': [1, 2, 5],
            # 'transformer__use_idf': [True],
            'classifier__C': [1.0e-10, 0.5, 3.0, 10.0],
            'classifier__kernel': ['linear', 'poly', 'rbf', 'sigmoid'],
        }
    ]

    # !!! Does STRATIFICATION BY DEFAULT !!!
    BERT_grid_search = GridSearchCV(estimator=BERT_pipeline, param_grid=BERT_param_grid, cv=10,
                                            scoring='accuracy', verbose=1, error_score="raise")
    BERT_grid_search.fit(X_train, y_train)
    BERT_best_hyperparameters = BERT_grid_search.best_params_
    
    print()
    print()
    print("SVM UNWEIGHT ENHANCED BEST PARAMS:", BERT_best_hyperparameters)

    BERT_best_SVM_model = BERT_grid_search.best_estimator_
    BERT_pipeline.set_params(**BERT_grid_search.best_params_)
    BERT_pipeline.fit(X_train, y_train)
    y_pred_BERT_cross_validation = cross_val_predict(BERT_best_SVM_model, X_train, y_train, cv=10)
    BERT_y_pred_test = BERT_pipeline.predict(X_test)

    cm_count = confusion_matrix(y_train, y_pred_BERT_cross_validation, normalize='true')
    
    print()
    print()
    print()
    print("CV confusion matrix of predictions:")
    print()
    print(cm_count)
    
    # np.savetxt("SVM_BERT_unweighted_enhanced_cross_validation_confusion_matrix" + str(n_gram_range) + '.txt', cm_count,
    #            delimiter=',', fmt='%f')

    result_cv["SVM_BERT_unweighted_enhanced_predictions_CV" + str(n_gram_range)] = metrics.classification_report(y_train, y_pred_BERT_cross_validation)


    print()
    print()
    print()
    print("CV metrics summary:")
    print(result_cv["SVM_BERT_unweighted_enhanced_predictions_CV" + str(n_gram_range)])
    
    result["SVM_BERT_unweighted_enhanced_predictions_testSet" + str(n_gram_range)] = metrics.classification_report(y_test, BERT_y_pred_test)
                         
    print()
    print()
    print()
    print("Test set metrics summary:")
    print()
    print(result["SVM_BERT_unweighted_enhanced_predictions_testSet" + str(n_gram_range)])
    
    print()
    print()
    print()                                                                                       
                                                                                                                
    filename = 'SVM_BERT_unweighted_enhanced_model' + str(n_gram_range) + '.pickle'
    # save model
    pickle.dump(BERT_pipeline, open(filename, "wb"))

    # full_x = pd.concat([X_train, X_test])
    # full_y = pd.concat([y_train, y_test])

    # BERT_grid_search.fit(full_x, full_y)
    BERT_pipeline.set_params(**BERT_grid_search.best_params_)
    # BERT_pipeline.fit(full_x, full_y)
    BERT_pipeline.fit(X, y_labels)

    filename = 'SVM_BERT_unweighted_enhanced_model_full' + str(n_gram_range) + '.pickle'
    pickle.dump(BERT_pipeline, open(filename, "wb"))
    
    # def save_dict_to_file(dictionary, filename):
    #	with open(filename, 'w') as file:
    #    	json.dump(dictionary, file)
    #
    # save_dict_to_file(result["SVM_BERT_unweighted_enhanced_predictions_testSet" + str(n_gram_range)], 'SVM_BERT_unWeighted_full' + str(n_gram_range) + '.txt')

# print(result)

