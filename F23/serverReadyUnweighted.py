import pandas as pd
from sklearn.model_selection import cross_val_predict, train_test_split, GridSearchCV
from sklearn.svm import SVC
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.pipeline import Pipeline
from sklearn import metrics
from sklearn.preprocessing import StandardScaler


from nltk.corpus import wordnet as wn
from nltk.stem.wordnet import WordNetLemmatizer
from nltk import word_tokenize, pos_tag
from collections import defaultdict
import re
import json

tag_map = defaultdict(lambda: wn.NOUN)
tag_map['J'] = wn.ADJ
tag_map['V'] = wn.VERB
tag_map['R'] = wn.ADV
tag_map['AS'] = wn.ADJ_SAT

# filepath = "finalized_8K_accounts.csv"
filepath = "./data/finalized_8K_accounts_emojis_replaced.csv"
hand_label = "hand.label"
government = "gov"
academia = "acad"

df = pd.read_csv(filepath)

df = df[((df[hand_label] == 'media') | (df[hand_label] == academia) | (df[hand_label] == government) | (
            df[hand_label] == 'other'))]

df = df[['username', 'description', hand_label]]  # keep only relevant columns

lemmatizer = WordNetLemmatizer()
words_not_changed = ['media']


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
"""
# Enhanced data
filepath = "./data/finalized_BIASED_accounts_ONLY_NON_OTHER_emojis_replaced.csv"

df2 = pd.read_csv(filepath)
df2 = df2[((df2[hand_label] == 'media') | (df2[hand_label] == academia) | (df2[hand_label] == government) | (
        df2[hand_label] == 'other'))]

df2 = df2[['username', 'description', hand_label]]  # keep only relevant columns

df2['description_lemmatized'] = df2['description'].apply(preprocessing)
"""
# %%
# split my data into training, and test sets
scaler = StandardScaler()

X = df['description_lemmatized']
y_labels = df[hand_label]

X_train, X_test, y_train, y_test = train_test_split(X, y_labels, test_size=0.2, random_state=42, stratify=y_labels)
# %%
# X2 = df2['description_lemmatized']
# Y2 = df2[hand_label]

# X_train = pd.concat([X_train, X2])
# y_train = pd.concat([y_train, Y2])

tfidf_transformer = TfidfTransformer()

n_gram_ranges = [(1,1), (1,2), (2,2)]

result = {}
for n_gram_range in n_gram_ranges:
    count_vectorizer = CountVectorizer(stop_words="english", ngram_range=n_gram_range)

    tfidf_pipeline = Pipeline([
        ('vectorizer', count_vectorizer),
        ('transformer', tfidf_transformer),
        ('normalize', StandardScaler(with_mean=False)),
        ('classifier', SVC())
    ])

    tfidf_param_grid = [
        {
            ''
            'vectorizer__min_df': [0.0],
            'transformer__use_idf': [True],
            'classifier__C': [1.0e-10, 0.5, 3.0, 10.0],
            'classifier__kernel': ['linear', 'poly', 'rbf', 'sigmoid'],
            # 'classifier__class_weight': ["balanced"]
        }
    ]

    bag_of_words_pipeline = Pipeline([
        ('vectorizer', count_vectorizer),
        ('normalize', StandardScaler(with_mean=False)),
        ('classifier', SVC())
    ])

    bag_of_words_param_grid = [
        {
            'vectorizer__min_df': [0.0],
            'classifier__C': [1.0e-10, 0.5, 3.0, 10.0],
            'classifier__kernel': ['linear', 'poly', 'rbf', 'sigmoid'],
            # 'classifier__class_weight': ["balanced"]
        }
    ]

    tfidf_grid_search = GridSearchCV(estimator=tfidf_pipeline, param_grid=tfidf_param_grid, cv=5, scoring='accuracy',
                                     verbose=1, error_score="raise")
    tfidf_grid_search.fit(X_train, y_train)
    bag_of_words_grid_search = GridSearchCV(estimator=bag_of_words_pipeline, param_grid=bag_of_words_param_grid, cv=5,
                                            scoring='accuracy', verbose=1, error_score="raise")
    bag_of_words_grid_search.fit(X_train, y_train)
    tfidf_best_hyperparameters = tfidf_grid_search.best_params_
    bag_of_words_best_hyperparameters = bag_of_words_grid_search.best_params_
    tfidf_best_SVM_model = tfidf_grid_search.best_estimator_
    tfidf_pipeline.set_params(**tfidf_grid_search.best_params_)
    tfidf_pipeline.fit(X_train, y_train)
    bag_of_words_best_SVM_model = bag_of_words_grid_search.best_estimator_
    bag_of_words_pipeline.set_params(**bag_of_words_grid_search.best_params_)
    bag_of_words_pipeline.fit(X_train, y_train)
    y_pred_tfidf = cross_val_predict(tfidf_best_SVM_model, X_train, y_train, cv=5)
    y_pred_bag_of_words = cross_val_predict(bag_of_words_best_SVM_model, X_train, y_train, cv=5)
    tfidf_y_pred_test = tfidf_pipeline.predict(X_test)
    bag_of_words_y_pred_test = bag_of_words_pipeline.predict(X_test)

    """    
    print("TF-IDF Classification Report TEST:")
    print(metrics.classification_report(y_test, tfidf_y_pred_test))
    print()
    
    print("Bag of Words Classification Report TEST:")
    print(metrics.classification_report(y_test, bag_of_words_y_pred_test))
    print()
    """

    result["tfidf_unweighted_unenhanced" + n_gram_range] = metrics.classification_report(y_test, tfidf_y_pred_test)
    result["BOW_unweighted_unenhanced" + n_gram_range] = metrics.classification_report(y_test, bag_of_words_y_pred_test)

print(result)


def save_dict_to_file(dictionary, filename):
    with open(filename, 'w') as file:
        json.dump(dictionary, file)

save_dict_to_file(result, 'unWeighted_unenhanced.txt')

