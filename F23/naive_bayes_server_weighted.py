# Import libraries
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import cross_val_score, cross_val_predict, train_test_split, GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
from nltk.corpus import wordnet as wn
from nltk.stem.wordnet import WordNetLemmatizer
from nltk import word_tokenize, pos_tag
from collections import defaultdict
import re
import json

# Initialization
count_vectorizer = CountVectorizer(stop_words="english", ngram_range=(1, 1))
nb_clf = MultinomialNB()
lemmatizer = WordNetLemmatizer()

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
words_not_changed = ['media']

result = {}
n_gram_range = (1, 1)


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
# split my data into training, and test sets
scaler = StandardScaler()

X = df['description_lemmatized']
y_labels = df[hand_label]

X_train, X_test, y_train, y_test = train_test_split(X, y_labels, test_size=0.2, random_state=42, stratify=y_labels)

weights_per_class = {
    "w_other": {},
    "w_media": {},
    "w_acad": {},
    "w_gov": {}
}

labels = y_train.value_counts().index.tolist()
values = y_train.value_counts()

for i in range(len(values)):
    # print(labels[i], values[i])

    class_name = "w_" + labels[i]
    value = values[i]
    weights_per_class[class_name] = 1 / value

# Weights list
weights_list = []

for _, label in y_train.items():
    # print(label)
    class_name = "w_" + label
    weight = weights_per_class[class_name]
    weights_list.append(weight)

nb_count_pipeline = Pipeline([('vectorizer', count_vectorizer),
                              ('classifier', nb_clf)])

param_count_grid = [
    {
        'vectorizer__min_df': [1,2,5],
        'classifier__alpha': [1.0e-10, 0.5, 2.0, 5.0, 10.0],
        'classifier__fit_prior': [True, False],
    }
]

grid_search_count = GridSearchCV(nb_count_pipeline, param_count_grid, cv=5, scoring='accuracy', verbose=1, classifier__sample_weight=weights_list)
grid_search_count.fit(X_train, y_train)

nb_count_pipeline.set_params(**grid_search_count.best_params_)
# nb_count_pipeline.fit(X_train, y_train, classifier__sample_weight=weights_list)
nb_count_pipeline.fit(X_train, y_train)

y_pred_count = cross_val_predict(nb_count_pipeline, X_train, y_train, cv=5)
y_pred_count_test = nb_count_pipeline.predict(X_test)

result["BOW_weighted_unenhanced" + str(n_gram_range)] = metrics.classification_report(y_test, y_pred_count_test)


def save_dict_to_file(dictionary, filename):
    with open(filename, 'w') as file:
        json.dump(dictionary, file)


save_dict_to_file(result, 'NB_weighted_unenhanced.txt')
