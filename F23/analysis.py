import re

import ast


def open_model_output(path):
    with open(path, "r") as file:
        file_content = file.read()
        dictionary = ast.literal_eval(file_content)
    return dictionary


def clean_up_results(key, results):
    test_case = re.sub(' + ', ' ', results[key])
    test_case = test_case.split()

    column_width = 12
    print(key)
    print(" ".rjust(column_width), test_case[2].rjust(column_width))
    print(test_case[4].rjust(column_width), test_case[7].rjust(column_width))
    print(test_case[9].rjust(column_width), test_case[12].rjust(column_width))
    print(test_case[14].rjust(column_width), test_case[17].rjust(column_width))
    print(test_case[19].rjust(column_width), test_case[22].rjust(column_width))
    print(test_case[24].rjust(column_width), test_case[25].rjust(column_width))
    print((test_case[27] + " " + test_case[28]).rjust(column_width), test_case[31].rjust(column_width))
    print((test_case[33] + " " + test_case[34]).rjust(column_width), test_case[37].rjust(column_width))
    print()


# Everything SVM
SVM_unweighted = "unWeighted_unenhanced.txt"
unweighted = open_model_output(SVM_unweighted)

SVM_weighted = "weighted_unenhanced.txt"
weighted = open_model_output(SVM_weighted)

# Everything Naive Bayes
NB_unweighted_unenhanced = "NB_unweighted_unenhanced.txt"
NB_weighted_unenhanced = "NB_weighted_unenhanced.txt"

nb_unweighted = open_model_output(NB_unweighted_unenhanced)
nb_weighted = open_model_output(NB_weighted_unenhanced)

print("SVM Model:")

print("------------------------------")

for key, _ in unweighted.items():
    clean_up_results(key, unweighted)

print("------------------------------")

for key, _ in weighted.items():
    clean_up_results(key, weighted)

print("------------------------------")

print("Naive Bayes Model:")

clean_up_results("BOW_unweighted_unenhanced(1, 1)", nb_unweighted)
clean_up_results("BOW_weighted_unenhanced(1, 1)", nb_weighted)
