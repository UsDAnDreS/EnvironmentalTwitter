import re
import ast
from sklearn.metrics import ConfusionMatrixDisplay
import matplotlib.pyplot as plt
import numpy as np


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


"""
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
"""

list_of_files = [
    "NB_weighted_unenhanced.txt", "NB_unweighted_unenhanced.txt", "SVM_unWeighted_enhanced.txt",
    "SVM_weighted_unenhanced.txt",
    "SVM_unWeighted_unenhanced.txt"
]

for file in list_of_files:
    current_file = open_model_output(file)
    for key, _ in current_file.items():
        clean_up_results(key, current_file)

confusion_matrices = [
    "SVM_BOW_unweighted_enhanced_cross_validation_confusion_matrix(1, 1).txt",
    "SVM_BOW_unweighted_unenhanced_cross_validation_confusion_matrix(1, 1).txt",
    "SVM_BOW_Weighted_unenhanced_cross_validation_confusion_matrix(1, 1).txt"
]


def open_cm_model_output(path):
    with open(path, "r") as file:
        file_content = file.read()
        return file_content


for file in confusion_matrices:
    # Convert the string to a 2D array
    array_str = open_cm_model_output(file)
    rows = array_str.strip().split('\n')
    array = np.array([list(map(float, row.split(','))) for row in rows])

    print(file)
    disp_count = ConfusionMatrixDisplay(confusion_matrix=array,
                                        display_labels=['academia', 'government', 'media', 'other'])
    disp_count.plot()
    plt.show()
