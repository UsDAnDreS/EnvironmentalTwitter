import re
from nltk.tokenize import word_tokenize
from util import Util


class Analysis:

    @staticmethod
    def preprocess_text_column(just_text_col):
        """
        :param just_text_col: Takes just the text column from the dataframe
        :return: a tokenized and partially cleaned text column
        """
        tokenized_text_column = []
        for line in just_text_col:
            my_punct = ['!', '"', '$', '%', '&', "'", '(', ')', '*', '+', ',', '.',
                        '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_',
                        '`', '{', '|', '}', '~', '»', '«', '“', '”']
            punct_pattern = re.compile("[" + re.escape("".join(my_punct)) + "]")
            line = re.sub(punct_pattern, "", line)
            line = re.sub(r"/[^\w\s\']|_/g, """, "", line)
            line = re.sub(r"/\s+/g, " "", "", line)
            line = re.sub(r"(https|www).*com", "", line)
            line = re.sub(r"\s+", " ", line)
            tokenized_text = word_tokenize(line)
            # if tokenized_text[0] != "RT":
            if tokenized_text not in tokenized_text_column:
                tokenized_text = Util().combine_hashtag_words(tokenized_text)
                Util().geo_tag_harvester(tokenized_text)
                Util().relevant_terms_catcher(tokenized_text)
                tokenized_text_column.append(tokenized_text)
        return tokenized_text_column

    @staticmethod
    def create_n_gram_corpus(tokenized_corpus_sentences, length_of_gram):
        """
        A function that will take a list of words and a length (1, 2 ,3) and return lists of words partitioned into
        the specific length :param tokenized_corpus_sentences: String[][] the final list of words that have been
        cleaned and tokenized :param length_of_gram: Integer -> the length of which you want the output ngrams to be
        of length :return: String[][] partitioned list of words into n-grams
        """
        xyz_gram_corpus = []
        for sentence in tokenized_corpus_sentences:
            if length_of_gram == 1:
                for i in range(len(sentence)):
                    xyz_gram_corpus.append(sentence[i])
            elif length_of_gram == 2:
                for i in range(len(sentence) - 1):
                    xyz_gram_corpus.append(sentence[i:i + 2])
            elif length_of_gram == 3:
                for i in range(len(sentence) - 2):
                    xyz_gram_corpus.append(sentence[i:i + 3])
        return xyz_gram_corpus

    def get_raw_frequency(self, xyz_gram_corpus):
        """
        takes in an n-gram partitioned corpus and returns a dictionary with the frequency of each n-gram
        :param xyz_gram_corpus: String[][] the n-gram partitioned corpus
        :return: Dict a dictionary of the frequency of each n-gram
        """
        frequency_dict = {}
        for gram in xyz_gram_corpus:
            if isinstance(gram, list):
                gram = tuple(gram)
            if frequency_dict.get(gram):
                frequency_dict[gram] = frequency_dict.get(gram) + 1
            else:
                frequency_dict[gram] = 1

        return sorted(frequency_dict.items(), key=lambda x: x[1], reverse=True)

    def get_normalized_frequency(self, xyz_gram_corpus):
        """
        :param xyz_gram_corpus: String[][] the n-gram partitioned corpus
        :return: Dict a dictionary of the normalized frequency of each n-gram
        """
        normalized_frequency = {}

        raw_frequency = self.get_raw_frequency(xyz_gram_corpus)
        for gram in raw_frequency:
            normalized_frequency[gram[0]] = int(gram[1]) / len(xyz_gram_corpus)

        return normalized_frequency

    def analyze_files(self, files):
        """
        Takes in a list of file urls, downloads each file, and generates uni-gram, bi-gram, and tri-gram frequency dictionaries for
        their text column. Returns a dictionary of n-gram frequency dictionaries for each file.
        """
        n_grams = {}
        for file in files:
            df = Util().get_file_dataframe(file)
            just_text_col = []
            for line in df["text"]:
                just_text_col.append(line)

            tokenized_text_column = self.preprocess_text_column(just_text_col)
            filtered_sentences = Util().filter_stopwords(tokenized_text_column)
            uni_grams = self.create_n_gram_corpus(filtered_sentences, 1)
            bi_grams = self.create_n_gram_corpus(filtered_sentences, 2)
            tri_grams = self.create_n_gram_corpus(filtered_sentences, 3)
            normal_uni = self.get_normalized_frequency(uni_grams)
            normal_bi = self.get_normalized_frequency(bi_grams)
            normal_tri = self.get_normalized_frequency(tri_grams)
            n_grams[file] = {"uni_grams": normal_uni, "bi_grams": normal_bi, "tri_grams": normal_tri}
            # TODO: Add a way to save/Export these to a CSV file?
        return n_grams


if __name__ == '__main__':
    list_of_files = [
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data/RedTide_Pasco_all_SIMPLE_columns"
        ".csv"]

    analysis_hub = Analysis()
    n_grams_per_file = analysis_hub.analyze_files(list_of_files)
    normalized_n_grams = n_grams_per_file[
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data/RedTide_Pasco_all_SIMPLE_columns" \
        ".csv"]["uni_grams"]

    # Getting the top 10 items with the highest values
    top_n = sorted(normalized_n_grams, key=normalized_n_grams.get, reverse=True)[:100]
    toptag = sorted(normalized_n_grams, key=normalized_n_grams.get, reverse=True)

    hashtags_absolute_frequency_dict = {}
    hashtags_normalized_frequency_dict = {}

    for key in normalized_n_grams:
        if '#' in key[0]:
            value = normalized_n_grams[key]
            print("Hashtag:", key)
            print("Absolute Frequency:", int(value * 63716), "Normalized Frequency:", value, "\n")
            hashtags_absolute_frequency_dict[key] = value * 63716
            # Only adding this as a temporary work-around to get the raw count instead of
            hashtags_normalized_frequency_dict[key] = value

    relevant_terms_dict = Util().relevant_terms_dict
    geo_tag_dict = Util().geo_tag_dict
    popular_hashtags_dict = Util().non_geo_hashtags_dict

    for key in relevant_terms_dict:
        if '#' in key[0]:
            value = relevant_terms_dict[key]
            print("Hashtag:", key)
            print("Absolute Frequency:", int(value * 63716), "Normalized Frequency:", value, "\n")

    top_n_geo_hashtags = sorted(geo_tag_dict, key=geo_tag_dict.get, reverse=True)[:50]
    print("TOP N GEO HASHTAGS: ", top_n_geo_hashtags)

    top_n_hashtags = sorted(popular_hashtags_dict, key=popular_hashtags_dict.get, reverse=True)[:50]
    print("TOP N HASHTAGS:", top_n_hashtags)

    top_n_terms = sorted(relevant_terms_dict, key=relevant_terms_dict.get, reverse=True)[:50]
    print("RELEVANT TOP N TERMS: ", top_n_terms)

    popular_tweets_overall = Util().tweet_popularity_weight(list_of_files[0], 1, 1, 1, 1, 1)
    Util().top_n_tweets_per_county(3, popular_tweets_overall)





