import re
from util import Util
from SentimentAnalysis import *


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
                        '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '-',
                        '`', '{', '|', '}', '~', '»', '«', '“', '”']
            punct_pattern = re.compile("[" + re.escape("".join(my_punct)) + "]")
            line = re.sub(punct_pattern, "", line)
            line = re.sub(r"/[^\w\s\']|_/g, """, "", line)
            line = re.sub(r"/\s+/g, " "", "", line)
            line = re.sub(r"(https|www).*com", "", line)
            line = re.sub(r"\s+", " ", line)
            tokenized_text = line.split()
            # tokenized_text = word_tokenize(line)
            if tokenized_text not in tokenized_text_column:
                # I should remove our own stop words here:
                hashtags = Util().extract_hashtags(tokenized_text)
                filtered_tags = Util().filter_hashtags(hashtags)
                Util().geo_tag_harvester(filtered_tags)
                tokenized_text_column.append(tokenized_text)
        return tokenized_text_column

    def analyze_files(self, file):
        """
        Takes in a list of file urls, downloads each file, and generates uni-gram, bi-gram, and tri-gram frequency dictionaries for
        their text column. Returns a dictionary of n-gram frequency dictionaries for each file.
        """
        df = Util().get_file_dataframe(file)
        just_text_col = []
        for line in df["text"]:
            line = re.sub(r'\bhttps\w*\b.*', '', line)
            just_text_col.append(line)

        self.preprocess_text_column(just_text_col)

    def output_file_name(self, github_url):
        start_index = github_url.index('Data/') + len('Data/')
        end_index = github_url.index('_all_SIMPLE_columns.csv')

        return github_url[start_index:end_index]


if __name__ == '__main__':
    analysis_hub = Analysis()
    list_of_files = [
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data/RedTide_Pasco_all_SIMPLE_columns"
        ".csv",
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data/RedTide_Tampa_all_SIMPLE_columns"
        ".csv",
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data"
        "/RedTide_Sarasota_all_SIMPLE_columns.csv",
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data/RedTide_Pinellas"
        ".StPete_all_SIMPLE_columns.csv",
        "https://raw.githubusercontent.com/UsDAnDreS/EnvironmentalTwitter/main/Data"
        "/RedTide_Manatee_all_SIMPLE_columns.csv "
    ]

    for file in list_of_files:
        analysis_hub.analyze_files(file)

        geo_tag_dict = Util().geo_tag_dict
        popular_hashtags_dict = Util().non_geo_hashtags_dict

        top_n_geo_hashtags = sorted(geo_tag_dict, key=geo_tag_dict.get, reverse=True)[:50]
        print("TOP N GEO HASHTAGS: ", top_n_geo_hashtags)

        top_n_hashtags = sorted(popular_hashtags_dict, key=popular_hashtags_dict.get, reverse=True)[:50]
        print("TOP N HASHTAGS:", top_n_hashtags)

        popular_tweets_overall = Util().tweet_popularity_weight(file, 1, 1, 1, 1, 1)
        # top_3_tweets = Util().top_n_tweets_per_county(3, popular_tweets_overall)

        destination_path = analysis_hub.output_file_name(file)

        Util().frequency_csv_creator(geo_tag_dict, "geo_tags_" + destination_path)
        Util().frequency_csv_creator(popular_hashtags_dict, "non_geo_tags_" + destination_path)

        sentiment_df = format_data(file)
        export_sentiment(sentiment_df, destination_path)
        # This should be the last thing to call for each file
        # Util().tweets_csv_creator(top_3_tweets, destination_path)
        Util().tweets_csv_creator(popular_tweets_overall, destination_path)
