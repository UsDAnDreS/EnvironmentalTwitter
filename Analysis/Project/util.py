import requests
import pandas as pd
import numpy as np
import io
import re
from nltk.corpus import stopwords
import collections
from Locations import Locations


class Util:
    geo_tag_dict = collections.defaultdict(int)
    non_geo_hashtags_dict = collections.defaultdict(int)
    pure_locations = Locations().get_pure_locations("combined")
    categorized_locations = Locations().category_adder("combined")
    relevant_terms_dict = {}

    def get_file_dataframe(self, url):
        """
        :param url: url for a specific CSV file (from the github repo)
        :return: a dataframe of the downloaded csv file
        """
        download = requests.get(url).content
        file_as_dataFrame = pd.read_csv(io.StringIO(download.decode('utf-8')))
        return file_as_dataFrame

    def combine_hashtag_words(self, lst):
        """
        Takes in a list of tokens, and will combine tokens that are consecutive with the first one being a hashtag
        :param lst: an array of tokens
        :return: an array of tokens of lesser size since some are combined
        """
        combined_list = []
        i = 0
        while i < len(lst):
            if lst[i] == "#" and i + 1 < len(lst) and not lst[i + 1].startswith("#"):
                combined_list.append("#" + lst[i + 1].lower())
                i += 2
            else:
                combined_list.append(lst[i])
                i += 1
        return combined_list

    def geo_tag_harvester(self, tokenized_sentnece):
        """
        This was made to look through a sentence at a time
        :param tokenized_sentnece: an array of tokens
        :return: None, it's a counter for location hashtags
        """
        pure_set = set(self.pure_locations[0])
        padded_set = set(self.categorized_locations[0])

        for word in tokenized_sentnece:
            if '#' in word:
                hashtag = word
                pure_word = re.sub(r"#", "", hashtag)

                if pure_word in pure_set or pure_word in padded_set:

                    self.geo_tag_dict[hashtag] += 1
                else:
                    self.non_geo_hashtags_dict[hashtag] = self.non_geo_hashtags_dict[hashtag] + 1

    def relevant_terms_catcher(self, tokenized_sentence):
        relevant_terms = ["dead", "dead fish", "dead dolphins", "dead manatees", "smell", "irritation", "respiratory",
                          "skin", "eye", "throat", "breathing", "asthma", "cough", "beach", "swim/swimming", "seafood",
                          "shellfish", "health", "business/restaurant", "discolored", "color"]
        for word in tokenized_sentence:
            if word in relevant_terms:
                try:
                    self.relevant_terms_dict[word] = self.relevant_terms_dict[word] + 1
                except:
                    self.relevant_terms_dict[word] = 1

    def filter_stopwords(self, tokenized_text_column):
        """
        Function to filter all the stop words and the words that we deem unnecessary/misleading the statistics
        :param tokenized_text_column: a tokenized and partially cleaned text_column from the previous function
        :return: filtered lists of words that should capture the essence of what each row was about
        """
        stop_words = set(stopwords.words('english'))
        filtered_sentences = []
        for sentence in tokenized_text_column:
            filtered_sentence = [w.lower() for w in sentence if not w in stop_words]
            filtered_sentence = [w.lower() for w in filtered_sentence if not w in Locations().get_stop_words()]
            filtered_sentences.append(filtered_sentence)

        return filtered_sentences

    def tweet_popularity_weight(self, file, retweets_weight, replies_weight, likes_weight, quotes_weight,
                                impressions_weight):
        """
        Here I will assign a weight to all the popularity metrics and then compute the average of them, this will
        allow us to rank the tweets
        :param file: The link to the specific CSV File
        :param retweets_weight: User will choose how important retweets are to the overall score
        :param quotes_weight: User will choose how important quotes are to the overall score
        :param likes_weight: User will choose how important likes are to the overall score
        :param replies_weight: User will choose how important replies are to the overall score
        :return: a dataframe containing the tweet ID and the popularity weight and the location of the tweet
                 sorted by popularity
        """
        df = Util().get_file_dataframe(file)

        retweets = "public_metrics.x_retweet_count"
        replies = "public_metrics.x_reply_count"
        likes = "public_metrics.x_like_count"
        quotes = "public_metrics.x_quote_count"
        impressions = "public_metrics.x_impression_count"
        location = "location"

        rows_length = len(df['id'])
        popularity_df = pd.DataFrame(df['id'])
        popularity_df[["popularity_weight"]] = np.random.randint(1, size=(rows_length, 1))
        popularity_df[["county"]] = np.nan

        result = [(row[0], row[1], row[2], row[3], row[4], row[5]) for row in
                  df[[retweets, replies, likes, quotes, impressions, location]].to_numpy()]

        for index, row in enumerate(result):
            total_weighted_metric = row[0] * retweets_weight + row[1] * replies_weight + row[2] * likes_weight + row[
                3] * quotes_weight + row[4] * impressions_weight
            popularity_df.iloc[index, 1] = total_weighted_metric
            popularity_df.iloc[index, 2] = row[5]

        # popularity_df = popularity_df.sort_values(by='popularity_weight', ascending=0)
        return popularity_df

    def top_n_tweets_per_county(self, n, df):
        # Here it only shows the ID, which we can use to get the actual tweet text
        # We can easily add the text body if that's what we want to do
        top3 = df.groupby('county').apply(lambda x: x.nlargest(n, 'popularity_weight')).reset_index(drop=True)
        top3 = top3.sort_values(by='popularity_weight', ascending=0)
        print(top3)



