import requests
import pandas as pd
import numpy as np
import io
import re
from nltk.corpus import stopwords
import collections
from Locations import Locations
import csv


class Util:
    geo_tag_dict = collections.defaultdict(int)
    non_geo_hashtags_dict = collections.defaultdict(int)
    pure_locations = Locations().get_pure_locations("combined")
    categorized_locations = Locations().category_adder("combined")

    def get_file_dataframe(self, url):
        """
        :param url: url for a specific CSV file (from the github repo)
        :return: a dataframe of the downloaded csv file
        """
        download = requests.get(url).content
        file_as_dataFrame = pd.read_csv(io.StringIO(download.decode('utf-8')), low_memory=False)
        return file_as_dataFrame

    def extract_hashtags(self, sentence):
        result = []
        for word in sentence:
            if word[0] == '#' and word not in result:
                result.append(word)
        return result

    def geo_tag_harvester(self, list_of_hashtags):
        """
        This was made to look through a sentence at a time
        :param list_of_hashtags: an array of hashatags
        :return: None, it's a counter for location hashtags
        """
        pure_set = set(self.pure_locations[0])
        padded_set = set(self.categorized_locations[0])

        for word in list_of_hashtags:
            caught = False

            for word2 in pure_set:
                if word in word2.lower() or word2.lower() in word:
                    self.geo_tag_dict[word] += 1
                    caught = True
                    break  # Break the loop once caught

            if not caught:
                for word2 in padded_set:
                    if word in word2.lower():
                        self.geo_tag_dict[word] += 1
                        caught = True
                        break  # Break the loop once caught

            if not caught:
                self.non_geo_hashtags_dict[word] += 1

    def filter_hashtags(self, tokenized_sentence):
        """
        Function to filter all the stop words and the words that we deem unnecessary/misleading the statistics
        :param tokenized_sentence: a tokenized and partially cleaned text_column from the previous function
        :return: filtered lists of words that should capture the essence of what each row was about
        """
        result = []
        for w in tokenized_sentence:
            hashtag = w
            pure_word = re.sub(r"#", "", hashtag).lower()
            approved = True
            for word in Locations().get_stop_words(True):
                if word.lower() in pure_word:
                    approved = False
            if approved:
                result.append(pure_word.lower())

        return result

    def tweet_popularity_weight(self, file, retweets_weight, replies_weight, likes_weight, quotes_weight,
                                impressions_weight):
        """
        Here I will assign a weight to all the popularity metrics and then compute the average of them, this will
        allow us to rank the tweets
        :param file:               The link to the specific CSV File
        :param retweets_weight:    User will choose how important retweets are to the overall score
        :param quotes_weight:      User will choose how important quotes are to the overall score
        :param likes_weight:       User will choose how important likes are to the overall score
        :param replies_weight:     user will choose how important replies are to the overall score
        :param impressions_weight: user will choose how important impressions are
        :return:                   a dataframe containing the tweet ID and the popularity weight and the location of
                                   the tweets sorted by popularity
        """
        df = Util().get_file_dataframe(file)

        retweets = "public_metrics.x_retweet_count"
        replies = "public_metrics.x_reply_count"
        likes = "public_metrics.x_like_count"
        quotes = "public_metrics.x_quote_count"
        impressions = "public_metrics.x_impression_count"
        location = "location"
        text = "text"
        date = 'created_at.x'

        rows_length = len(df['id'])
        popularity_df = pd.DataFrame(df['id'])
        popularity_df[["popularity_weight"]] = np.random.randint(1, size=(rows_length, 1))
        popularity_df[["location"]] = np.nan
        popularity_df[["text"]] = np.nan
        popularity_df[["date"]] = np.nan

        result = [(row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7]) for row in
                  df[[retweets, replies, likes, quotes, impressions, location, text, date]].to_numpy()]

        for index, row in enumerate(result):
            total_weighted_metric = row[0] * retweets_weight + row[1] * replies_weight + row[2] * likes_weight + row[
                3] * quotes_weight + row[4] * impressions_weight
            popularity_df.iloc[index, 1] = total_weighted_metric
            popularity_df.iloc[index, 2] = row[5]
            popularity_df.iloc[index, 3] = row[6]
            popularity_df.iloc[index, 4] = row[7]

        # popularity_df = popularity_df.sort_values(by='popularity_weight', ascending=0)
        return popularity_df

    def top_n_tweets_per_county(self, n, df):
        # Here it only shows the ID, which we can use to get the actual tweet text
        # We can easily add the text body if that's what we want to do
        topN = df.groupby('location').apply(lambda x: x.nlargest(n, 'popularity_weight')).reset_index(drop=True)
        topN = topN.sort_values(by='popularity_weight', ascending=0)[:n]
        print(topN)
        return topN

    def frequency_csv_creator(self, dictionary, file_path):
        csv_path = file_path + ".csv"

        data = [{"word": key, "frequency": value} for key, value in dictionary.items()]
        header = data[0].keys()

        with open(csv_path, 'w', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=header)
            writer.writeheader()
            writer.writerows(data)

        print("Frequency CSV file has been created successfully.")

    def tweets_csv_creator(self, df, file_path):
        """
        THIS SHOULD BE THE LAST THING YOU CALL FOR EVERY FILE
        :param df: dataframe containing top n tweets and their weights
        :param file_path: the file path to which we're saving the data
        :return: none.
        """
        csv_path = file_path + "top3Tweets.csv"
        df.to_csv(csv_path, index=False)

        print("Tweets CSV file has been created successfully.")

        print("Clearing cache: ")
        self.geo_tag_dict.clear()
        self.non_geo_hashtags_dict.clear()
        print("geo_Tag_dict:", self.geo_tag_dict)
        print("non_geo_Tag_dict:", self.non_geo_hashtags_dict)

        print("Dictionaries successfully reset.")
        print("-----------------------------------------------------------------")
