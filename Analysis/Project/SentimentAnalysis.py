import pandas as pd
from util import Util
from datetime import datetime
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# Create an instance of SentimentIntensityAnalyzer
sent_analyzer = SentimentIntensityAnalyzer()


def format_data(file):
    df = Util().get_file_dataframe(file)
    selected_columns = df[['created_at.x', 'text']]
    # Assuming your DataFrame is called df and the date column is named 'date_column'
    for index, row in selected_columns.iterrows():
        old_date = row['created_at.x']
        new_date = datetime.strptime(old_date, '%Y-%m-%d %H:%M:%S').strftime('%Y-%m')
        selected_columns.at[index, 'created_at.x'] = new_date

    return selected_columns


def export_sentiment(df, file_name):
    for index, row in df.iterrows():
        text = row['text']
        sentiment_scores = sent_analyzer.polarity_scores(text)
        sentiment = sentiment_scores['compound']
        df.at[index, 'Sentiment'] = sentiment

    df.to_csv(file_name + '_sentiment_output.csv', index=False)
