import json
import boto3 
from snscrape.modules.twitter import TwitterTweetScraper,TwitterTweetScraperMode

s3 = boto3.client('s3')
bucket_name = 'twitter-thesis-data'
def get_specific_tweet(tweet_id):
    tweets_list = []
    try:
        for i,tweet in enumerate(TwitterTweetScraper(tweetId=tweet_id,mode=TwitterTweetScraperMode.SINGLE).get_items()):
            return json.loads(tweet.json())
    except Exception as e:
        return {"error": str(e) , "id": tweet_id}
        
def lambda_handler(event, context):
    
    for key, value in event.items():
        pass
    print(f'爬取分段为：{key}')
    key = 'output/'+ key.replace('-', '/',2) + '.json'
    error = 'error/'+key
    
    error_list = []
    successful = []
    
    print(f'开始爬取：{len(value)} 条')

    for i in value:


        res = get_specific_tweet(i)
        if res is None:
            continue
        if "error" in res:
            error_list.append(res)
            continue
        successful.append(res)
    
    successful_bytes = json.dumps(successful).encode('utf-8')
    s3.put_object(Bucket=bucket_name, Key=key, Body=successful_bytes)
    
    if error_list:
        error_bytes = json.dumps(error_list).encode('utf-8')
        s3.put_object(Bucket=bucket_name, Key=error, Body=error_bytes)
    print(f'完成分段：{key}')