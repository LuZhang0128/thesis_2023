import json
import pandas as pd
import math
import os
import boto3

client = boto3.client('stepfunctions')

def lambda_handler(event, context):
    print(event)
    
    bucket = event['Records'][0]['s3']['bucket']['name']
    obj = event['Records'][0]['s3']['object']['key']
    basename, _ = os.path.splitext(os.path.basename(obj))
    
    df = pd.read_csv(f's3://{bucket}/{obj}')
    group_size = 1500
    print(f'一共 {df.shape[0] } 条')
    print(f'每次最大爬取 {group_size} 条')
    n_groups = math.ceil(df.shape[0] / group_size)
    print(f'执行并发: {n_groups} 次')
    
    run_size = 5
    r_groups = math.ceil(n_groups / run_size)
    print(f'执行 {r_groups} 次 Step Functions, 每次最大并发 {run_size}')
    groups = [{f'{basename}-{i}':df["status_id"].iloc[i*group_size:(i+1)*group_size].tolist()} for i in range(n_groups)] 
    run_groups = [ groups[i * run_size : ( i + 1 ) * run_size] for i in range(r_groups) ]
    print(len(run_groups))
    
    for  j in run_groups:
        input_data = {
            "data": j
        }
        response = client.start_execution(
          stateMachineArn='arn:aws:states:us-east-1:190312862683:stateMachine:tweet_state_machines',
          input=json.dumps(input_data)
        )
        print(str(response))
    
