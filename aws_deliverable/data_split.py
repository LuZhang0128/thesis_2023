import sys
import getopt
import pandas as pd
import os 
import math

# 获取CLI参数
def input_file(argv):
    inputfile = ''
    split_rows = 500000

    try:
        opts, args = getopt.getopt(argv,"hi:s:",["ifile=","split_rows="])
    except getopt.GetoptError:
        print("data_split.py -i <inputfile> -s <split_rows>")
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print("test.py -i <inputfile> -s <split_rows>")
            sys.exit()
        elif opt in ("-i", "--ifile"):
            inputfile = arg
        elif opt in ("-s", "--split_rows"):
            split_rows = int(arg)
        else :
            print("test.py -i <inputfile> -s <split_rows>")
            break
    else:
        return inputfile,split_rows
    
def split_file(inputfile, split_rows):
    df = pd.read_csv(inputfile)
    basename, _ = os.path.splitext(os.path.basename(inputfile))  
    output = f'split/{basename}'
    # 创建存储路径
    if not os.path.exists('split'):
        os.mkdir('split')
        os.mkdir(output)
    elif not os.path.exists(output):
        os.mkdir(output)

    # 判断该文件是否需要分割
    if df.shape[0] < split_rows:
        print(f"{basename}rows < split_rows")
        df.to_csv(f'{output}/{basename}.csv', index=False, header=True)
        return 
    
    # 分割文件
    group_size = split_rows
    n_groups = math.ceil(df.shape[0] / group_size)
    groups = [df.iloc[i*group_size:(i+1)*group_size] for i in range(n_groups)] 
    for i in range(len(groups)):
        groups[i].to_csv(f'{output}/{basename}-{i}.csv', index=False, header=True)
    else: 
        print(f"{basename} split ok")
    return   

if __name__ == "__main__":
    inputfile, split_rows = input_file(sys.argv[1:])
    print(f"file_path: {inputfile} , split_rows: {split_rows}")
    split_file(inputfile, split_rows)