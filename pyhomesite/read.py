
import pyspark
import pyspark.ml.feature as feature
import pyspark.ml.classification as classification
import pyspark.ml.evaluation as evaluation
from pyspark.sql import SQLContext, Row
import pyspark.sql as sql

import homesite

def main():
    # Main entry point. The function:
    # 1) reads the datafile
    # 2) prepares the data into a dataframe with proper transformation of categorical variables
    # 3) Splits the data into train and test set.
    # 4) Creates a model
    # 5) evaluates the results.
    fn_data = "/datasets/HomesiteKaggle/train.10000.csv"
    fn_output = "/datasets/HomesiteKaggle/train.out"
    khome = homesite.HomesiteKaggle()
    df = khome.read_original_labeled(fn_data)
    df.rdd.saveAsTextFile(fn_output)
    df.rdd.unpersist()





# df = khome.preprocess(rdd)
    # splitted = df.randomSplit(weights=[0.7,0.3])
    # df_train = splitted[0]
    # df_test = splitted[1]
    # print "counts: train", df_train.count(), "test", df_test.count()
    # #    ml_model = bank.train_decision_tree(df)
    # ml_model = bank.train_random_forest(df)
    # bank.predict(ml_model, df_test)
    # rdd.unpersist()

if __name__ == "__main__":
    main()




"""
hadoop fs -rm -r /datasets/HomesiteKaggle/train.out; spark-submit --master yarn-cluster  --num-executors 3 --name Homesite --executor-cores 2 --driver-memory 256M --executor-memory 300M  read.py
"""