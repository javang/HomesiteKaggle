
import pyspark
import pyspark.ml.feature as feature
import pyspark.ml.classification as classification
import pyspark.ml.evaluation as evaluation
from pyspark.sql import SQLContext, Row
from pyspark.sql.types import StringType, StructField, IntegerType, FloatType, StructType

class HomesiteKaggle(object):

    def __init__(self):
        self.sc = None
        self.config()

    def config(self):
        """ Create the appropriate Spark Contexts """
        conf = pyspark.SparkConf()
        self.sc = pyspark.SparkContext(conf=conf)
        self.context = SQLContext(self.sc)

    def read_original_labeled(self, fn):
        data_rdd = self.sc.textFile(fn)
        data_rdd = data_rdd.filter(lambda line: not line.startswith('"QuoteNumber"')).map(read_labeled_example)
        # data_rdd.persist()
        # This string defines the schema (same as header)
        schema_string = "QuoteNumber,Original_Quote_Date,QuoteConversion_Flag,Field6,Field7,Field8,Field9,Field10,Field11,Field12,CoverageField1A,CoverageField1B,CoverageField2A,CoverageField2B,CoverageField3A,CoverageField3B,CoverageField4A,CoverageField4B,CoverageField5A,CoverageField5B,CoverageField6A,CoverageField6B,CoverageField8,CoverageField9,CoverageField11A,CoverageField11B,SalesField1A,SalesField1B,SalesField2A,SalesField2B,SalesField3,SalesField4,SalesField5,SalesField6,SalesField7,SalesField8,SalesField9,SalesField10,SalesField11,SalesField12,SalesField13,SalesField14,SalesField15,PersonalField1,PersonalField2,PersonalField4A,PersonalField4B,PersonalField5,PersonalField6,PersonalField7,PersonalField8,PersonalField9,PersonalField10A,PersonalField10B,PersonalField11,PersonalField12,PersonalField13,PersonalField14,PersonalField15,PersonalField16,PersonalField17,PersonalField18,PersonalField19,PersonalField22,PersonalField23,PersonalField24,PersonalField25,PersonalField26,PersonalField27,PersonalField28,PersonalField29,PersonalField30,PersonalField31,PersonalField32,PersonalField33,PersonalField34,PersonalField35,PersonalField36,PersonalField37,PersonalField38,PersonalField39,PersonalField40,PersonalField41,PersonalField42,PersonalField43,PersonalField44,PersonalField45,PersonalField46,PersonalField47,PersonalField48,PersonalField49,PersonalField50,PersonalField51,PersonalField52,PersonalField53,PersonalField54,PersonalField55,PersonalField56,PersonalField57,PersonalField58,PersonalField59,PersonalField60,PersonalField61,PersonalField62,PersonalField63,PersonalField64,PersonalField65,PersonalField66,PersonalField67,PersonalField68,PersonalField69,PersonalField70,PersonalField71,PersonalField72,PersonalField73,PersonalField74,PersonalField75,PersonalField76,PersonalField77,PersonalField78,PersonalField79,PersonalField80,PersonalField81,PersonalField82,PersonalField83,PersonalField84,PropertyField1A,PropertyField1B,PropertyField2A,PropertyField2B,PropertyField3,PropertyField4,PropertyField5,PropertyField6,PropertyField7,PropertyField8,PropertyField9,PropertyField10,PropertyField11A,PropertyField11B,PropertyField12,PropertyField13,PropertyField14,PropertyField15,PropertyField16A,PropertyField16B,PropertyField17,PropertyField18,PropertyField19,PropertyField20,PropertyField21A,PropertyField21B,PropertyField22,PropertyField23,PropertyField24A,PropertyField24B,PropertyField25,PropertyField26A,PropertyField26B,PropertyField27,PropertyField28,PropertyField29,PropertyField30,PropertyField31,PropertyField32,PropertyField33,PropertyField34,PropertyField35,PropertyField36,PropertyField37,PropertyField38,PropertyField39A,PropertyField39B,GeographicField1A,GeographicField1B,GeographicField2A,GeographicField2B,GeographicField3A,GeographicField3B,GeographicField4A,GeographicField4B,GeographicField5A,GeographicField5B,GeographicField6A,GeographicField6B,GeographicField7A,GeographicField7B,GeographicField8A,GeographicField8B,GeographicField9A,GeographicField9B,GeographicField10A,GeographicField10B,GeographicField11A,GeographicField11B,GeographicField12A,GeographicField12B,GeographicField13A,GeographicField13B,GeographicField14A,GeographicField14B,GeographicField15A,GeographicField15B,GeographicField16A,GeographicField16B,GeographicField17A,GeographicField17B,GeographicField18A,GeographicField18B,GeographicField19A,GeographicField19B,GeographicField20A,GeographicField20B,GeographicField21A,GeographicField21B,GeographicField22A,GeographicField22B,GeographicField23A,GeographicField23B,GeographicField24A,GeographicField24B,GeographicField25A,GeographicField25B,GeographicField26A,GeographicField26B,GeographicField27A,GeographicField27B,GeographicField28A,GeographicField28B,GeographicField29A,GeographicField29B,GeographicField30A,GeographicField30B,GeographicField31A,GeographicField31B,GeographicField32A,GeographicField32B,GeographicField33A,GeographicField33B,GeographicField34A,GeographicField34B,GeographicField35A,GeographicField35B,GeographicField36A,GeographicField36B,GeographicField37A,GeographicField37B,GeographicField38A,GeographicField38B,GeographicField39A,GeographicField39B,GeographicField40A,GeographicField40B,GeographicField41A,GeographicField41B,GeographicField42A,GeographicField42B,GeographicField43A,GeographicField43B,GeographicField44A,GeographicField44B,GeographicField45A,GeographicField45B,GeographicField46A,GeographicField46B,GeographicField47A,GeographicField47B,GeographicField48A,GeographicField48B,GeographicField49A,GeographicField49B,GeographicField50A,GeographicField50B,GeographicField51A,GeographicField51B,GeographicField52A,GeographicField52B,GeographicField53A,GeographicField53B,GeographicField54A,GeographicField54B,GeographicField55A,GeographicField55B,GeographicField56A,GeographicField56B,GeographicField57A,GeographicField57B,GeographicField58A,GeographicField58B,GeographicField59A,GeographicField59B,GeographicField60A,GeographicField60B,GeographicField61A,GeographicField61B,GeographicField62A,GeographicField62B,GeographicField63,GeographicField64"
        # Each field in the schema needs to create a StructField with the proper type
        structfields = [StructField(field_name, get_field_type(field_name), True) for field_name in schema_string.split(",")]
        schema = StructType(structfields)
        df = self.context.createDataFrame(data_rdd, schema)
        return df


    def train_decision_tree(self, dataframe):
        """ Train a decision tree using the provided dataframe. The dataframe must have been prepared in advance.
        :param dataframe: A Spark SQL dataframe
        :return: A trained model
        """
        dtree = classification.DecisionTreeClassifier(featuresCol="features", labelCol="ipep",
                                                      predictionCol="PredictedPep", probabilityCol="Probability",
                                                      rawPredictionCol="RawPredictedPep",
                                                      minInstancesPerNode=5, impurity="gini")
        dtree_model = dtree.fit(dataframe)
        return dtree_model

    def train_random_forest(self, dataframe):
        rforest = classification.RandomForestClassifier(featuresCol="features", labelCol="ipep",
                                                        predictionCol="PredictedPep", probabilityCol="Probability",
                                                        rawPredictionCol="RawPredictedPep",  minInstancesPerNode=5,
                                                        impurity="gini", numTrees=1500,
                                                        featureSubsetStrategy="auto")
        rforest_model = rforest.fit(dataframe)
        return rforest_model


    def predict(self, model, test_dataframe):
        """ Apply a machine learning model to the test_dataframe a output the value

        :param model: A trained model
        :param test_dataframe: The test dataframe. The model is evaluated based on this data
        :return: Nothing. Prints the results.
        """
        df = model.transform(test_dataframe)
        print "***************************** PREDICTIONS               ********************************************"
        toshow = df.select('age', 'income', "mortgage", "pep", "ipep", "PredictedPep", "RawPredictedPep", "Probability")
        toshow.show()
        print "****************************************************************************************************"
        evaluator = evaluation.BinaryClassificationEvaluator(labelCol="ipep", rawPredictionCol="RawPredictedPep")
        print "***************************** EVALUATION                ********************************************"
        area_under_roc = evaluator.evaluate(df, {evaluator.metricName: "areaUnderROC"})
        print "Area under ROC", area_under_roc
        print "****************************************************************************************************"



    def preprocess(self, data_rdd):
        """ Prepare a raw RDD as read from the file and:
            - Creates a dataframe
            - Indexes the categorical variables
            - Creates the vector of features

        :param data_rdd: An Spark RDD
        :return: A Spark SQL dataframe
        """
        df = self.context.createDataFrame(data_rdd)
        print "************************* INPUT DATAFRAME                *******************************************"
        df.show()
        print "****************************************************************************************************"

        # example of indexing a categorical variable using the string indexer, creating the model, and transforming
        # the dataframe
        sidx = feature.StringIndexer(inputCol="save_act", outputCol="isave_act")
        model = sidx.fit(df)
        df = model.transform(df)
        # same thing but creating the model and transforming in one single step
        sidx = feature.StringIndexer(inputCol="mortgage", outputCol="imortgage")
        df = sidx.fit(df).transform(df)
        # transforming a bunch of categorical columns at the same time
        for col in ("sex", "region", "married", "car", "current_act" , "pep"):
            out_col = "i{0}".format(col)
            sidx = feature.StringIndexer(inputCol=col, outputCol=out_col)
            df = sidx.fit(df).transform(df)
        # Build the features vector
        vasmb = feature.VectorAssembler(inputCols=("age", "isex", "iregion", "income",
                                                   "imarried", "children", "icar", "isave_act", "icurrent_act", "imortgage"),
                                        outputCol="features")
        df = vasmb.transform(df)
        print "************************* PREPARED DATAFRAME             *******************************************"
        df.show()
        print "****************************************************************************************************"
        return df

def __del__(self):
    if self.sc is not None:
        self.sc.stop()


def read_labeled_example(line):
    """
    :param line: Process one line of the labeled input file
    :return: A Spark SQL Row object
    """
    sep = ","
    words = line.rstrip().split(sep)
    n = len(words)
    if n > 299: # Hack to recover from commas inside quotes
        new_words=[]
        i = 0
        while i < n:
            if (words[i].startswith('"') and not words[i].endswith('"')) and \
               (words[i+1].endswith('"') and not  words[i+1].startswith('"')):
                new_words.append(words[i] + sep + words[i+1])
                i += 1
            else:
                new_words.append(words[i])
            i += 1
        # if len(new_words) != 299:
        #     print len(words), "========>", words
        #     print len(new_words), "<========", new_words
        return [w.replace('"', '') for w in new_words]
    else:
        return [w.replace('"', '') for w in words]


    return words
    # Create a row
    # row = Row(
    #     QuoteNumber=words[0],
    #     Original_Quote_Date=words[1],
    #     QuoteConversion_Flag=words[2],
    #     Field6=words[3],
    #     Field7=words[4],
    #     Field8=words[5],
    #     Field9=words[6],
    #     Field10=words[7],
    #     Field11=words[8],
    #     Field12=words[9],
    #     CoverageField1A=words[10],
    #     CoverageField1B=words[11],
    #     CoverageField2A=words[12],
    #     CoverageField2B=words[13],
    #     CoverageField3A=words[14],
    #     CoverageField3B=words[15],
    #     CoverageField4A=words[16],
    #     CoverageField4B=words[17],
    #     CoverageField5A=words[18],
    #     CoverageField5B=words[19],
    #     CoverageField6A=words[20],
    #     CoverageField6B=words[21],
    #     CoverageField8=words[22],
    #     CoverageField9=words[23],
    #     CoverageField11A=words[24],
    #     CoverageField11B=words[25],
    #     SalesField1A=words[26],
    #     SalesField1B=words[27],
    #     SalesField2A=words[28],
    #     SalesField2B=words[29],
    #     SalesField3=words[30],
    #     SalesField4=words[31],
    #     SalesField5=words[32],
    #     SalesField6=words[33],
    #     SalesField7=words[34],
    #     SalesField8=words[35],
    #     SalesField9=words[36],
    #     SalesField10=words[37],
    #     SalesField11=words[38],
    #     SalesField12=words[39],
    #     SalesField13=words[40],
    #     SalesField14=words[41],
    #     SalesField15=words[42],
    #     PersonalField1=words[43],
    #     PersonalField2=words[44],
    #     PersonalField4A=words[45],
    #     PersonalField4B=words[46],
    #     PersonalField5=words[47],
    #     PersonalField6=words[48],
    #     PersonalField7=words[49],
        # PersonalField8=words[],
        # PersonalField9=words[],
        # PersonalField10A=words[],
        # PersonalField10B=words[],
        # PersonalField11=words[],
        # PersonalField12=words[],
        # PersonalField13=words[],
        # PersonalField14=words[],
        # PersonalField15=words[],
        # PersonalField16=words[],
        # PersonalField17=words[],
        # PersonalField18=words[],
        # PersonalField19=words[],
        # PersonalField22=words[],
        # PersonalField23=words[],
        # PersonalField24=words[],
        # PersonalField25=words[],
        # PersonalField26=words[],
        # PersonalField27=words[],
        # PersonalField28=words[],
        # PersonalField29=words[],
        # PersonalField30=words[],
        # PersonalField31=words[],
        # PersonalField32=words[],
        # PersonalField33=words[],
        # PersonalField34=words[],
        # PersonalField35=words[],
        # PersonalField36=words[],
        # PersonalField37=words[],
        # PersonalField38=words[],
        # PersonalField39=words[],
        # PersonalField40=words[],
        # PersonalField41=words[],
        # PersonalField42=words[],
        # PersonalField43=words[],
        # PersonalField44=words[],
        # PersonalField45=words[],
        # PersonalField46=words[],
        # PersonalField47=words[],
        # PersonalField48=words[],
        # PersonalField49=words[],
        # PersonalField50=words[],
        # PersonalField51=words[],
        # PersonalField52=words[],
        # PersonalField53=words[],
        # PersonalField54=words[],
        # PersonalField55=words[],
        # PersonalField56=words[],
        # PersonalField57=words[],
        # PersonalField58=words[],
        # PersonalField59=words[],
        # PersonalField60=words[],
        # PersonalField61=words[],
        # PersonalField62=words[],
        # PersonalField63=words[],
        # PersonalField64=words[],
        # PersonalField65=words[],
        # PersonalField66=words[],
        # PersonalField67=words[],
        # PersonalField68=words[],
        # PersonalField69=words[],
        # PersonalField70=words[],
        # PersonalField71=words[],
        # PersonalField72=words[],
        # PersonalField73=words[],
        # PersonalField74=words[],
        # PersonalField75=words[],
        # PersonalField76=words[],
        # PersonalField77=words[],
        # PersonalField78=words[],
        # PersonalField79=words[],
        # PersonalField80=words[],
        # PersonalField81=words[],
        # PersonalField82=words[],
        # PersonalField83=words[],
        # PersonalField84=words[],
        # PropertyField1A=words[],
        # PropertyField1B=words[],
        # PropertyField2A=words[],
        # PropertyField2B=words[],
        # PropertyField3=words[],
        # PropertyField4=words[],
        # PropertyField5=words[],
        # PropertyField6=words[],
        # PropertyField7=words[],
        # PropertyField8=words[],
        # PropertyField9=words[],
        # PropertyField10=words[],
        # PropertyField11A=words[],
        # PropertyField11B=words[],
        # PropertyField12=words[],
        # PropertyField13=words[],
        # PropertyField14=words[],
        # PropertyField15=words[],
        # PropertyField16A=words[],
        # PropertyField16B=words[],
        # PropertyField17=words[],
        # PropertyField18=words[],
        # PropertyField19=words[],
        # PropertyField20=words[],
        # PropertyField21A=words[],
        # PropertyField21B=words[],
        # PropertyField22=words[],
        # PropertyField23=words[],
        # PropertyField24A=words[],
        # PropertyField24B=words[],
        # PropertyField25=words[],
        # PropertyField26A=words[],
        # PropertyField26B=words[],
        # PropertyField27=words[],
        # PropertyField28=words[],
        # PropertyField29=words[],
        # PropertyField30=words[],
        # PropertyField31=words[],
        # PropertyField32=words[],
        # PropertyField33=words[],
        # PropertyField34=words[],
        # PropertyField35=words[],
        # PropertyField36=words[],
        # PropertyField37=words[],
        # PropertyField38=words[],
        # PropertyField39A=words[],
        # PropertyField39B=words[],
        # GeographicField1A=words[],
        # GeographicField1B=words[],
        # GeographicField2A=words[],
        # GeographicField2B=words[],
        # GeographicField3A=words[],
        # GeographicField3B=words[],
        # GeographicField4A=words[],
        # GeographicField4B=words[],
        # GeographicField5A=words[],
        # GeographicField5B=words[],
        # GeographicField6A=words[],
        # GeographicField6B=words[],
        # GeographicField7A=words[],
        # GeographicField7B=words[],
        # GeographicField8A=words[],
        # GeographicField8B=words[],
        # GeographicField9A=words[],
        # GeographicField9B=words[],
        # GeographicField10A=words[],
        # GeographicField10B=words[],
        # GeographicField11A=words[],
        # GeographicField11B=words[],
        # GeographicField12A=words[],
        # GeographicField12B=words[],
        # GeographicField13A=words[],
        # GeographicField13B=words[],
        # GeographicField14A=words[],
        # GeographicField14B=words[],
        # GeographicField15A=words[],
        # GeographicField15B=words[],
        # GeographicField16A=words[],
        # GeographicField16B=words[],
        # GeographicField17A=words[],
        # GeographicField17B=words[],
        # GeographicField18A=words[],
        # GeographicField18B=words[],
        # GeographicField19A=words[],
        # GeographicField19B=words[],
        # GeographicField20A=words[],
        # GeographicField20B=words[],
        # GeographicField21A=words[],
        # GeographicField21B=words[],
        # GeographicField22A=words[],
        # GeographicField22B=words[],
        # GeographicField23A=words[],
        # GeographicField23B=words[],
        # GeographicField24A=words[],
        # GeographicField24B=words[],
        # GeographicField25A=words[],
        # GeographicField25B=words[],
        # GeographicField26A=words[],
        # GeographicField26B=words[],
        # GeographicField27A=words[],
        # GeographicField27B=words[],
        # GeographicField28A=words[],
        # GeographicField28B=words[],
        # GeographicField29A=words[],
        # GeographicField29B=words[],
        # GeographicField30A=words[],
        # GeographicField30B=words[],
        # GeographicField31A=words[],
        # GeographicField31B=words[],
        # GeographicField32A=words[],
        # GeographicField32B=words[],
        # GeographicField33A=words[],
        # GeographicField33B=words[],
        # GeographicField34A=words[],
        # GeographicField34B=words[],
        # GeographicField35A=words[],
        # GeographicField35B=words[],
        # GeographicField36A=words[],
        # GeographicField36B=words[],
        # GeographicField37A=words[],
        # GeographicField37B=words[],
        # GeographicField38A=words[],
        # GeographicField38B=words[],
        # GeographicField39A=words[],
        # GeographicField39B=words[],
        # GeographicField40A=words[],
        # GeographicField40B=words[],
        # GeographicField41A=words[],
        # GeographicField41B=words[],
        # GeographicField42A=words[],
        # GeographicField42B=words[],
        # GeographicField43A=words[],
        # GeographicField43B=words[],
        # GeographicField44A=words[],
        # GeographicField44B=words[],
        # GeographicField45A=words[],
        # GeographicField45B=words[],
        # GeographicField46A=words[],
        # GeographicField46B=words[],
        # GeographicField47A=words[],
        # GeographicField47B=words[],
        # GeographicField48A=words[],
        # GeographicField48B=words[],
        # GeographicField49A=words[],
        # GeographicField49B=words[],
        # GeographicField50A=words[],
        # GeographicField50B=words[],
        # GeographicField51A=words[],
        # GeographicField51B=words[],
        # GeographicField52A=words[],
        # GeographicField52B=words[],
        # GeographicField53A=words[],
        # GeographicField53B=words[],
        # GeographicField54A=words[],
        # GeographicField54B=words[],
        # GeographicField55A=words[],
        # GeographicField55B=words[],
        # GeographicField56A=words[],
        # GeographicField56B=words[],
        # GeographicField57A=words[],
        # GeographicField57B=words[],
        # GeographicField58A=words[],
        # GeographicField58B=words[],
        # GeographicField59A=words[],
        # GeographicField59B=words[],
        # GeographicField60A=words[],
        # GeographicField60B=words[],
        # GeographicField61A=words[],
        # GeographicField61B=words[],
        # GeographicField62A=words[],
        # GeographicField62B=words[],
        # GeographicField63=words[],
        # GeographicField64=words[]
        #
    # )
    # return row


def get_field_type(field_name):
    if field_name in ["GeographicField64"]:
        return StringType()
    # elif field_name in ["Field6"]:
    #     return FloatType()
    return StringType()
