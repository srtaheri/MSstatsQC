test_that("camelCaseSplit splits the camel Case correctly",{
  expect_equal(camelCaseSplit("HelloWorld"),"Hello world")
})

test_that("punc_remove removes all punctuations from a string",{
  expect_equal(punc_remove("Best_RT"),"Best RT")
})

test_that("clearString changes all upper level letters to lower
          case and removes the punctuations",{
            expect_equal(clearString("thisIs_MSstatsQC.Package"),
                         "this is msstats qc package")
          })

test_that("guessColumnName is returning the column names that we want",{
  expect_equal(guessColumnName("prucurs"),"Precursor")
  expect_equal(guessColumnName("minsttime"),"MinStartTime")
  expect_equal(guessColumnName("Best.RT"),"Best.RT")
  expect_equal(guessColumnName("aqired.time"),"AcquiredTime")
  expect_equal(guessColumnName("max.time"),"MaxEndTime")
  expect_equal(guessColumnName("wdrft"),"wdrft")

})

test_that("CUSUM.data.prepare is working correctly",{
  metricData <- getMetricData(data = S9Site54, peptide = "VLVLDTDYK", L = 1, U = 5,
                              metric = "Best.RT", normalization = FALSE,
                              selectMean = NULL, selectSD = NULL)
  df_func <- CUSUM.data.prepare(data = S9Site54, metricData = metricData, "VLVLDTDYK", "mean")[1:5,]
  df_func$Annotations <- NA
  # I create the first 5 rows of CUSUM.data.prepare by hand
  df <- data.frame(QCno = seq(1,5), CUSUM.poz = c(0.00,24.20,48.23,72.32,96.44),
                   CUSUM.neg = c(0,0,0,0,0), Annotations = c(1,1,1,1,1),
                   outRangeInRangePoz = c("InRangeCUSUM+","OutRangeCUSUM+","OutRangeCUSUM+","OutRangeCUSUM+","OutRangeCUSUM+"),
                   outRangeInRangeNeg = c("InRangeCUSUM-","InRangeCUSUM-","InRangeCUSUM-","InRangeCUSUM-","InRangeCUSUM-"))
  df$Annotations <- NA
  expect_equal(df_func,df)
})

# test_that("CP.data.prepare is working correctly",{
#   metricData <- getMetricData(data = S9Site54, peptide = "VLVLDTDYK", L = 1, U = 5,
#                               metric = "Best.RT", normalization = FALSE,
#                               selectMean = NULL, selectSD = NULL)
#   df_func <- CP.data.prepare(data = S9Site54, metricData = metricData, type = "mean")[1:5,]
#   df_func$Et <- floor(df_func$Et)
#   # I create the first 5 rows of CUSUM.data.prepare by hand
#   df <- data.frame(QCno = seq(1,5), Et = c(27444.17,26834.08,26232.39,25627.73,25021.59),
#                    tho.hat = c(1,1,1,1,1)
#                    )
#   expect_equal(df_func,df)
# })

test_that("get_CP_tho.hat first ten rows for reten time works well",{
  df <- c(get_CP_tho.hat(data = S9Site54, L = 1, U = 5, data.metrics = c("Best.RT"), NULL,NULL)[1:10,1])
  expect_equal(df, c(5,45,5,45,5,45,38,45,16,45))
})
