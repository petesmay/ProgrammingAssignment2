# Author: Peter May
# Defines the test suite for execution

library('RUnit')

source('cachematrix.R')

test.suite1 <- defineTestSuite("cachematrix",
                               dirs = file.path("tests"),
                               testFileRegexp = '^\\d+\\.R')

test.result1 <- runTestSuite(test.suite1)

printTextProtocol(test.result1)