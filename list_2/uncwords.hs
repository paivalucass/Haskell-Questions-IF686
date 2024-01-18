


main = do
sentence_1 <- getLine
sentence_2 <- getLine
let result = uncommonFromTwoSentences sentence_1 sentence_2
print result