
solve :: FilePath -> (String -> a) -> IO a
solve fileName solver = do
    text <- readFile fileName
    return $ solver text