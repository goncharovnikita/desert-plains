module Main where

import ClassyPrelude
import Lib
import System.FSNotify
import Control.Concurrent (threadDelay)
-- import Control.Monad (forever)

testTemplate :: Text
testTemplate = "\
\[head]\n\
\   [title \"Desert plains test page\"]\n\
\[head]\n\
\\
\[body]\
\\"Hello there, in body!\"\
\[h1 \"Some very nice title here\" [h1 \"Embedded title\"]]\
\[body]\
\"

parseCommand :: [Text] -> Maybe (Arg, [Text])
parseCommand ("--src":src:xs) = Just (Src src, xs)
parseCommand ("--dest":dest:xs) = Just (Dest dest, xs)
parseCommand ("--watch":xs) = Just (Watch, xs)
parseCommand _ = Nothing

argsToList :: [Text] -> [Arg]
argsToList list = case parseCommand list of
    Nothing -> []
    Just (arg, xs) -> arg:argsToList xs

hasArg :: Arg -> [Arg] -> Bool
hasArg arg = any (== arg)

parseArgs :: IO (Either Text (Text, Text, Bool))
parseArgs = do
    args <- getArgs
    let parsedArgs = argsToList args

    case parsedArgs of
        [Src src, Dest dest] -> return $ Right (src, dest, False)
        [Src src, Dest dest, Watch] -> return $ Right (src, dest, True)
        _ -> return $ Left "source and destination must be provided as --src src --dest dest"
    
compilePlain :: (Text, Text) -> IO ()
compilePlain (src, dest) = do
    srcFile <- readFileUtf8 $ unpack src

    let parseResult = execTemplateParser parseTemplate srcFile
    case parseResult of
        Left errorText -> putStr $ "Error parsing template:" <> errorText
        Right (templ, logged) -> do
            putStr $ concat ["\nCompilation messages:\n", logged, "\n\n"]
            putStr $ concat ["\nCompiled template\n", pack $ show templ, "\n\n"]
            let renderedTemplate = render templ
            putStrLn $ "About to write " <> renderedTemplate
            writeFileUtf8 (unpack dest) $ renderedTemplate

recompile :: (Text, Text) -> Action
recompile (src, dest) evt =
    case evt of
        Modified fpath _ _ ->
            when (".desertp" `isSuffixOf` fpath) $ do
                putStrLn "detected changes, recompiling...\n"
                compilePlain (src, dest)
        _ -> return ()

watchAndRecompile :: (Text, Text) -> IO ()
watchAndRecompile (src, dest) = withManager $ \mgr -> do
    -- start a watching job (in the background)
    let doRecompile = recompile (concat [src, "/", "index.desertp"], dest)

    compilePlain (concat [src, "/", "index.desertp"], dest)

    watchDir
        mgr          -- manager
        (unpack src) -- directory to watch
        (const True) -- predicate
        doRecompile  -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

main :: IO ()
main = do
    putStrLn "Starting to parse"
    parsedArgs <- parseArgs
    case parsedArgs of
        Left errMsg -> print errMsg
        Right (src, dest, watch) -> do
            case watch of
                True -> do
                    watchAndRecompile (src, dest)
                _ -> do
                    compilePlain (concat [src, "/", "index.desertp"], dest)
    -- putStrLn $ fromList $ show (runTemplateParser (parseTemplate) testTemplate)
