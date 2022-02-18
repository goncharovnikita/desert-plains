module Main where

import ClassyPrelude
import Lib
import System.FSNotify
import Control.Concurrent (threadDelay)

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
hasArg = elem

parseArgs :: IO (Either Text (Text, Text, Bool))
parseArgs = do
    args <- getArgs
    let parsedArgs = argsToList args

    case parsedArgs of
        [Src src, Dest dest] -> return $ Right (src, dest, False)
        [Src src, Dest dest, Watch] -> return $ Right (src, dest, True)
        _ -> return $ Left "source and destination must be provided as --src src --dest dest"
    
compilePlain :: (Text, Text) -> IO (Either Text ())
compilePlain (src, dest) = do
    srcFile <- readFileUtf8 $ unpack src

    let parseResult = execTemplateParser parseTemplate srcFile
    case parseResult of
        Left errorText -> return $ Left $ "Error parsing template:" <> errorText
        Right (templ, logged) -> do
            putStr $ concat ["\nCompilation messages:\n", logged, "\n\n"]
            putStr $ concat ["\nCompiled template\n", pack $ show templ, "\n\n"]
            let renderedTemplate = render templ
            putStrLn $ "About to write " <> renderedTemplate
            writeFileUtf8 (unpack dest) renderedTemplate
            return $ Right ()

recompile :: (Text, Text) -> Action
recompile (src, dest) evt =
    case evt of
        Modified fpath _ _ -> do
            putStrLn $ concat ["detected changes in ", pack fpath, ", watching on ", src]
            when (unpack src `isSuffixOf` fpath) $ do
                putStrLn "detected changes, recompiling...\n"
                r <- compilePlain (src, dest)
                case r of
                    Left err -> putStrLn err
                    Right () -> putStrLn "template recompiled"
        _ -> return ()

watchAndRecompile :: (Text, Text) -> IO (Either Text ())
watchAndRecompile (src, dest) = withManager $ \mgr -> do
    -- start a watching job (in the background)
    let doRecompile = recompile (src, dest)
    case getDirFromSrc src of
        Left err -> return $ Left err
        Right dirToWatch -> do
            putStrLn $ "watching dir " <> dirToWatch
            compilePlain (src, dest)

            watchDir
                mgr          -- manager
                (unpack dirToWatch) -- directory to watch
                (const True) -- predicate
                doRecompile  -- action

            -- sleep forever (until interrupted)
            forever $ threadDelay 1000000

getDirFromSrc :: Text -> Either Text Text
getDirFromSrc t =
    let spl = splitElem '/' t
    in case fromNullable spl of
        Nothing -> Left "could not get watch directory from src" 
        Just s -> Right $ intercalate "/" $ init s

runApp :: Either Text (Text, Text, Bool) -> IO (Either Text ())
runApp (Left e) = return $ Left e
runApp (Right (src, dest, watch)) = case watch of
        True -> watchAndRecompile (src, dest)
        False -> compilePlain (src, dest)

main :: IO ()
main = do
    parsedArgs <- parseArgs
    putStrLn "Starting to parse"
    result <- runApp parsedArgs
    case result of
        Left err -> putStrLn err
        Right () -> putStrLn "compilation finished"
