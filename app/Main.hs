module Main where
import System.Environment
import Network.URI
import Data.List.Split
import System.Process


main = do
  args <- getArgs
  case args of
    [x] -> genLink x format0
    [x, "--f1"] -> genLink x format1
    [x, "--f2"] -> genLink x format2
    _ -> putStrLn "org-links: invalid arguments"

genLink url format_fun = do
    case parseURI url of
        Nothing  -> error "org-links: invalid URI"
        Just uri -> do
            case uriAuthority uri of
                Nothing -> error "org-links: no Authority"
                Just auth -> do
                    case uriRegName auth of
                      "github.com" -> do
                        let list = splitOn "/" $ Prelude.drop 1 $ uriPath uri
                            query_type = case list!!2 of
                                           "issues" -> "issue"
                                           "pull" -> "pr"
                        case length list of
                          2 -> putStrLn $ "[[" <> url <> "][" <> list!!0 <> "/" <> list!!1 <> "]]"
                          4 -> do 
                            title <- readProcess "gh" [query_type, "view", url, "--json", "title", "-q", ".title"] ""
                            putStrLn $ format_fun url title list
                          _ -> putStrLn "org-links: url must point to github repo/pr/issue"
                      _ -> do
                        putStrLn "org-links: url must point to github repo/pr/issue"



format0 url title list = "["
  <> "["
  <> url
  <> "]"
  <> "["
  <> list!!0 
  <> "/" 
  <> list!!1 
  <> " | "
  <> (case list!!2 of
         "issues" -> "Issue" <> replicate (4 - (length $ list!!3)) ' '
         "pull" -> "Pull" <> replicate (5 - (length $ list!!3)) ' '
         _ -> "_"
     )
  <> list!!3
  <> " · "
  <> (Prelude.take ((Prelude.length title) - 1) title)
  <> "]"
  <> "]"

format1 url title list = "["
  <> "["
  <> url
  <> "]"
  <> "["
  <> list!!0 
  <> "/" 
  <> list!!1 
  <> (case list!!2 of
         "issues" -> " | Issue #"
         "pull" -> " | PR #"
         _ -> "_"
     )
  <> list!!3
  <> " | "
  <> (Prelude.take ((Prelude.length title) - 1) title)
  <> "]"
  <> "]"

format2 url title list = "["
  <> "["
  <> url
  <> "]"
  <> "["
  <> (Prelude.take ((Prelude.length title) - 1) title)
  <> " · "
  <> (case list!!2 of
         "issues" -> "Issue #"
         "pull" -> "PR #"
         _ -> "_"
      )
  <> list!!3
  <> " · "
  <> list!!0 
  <> "/" 
  <> list!!1 
  <> "]"
  <> "]"
