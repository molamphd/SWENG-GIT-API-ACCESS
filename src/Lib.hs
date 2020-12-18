{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

-- imports
import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

someFunc :: IO ()
someFunc = do
  putStrLn "Calling Github... "
  (rName:user:token:_) <- getArgs
  putStrLn $ "Name is " ++ rName
  putStrLn $ "GitHub account for API call is " ++ user
  putStrLn $ "GitHub token for API call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)
  
  testGitHubCall auth $ pack rName
  putStrLn "Finished."


testGitHubCall :: BasicAuthData -> Text -> IO ()
-- gets details for the username provided via the command line parameter 
testGitHubCall auth name = 
  (SC.runClientM (GH.getUser (Just "haskell-app") auth name) =<< env) >>= \case

    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right res -> do
      putStrLn $ "Results: " ++ show res
      
      -- get the user's repos
      (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "Error getting user's repos: " ++ show err
        Right repos -> do
          putStrLn $ "User's repos are: " ++ "\n\t" ++
            intercalate "\n\t" (map (\(GH.GitHubRepo n f l s i c u ) -> "[" ++ show n ++ ", " ++ show f ++ ", " ++ show l ++ ", " ++ show s ++ ", " ++ show i ++ ", " ++ show c ++ ", " ++ show u ++ "]") repos)

          -- get the list of contributors to repos
          partitionEithers <$> mapM (getContribs auth name) repos >>= \case

            ([], contribs) -> 
              putStrLn $ "Contributors to the repos are: " ++ "\n\t" ++
              (intercalate "\n\t" .
               map (\(GH.RepoContributor n c) -> "[" ++ show n ++ ", " ++ show c ++ "]") .
               groupContributors $ concat contribs)

	    (ers, _)-> do
              putStrLn $ "Error getting contributors: " ++ show ers

	-- get list of languages specified used in repos and amount of code (in bytes) for each
	 -- partitionEithers <$> mapM (getLanguages auth name) repos >>= \case

         -- ([], languages) -> 
            -- putStrLn $ "languages used in repos and amount of code for each (in bytes): " ++ "\n\t" ++
             -- (intercalate "\n\t" .
               -- map (\(GH.RepoLanguages c p j h) -> "[" ++ show c ++ ", " ++ show p ++ ", " ++ show j ++ ", " ++ show h ++ "]") languages)

	   -- (errs, _)-> do
            -- putStrLn $ "Error getting languages: " ++ show errs
                
           
     
  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _ _ _ _ _) =
          SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth name repo) =<< env

	-- groups contributors by number of commits in ascending order
        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor _ c1) (GH.RepoContributor _ c2) ->  compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) ->  l1 == l2)
         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor
               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l . sum $ 
                                                       map (\(GH.RepoContributor _ c) -> c)  xs

        -- getLanguages :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoLanguages])
        -- getLanguages auth name (GH.GitHubRepo repo _ ) =
        -- SC.runClientM (GH.getRepoLanguages (Just "haskell-app") auth name repo) =<< env


              
                
          



       
