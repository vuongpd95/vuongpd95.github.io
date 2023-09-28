--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Debug.Trace
import           qualified I18n

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    {
        destinationDirectory = "docs"
    }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "about.html" $ do
        route idRoute
        compile $ do
            let aboutCtx =
                    youtubeInfo `mappend`
                    headerInfo `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    headerInfo `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
lang :: String
lang = "vi"

headerInfo =
    constField "aboutPageTitle" (I18n.t lang "about") `mappend`
    constField "aboutPageUrl" "/about.html" `mappend`
    constField "homePageTitle" (I18n.t lang "home") `mappend`
    constField "homePageUrl" "/"


youtubeInfo =
    constField "youtubeChannelUrl" "https://www.youtube.com/channel/UCxYPvrOjEWnuQoisxmmaBeg" `mappend`
    constField "youtubeChannelName" "@7Ngay1Tuan" `mappend`
    constField "youtubeChannelWhy" "/posts/2023-09-19-why-I-have-a-youtube-channel.html"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    headerInfo `mappend`
    defaultContext
