--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Debug.Trace

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

    create ["about.html"] $ do
        route idRoute
        compile $ do
            let ctx =
                    constField "title" "About" `mappend`
                    youtubeInfo `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/about.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
youtubeInfo =
    constField "youtubeChannelUrl" "https://www.youtube.com/channel/UCxYPvrOjEWnuQoisxmmaBeg" `mappend`
    constField "youtubeChannelName" "@7Ngay1Tuan" `mappend`
    constField "youtubeChannelWhy" "/posts/2023-09-19-why-I-have-a-youtube-channel.html"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
