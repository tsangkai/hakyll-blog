--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List (elemIndex, intercalate)
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "templates/*" $ compile templateBodyCompiler

    match "image/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagPostCtx = 
                    constField "title" ("Tagged: #" ++ tag) <>
                    postListCtx posts tags

            makeItem ""
                >>= applyAsTemplate tagPostCtx
                >>= loadAndApplyTemplate "templates/tag.html"     tagPostCtx
                >>= loadAndApplyTemplate "templates/default.html" tagPostCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"

        compile $ do
            let postsPattern = "posts/*"
            
            pandocOut <- pandocCompiler
            _ <- saveSnapshot "content" pandocOut
            
            let postCtxWithNav =
                    postCtx tags <> 
                    postNavigationCtx postsPattern

            loadAndApplyTemplate "templates/post.html"    postCtxWithNav pandocOut
                >>= loadAndApplyTemplate "templates/default.html" postCtxWithNav
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            all_tags <- myRenderTagList tags

            let archiveCtx =
                    constField "all-tags" all_tags  <>
                    constField "title" "Archives" <>
                    postListCtx posts tags

            makeItem ""
                >>= applyAsTemplate archiveCtx
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            recentPosts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexCtx = postListCtx recentPosts tags

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls



--------------------------------------------------------------------------------


myRenderTagList :: Tags -> Compiler String
myRenderTagList = renderTags makeLink unlines
  where
    makeLink tag url count _ _ = renderHtml $ renderTagLink tag url

-- | Render tags with links
myTagsField :: String     -- ^ Destination key
            -> Tags       -- ^ Tags
            -> Context a  -- ^ Context
myTagsField = tagsFieldWith getTags mySimpleRenderLink mconcat

-- | Render one tag link
mySimpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
mySimpleRenderLink _   Nothing         = Nothing
mySimpleRenderLink tag (Just filePath) = Just $ renderTagLink tag (toUrl filePath)

-- | Render one tag link html
renderTagLink :: String -> String -> H.Html
renderTagLink tag url =
    H.a ! A.title (H.stringValue ("All pages with tag # "++tag++"."))
        ! A.href (toValue url)
        ! A.class_ "badge"
        $ toHtml ("# " ++ tag)

-- | Context provider for a single post
postCtx :: Tags -> Context String
postCtx tags =
    myTagsField "tags" tags          <>
    dateField "date" "%B %e, %Y"   <>
    teaserField "teaser" "content" <> 
    defaultContext

-- | Context provider for post teaser
postListCtx :: [Item String] -> Tags -> Context String
postListCtx posts tags = 
    listField "posts" (postCtx tags) (return posts) <>
    modificationTimeField "modified" "%Y-%m-%d %H:%M:%S" <>
    defaultContext

-- | Context provider for Next and Previous post links
postNavigationCtx :: Pattern -> Context String
postNavigationCtx postPattern = 
    field "nextPostUrl"   (getNavUrl (>))   <>
    field "nextPostTitle" (getNavTitle (>)) <>
    field "prevPostUrl"   (getNavUrl (<))   <>
    field "prevPostTitle" (getNavTitle (<))
  where
    -- Helper to get sorted identifiers
    getSortedIds :: Compiler [Identifier]
    getSortedIds = do
        -- CRITICAL: Load from the snapshot ("content") instead of the rule pattern directly
        posts <- loadAllSnapshots postPattern "content" :: Compiler [Item String]
        sorted <- chronological posts
        return $ map itemIdentifier sorted

    -- Helper to shift the current item index by a comparison function

    getNeighborId :: (Int -> Int -> Bool) -> Compiler (Maybe Identifier)
    getNeighborId comp = do
        currentId <- getUnderlying
        sortedIds <- getSortedIds
        case elemIndex currentId sortedIds of
            Just idx -> return $ lookupNeighbor idx sortedIds
            Nothing  -> return Nothing
      where
        lookupNeighbor idx ids
            | comp 1 0 && idx < length ids - 1 = Just (ids !! (idx + 1)) -- Next post
            | comp 0 1 && idx > 0              = Just (ids !! (idx - 1)) -- Prev post
            | otherwise                        = Nothing

    -- Retrieves the calculated neighboring URL
    getNavUrl :: (Int -> Int -> Bool) -> Item String -> Compiler String
    getNavUrl comp item = do
        neighborId <- getNeighborId comp
        case neighborId of
            Just ident -> do
                route <- getRoute ident
                case route of
                    Just r  -> return $ toUrl r
                    Nothing -> noResult "No route for neighbor"
            Nothing -> noResult "No neighboring post available"

    -- Retrieves the calculated neighboring title
    getNavTitle :: (Int -> Int -> Bool) -> Item String -> Compiler String
    getNavTitle comp item = do
        neighborId <- getNeighborId comp
        case neighborId of
            Just ident -> getMetadataFieldURL ident "title"
            Nothing    -> noResult "No neighboring post available"

    -- Helper for reading metadata cleanly
    getMetadataFieldURL ident key = do
        meta <- getMetadata ident
        case lookupString key meta of
            Just val -> return val
            Nothing  -> noResult $ "Missing " ++ key ++ " field"
