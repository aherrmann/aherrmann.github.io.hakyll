module Adjacent (

  prevNextByDateFields

) where


import           Control.Applicative       (empty)
import           Data.Hashable             (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict       as HM
import           Hakyll


-- | Construct previous/next fields sorted by date.
--
-- @prevNextByDateFields key pat@ returns an action that produces a context
-- that contains two fields @"prev" ++ key@, @"next" ++ key@.
-- Each of these fields contains a url (or @empty@) to the previous, next item
-- according to the pattern @pat@ in chronological order.
--
-- Credit to Tim DuBois [https://github.com/Libbum/AxiomaticSemantics].
-- This implementation of an adjacent items context is strongly influenced by his.
-- His version is available under the above link licensed under the MIT license.
prevNextByDateFields :: MonadMetadata m => String -> Pattern -> m (Context String)
prevNextByDateFields key pat = do
  idents <- sortChronological =<< getMatches pat
  let (prevMap, nextMap) = buildPrevNextMap idents
  pure . mconcat $
    [ field ("prev" ++ key) (lookupAdjacentUrl prevMap)
    , field ("next" ++ key) (lookupAdjacentUrl nextMap) ]


type AdjacencyMap = HM.HashMap Identifier Identifier


instance Hashable Identifier where
    hashWithSalt salt = hashWithSalt salt . show


buildPrevNextMap :: [Identifier] -> (AdjacencyMap, AdjacencyMap)
buildPrevNextMap idents =
  ( buildMap (tail idents) idents
  , buildMap idents (tail idents) )
  where
    buildMap ks vs = foldr (uncurry HM.insert) HM.empty $ zip ks vs


lookupAdjacentUrl :: AdjacencyMap -> Item String -> Compiler String
lookupAdjacentUrl adjMap item = do
  let ident = itemIdentifier item
      mAdjIdent = HM.lookup ident adjMap
  mRoute <- maybe empty getRoute mAdjIdent
  pure $ maybe empty toUrl mRoute
