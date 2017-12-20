{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts, NoImplicitPrelude #-}

module Seven
  ( Name
  , Program (..)
  , buildProgramMap
  , buildTree
  , findBottom
  , findUnbalanced
  , supported
  , supports
  ) where

-- import Control.Lens
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Protolude

type Name = [Char]
type Weight = Int

data Program = Program
  { _name :: Name
  , _weight :: Int
  , _disc :: Maybe [Name]
  } deriving (Show, Eq)

data ProgramTree = ProgramTree
  { _tRoot :: Name
  , _tWeight :: Weight
  , _tLeaves :: [ProgramTree]
  } deriving (Show)

type ProgramMap = Map Name Program

--makeLenses ''Program

flatten :: (Foldable t, Foldable t1) => t1 (t a) -> [a]
flatten zs = (\z n -> foldr (\x y -> foldr z y x) n zs) (:) []

supported :: Map Name (Maybe [Name]) -> Set Name
supported programSupports = Set.fromList $ flatten $ flatten $ Map.elems programSupports

supports :: [Program] -> Map Name (Maybe [Name])
supports programs = foldl mapReduce (Map.empty) $ withChildren programs
  where
    mapReduce acc cur = Map.insert (_name cur) (_disc cur) acc
    withChildren = filter (\p -> case (_disc p) of
                                   Just _ -> True
                                   Nothing -> False)

findBottom :: [Program] -> Maybe Name
findBottom [] = Nothing
findBottom programs = head $ filter (\n -> Set.notMember n supported') $ Map.keys supports'
  where
    supports' = supports programs
    supported' = supported supports'

buildProgramMap :: [Program] -> ProgramMap
buildProgramMap programs = foldl mapReduce Map.empty programs
  where
    mapReduce m p = Map.insert (_name p) p m

buildTree :: ProgramMap -> ProgramTree
buildTree m = buildLeaves root
  where
    allChildren = Set.fromList
                  $ flatten
                  $ flatten
                  $ map (\p -> (_disc p))
                  $ Map.elems m
    root = Set.findMax $ Map.keysSet m `Set.difference` allChildren
    buildLeaves p = ProgramTree p weight (buildLeaves <$> children)
      where
        p' = m ! p
        weight = (_weight p')
        children = case (_disc p') of
                     Just cs -> cs
                     Nothing -> []

findUnbalanced :: ProgramTree -> Maybe Weight
findUnbalanced (ProgramTree _ _ leaves) = listToMaybe badLeaves <|> unbalanced
  where
    badLeaves = mapMaybe findUnbalanced leaves
    weights = Map.fromListWith (++)
              $ map (\t -> (stackWeight t, [_tWeight t]))
              $ toList leaves
    stackWeight (ProgramTree _ weight ls) = weight + sum (stackWeight <$> ls)
    unbalanced = case sortOn (length . snd) (Map.toList weights) of
                   [] -> Nothing
                   [_] -> Nothing
                   [(t1, [w]), (t2, _)] -> Just (w + (t2 - t1))
                   _ -> panic "More than one error detected"
