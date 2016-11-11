{-| The Quasiquoter for the reflex-jsx language

    The only import you need is "jsx", which is the quasiquoter. See the README
    for more information.
-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module ReflexJsx.QQ
       ( jsx
       ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad

import qualified Reflex.Dom as Dom

import ReflexJsx.Parser

import Prelude hiding (exp)

{-| Quasiquoter for jsx-like expressions

    Used like "[jsx| <div /> |]"
-}
jsx :: QuasiQuoter
jsx = QuasiQuoter
  { quoteExp = quoteJsxExpression
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = undefined
  }


quoteJsxExpression :: String -> TH.ExpQ
quoteJsxExpression str = do
  exp <- parseJsx str
  outputWidgetCode exp


outputWidgetCode :: Node -> TH.ExpQ
outputWidgetCode node =
  case node of
    Node tag attrs children -> outputNode tag attrs children
    Text content -> [| Dom.text content |]
    SplicedNode expr -> spliced expr

spliced :: String -> TH.ExpQ
spliced expr = do
  let Right parsedExpr = parseExp expr
  return parsedExpr

outputNode :: String -> Attrs -> [Node] -> TH.ExpQ
outputNode tag attrs children = do
  childrenStmts <- mapM outputNodeWithBindings children
  let allChildrenBindings = return $ TH.TupE $ retrieveBindingVars childrenStmts
  returnStmt <- TH.NoBindS <$> [|return $(allChildrenBindings)|]
  let doExpression = return $ TH.DoE $ childrenStmts ++ [returnStmt]
  case attrs of
    StaticAttrs staticAttrs -> do
      let stringAttrs = TH.listE $ List.map toStringAttr staticAttrs
      [| Dom.elAttr tag (Map.fromList $(stringAttrs)) $ $(doExpression) |]
    SplicedAttrs attrExpr -> do
      let Right exp = parseExp attrExpr
      [| Dom.elDynAttr tag $(return exp) $ $(doExpression) |]

outputNodeWithBindings :: Node -> TH.StmtQ
outputNodeWithBindings t@(Text _) =
  TH.NoBindS <$> outputWidgetCode t
outputNodeWithBindings s@(SplicedNode _) = do
  varP <- TH.VarP <$> TH.newName "i"
  TH.BindS <$> pure varP <*> outputWidgetCode s
outputNodeWithBindings n@(Node _ _ _) = do
  varPs <- replicateM (numberOfSplices n) (TH.VarP <$> TH.newName "i")
  TH.BindS <$> pure (TH.TupP varPs) <*> outputWidgetCode n

retrieveBindingVars :: [TH.Stmt] -> [TH.Exp]
retrieveBindingVars = concatMap retrieveFrom
  where
    retrieveFrom :: TH.Stmt -> [TH.Exp]
    retrieveFrom (TH.BindS p _) = map TH.VarE (fromPat p)
    retrieveFrom _              = []
    fromPat :: TH.Pat -> [TH.Name]
    fromPat (TH.VarP name) = [name]
    fromPat (TH.TupP ps)   = concatMap fromPat ps
    fromPat _              = []

numberOfSplices :: Node -> Int
numberOfSplices (Text _)        = 0
numberOfSplices (SplicedNode _) = 1
numberOfSplices (Node _ _ ns)   = sum $ map numberOfSplices ns

toStringAttr :: (String, String) -> TH.ExpQ
toStringAttr (key, value) = [| (key, value) |]

outputSplice :: String -> TH.ExpQ
outputSplice function = do
  let Right exp = parseExp function
  return exp

{-


InfixE
  (Just (AppE (AppE (VarE Reflex.Dom.Widget.Basic.elAttr) (ListE [LitE (CharL 'd'),LitE (CharL 'i'),LitE (CharL 'v')]))
              (AppE (VarE Data.Map.Base.fromList) (ListE []))))
  (VarE GHC.Base.$)
  (Just (DoE [NoBindS (AppE (VarE GHC.Base.return) (AppE (ConE Language.Haskell.TH.Syntax.TupP) (ListE [])))]))
-}









{-
InfixE
  (Just (AppE (UnboundVarE el)
              (LitE (StringL "div"))))
  (VarE GHC.Base.$)
  (Just (DoE [BindS (VarP i_2) (UnboundVarE fun)
             ,BindS (VarP j_3) (UnboundVarE fun2)
             ,NoBindS (AppE (VarE GHC.Base.return)
                            (TupE [VarE i_2,VarE j_3]))]))
-}
{-
<div class="row">
  <div class="col-lg-6 col-md-8 col-sm-8 col-xs-12">
    <div class="input-group">
      <div class="input-group-addon"><span>Add another user</span></div>
      <input class="form-control" type="text">
      <div class="input-group-btn">
        <button class="btn btn-default" type="button">Add </button>
      </div>
    </div>
  </div>
</div>
-}

{-
<div class="row">
  <div class="col-lg-6 col-md-8 col-sm-8 col-xs-12">
    <div class="input-group">
      <div class="input-group-addon"><span>Add another user</span></div>
      <input class="form-control" type="text">
      <div class="input-group-btn">
        <button class="btn btn-default" type="button">Add </button>
      </div>
    </div>
  </div>
</div>
-}
