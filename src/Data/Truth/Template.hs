{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Truth.Template where

_tuplePrefix :: Char
_tuplePrefix = 'x'

_varNames :: Char -> [Name]
_varNames c = map (mkName . (c :) . map (chr . (0x2050 +) . ord) . show) [1 :: Int ..]

_tupleVar :: Char -> Int -> Type
_tupleVar c n = foldl AppT (TupleT n) (map VarT (take n (_varNames c)))

-- _simpleInstance ::
-- _simpleInstance tc f tca tcb tcc d = InstanceD Nothing [] (ConT tc `AppT` tca `AppT` tcb `AppT` tcc) [d f]

