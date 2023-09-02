module Data.Truth.Template where

_simpleInstance tc f tca tcb tcc d = InstanceD Nothing [] (ConT tc `AppT` tca `AppT` tcb `AppT` tcc) [d f]
