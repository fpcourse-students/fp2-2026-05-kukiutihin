-- | Примеры программ на языке Lalalang.
module Lalalang.Programs where

import Lalalang.Syntax
import Lalalang.Embedding
import Lalalang.Semantics

-- | Вычисляет пройденный путь с равномерным ускорением, если
-- v - конечная скорость, v0 - начальная скорость, a - ускорение.
displacement :: LaExpr
displacement = lam "v" $ lam "v0" $ lam "a" $
  (v "v" .* v "v" .- v "v0" .* v "v0") ./ (c 2 .* v "a")

-- | Или в эмбеддинге получше.
displacement' :: LaExpr
displacement' = lam #v $ lam #v0 $ lam #a $
  (#v * #v - #v0 * #v0) ./ (2 .* #a)

{- HLINT ignore fix "Move brackets to avoid $" -}
-- | Комбинатор рекурсии Z.
fix :: LaExpr
fix = lam #f (
  (lam #x $ #f @ (lam #y $ #x @ #x @ #y)) @
  (lam #x $ #f @ (lam #y $ #x @ #x @ #y)))

fac :: LaExpr
fac = (fix @) $ lam #rec $ lam #n $
  if_ (#n .<= 1) 1 (#n * (#rec @ (#n - 1)))

-- | Изменяемое состояние в Haskell!
fac' :: Int -> Int
fac' = evalWithInput $
  #res .:= 1 .:
  #n .:= 1 .:
  while (#n .<= #input) (
    #res .:= #res .* #n .:
    #n .:= #n .+ 1
  ) .:
  #res
