-- | Область видимости для @make repl@. Одноимённые определения различайте
-- квалификацией: @Level1.displacement@ и @Lalalang.Programs.displacement@.
module Repl () where

import Level1
import Level2 hiding (lam)
import Level2 qualified
import Level3
import Lalalang.Syntax
import Lalalang.Embedding hiding (c, (.+), (.-), (.*), (./), (.<))
import Lalalang.Embedding qualified
import Lalalang.Semantics
import Lalalang.Programs hiding (displacement)
import Lalalang.Programs qualified
