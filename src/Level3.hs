module Level3 where

import Level2 (Term (..), example)
import MetaUtils


-- 3.1. Печать функций
-- Выберите представление для печати и напишите инстанс Term.
-- render печатает целые литералы, имена, (l + r), (f x) и (\x -> body).
-- Имя начинается с латинской буквы, далее допустимы буквы и цифры.
-- При вложении функций сохраняйте принадлежность вхождений их лямбдам.
-- Например, внешнюю переменную в \x -> \y -> x нельзя захватить внутренней.

render :: (forall d. (Term d, C d Int) => d a) -> String
render _ = todo "render"

exampleShow :: String
exampleShow = render example
