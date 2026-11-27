module Level2 where

import Data.Coerce
import Data.Functor.Identity
import Data.Kind
import Level1
import MetaUtils

data TypedExpr a where
  Number :: Int -> TypedExpr Int
  Boolean :: Bool -> TypedExpr Bool
  Add :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
  Less :: TypedExpr Int -> TypedExpr Int -> TypedExpr Bool
  If :: TypedExpr Bool -> TypedExpr a -> TypedExpr a -> TypedExpr a


-- 2.1. Типизированные выражения
-- Реализуйте вычислитель. Тип результата задаёт само дерево.
-- Например, evalTyped (Less (Number 1) (Number 2)) == True.
-- Условие вычисляет только выбранную ветку.

evalTyped :: TypedExpr a -> a
evalTyped = todo "evalTyped"


-- 2.2. Независимое расширение языка
-- Класс Control добавляет в язык условные выражения. Напишите два инстанса.
-- Для Int ноль выбирает ветку else, любое другое число — ветку then.
-- Для String напечатайте условие со скобками, например "(if (1 < 2) then 3 else 4)".
--
-- Напишите maxDisplacement, которая выбирает большее из перемещений двух тел.
-- Тело задано тройкой (v, v0, a). Для каждой тройки используйте displacement v v0 a
-- из 1.2: она уже строит выражение (v² - v0²) / (2 * a). Сравните перемещения
-- операцией (.<) и выберите одно из них с помощью if'. Укажите нужные ограничения
-- в сигнатуре maxDisplacement.
-- GHCi> maxDisplacement (50, 30, 2) (30, 10, 2) :: Int
-- 400

class Control target where
  if' :: target -> target -> target -> target

todoImpl ''Control ''Int
todoImpl ''Control ''String

type Object = (Int, Int, Int)

maxDisplacement :: Object -> Object -> target
maxDisplacement = todo "maxDisplacement"


-- 2.3. Из final в initial и обратно
-- Напишите инстанс Expr для дерева и foldTree для интерпретации дерева
-- в любом домене Expr. Каждый метод инстанса строит один узел дерева.
-- Пример: (c 3 .+ c 4 :: Tree) == Sum (Lit 3) (Lit 4).

data Tree
  = Lit Int
  | Sum Tree Tree
  | Difference Tree Tree
  | Product Tree Tree
  | Quotient Tree Tree
  | LessThan Tree Tree
  deriving stock (Eq, Show)

todoImpl ''Expr ''Tree

foldTree :: Expr target => Tree -> target
foldTree = todo "foldTree"


-- 2.4. Типизированные функции
-- Напишите полный инстанс Term Identity и используйте его в compute.
-- Ограничение C выбирает допустимые литералы для каждого домена.
-- compute принимает произвольную программу с целыми литералами.

class Term d where
  -- Дадим возможность инстансам управлять ограничениями.
  type C d ty :: Constraint
  val :: C d ty => ty -> d ty
  plus :: d Int -> d Int -> d Int
  app :: d (arg -> res) -> d arg -> d res
  lam :: (d arg -> d res) -> d (arg -> res)

example :: (Term d, C d Int) => d Int
example = (lam \x -> x `plus` val 41) `app` val 1

compute :: (forall d. (Term d, C d Int) => d a) -> a
compute _ = todo "compute"

exampleCompute :: Int
exampleCompute = compute example


-- 2.5. Программы с изменяемым состоянием
--
-- Продолжите работу в модулях Lalalang.Syntax, Lalalang.Embedding и Lalalang.Semantics
-- из 1.1. Расширьте дерево и интерпретатор последовательностью `p .: q`, присваиванием
-- `#x .:= e` и циклом `while cond body`. Запись программы строит дерево. Эффекты
-- возникают при его вычислении.
--
-- * Последовательность вычисляет выражения слева направо и возвращает значение второго.
-- Каждое выражение получает окружение, оставленное предыдущим.
-- * Присваивание вычисляет правую часть, записывает результат по имени и возвращает
-- `unit`. Первое присваивание создаёт переменную, следующее меняет её значение.
-- * Цикл проверяет условие перед каждой итерацией. Ноль завершает цикл, любое другое
-- число запускает тело. Изменения, сделанные последней проверкой условия, сохраняются.
-- Значение цикла — `unit`. В `if_` эффекты выполняет только выбранная ветка.
-- * Замыкание сохраняет снимок окружения. Присваивания при вычислении аргумента видны
-- вызывающему коду. Присваивания при вычислении тела функции локальны для вызова.
--
-- В `Lalalang.Programs.fac'` уже записана программа, которая в цикле меняет аккумулятор
-- и счётчик. После реализации `fac' 0` даёт 1, а `fac' 5` даёт 120.
