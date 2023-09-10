# Either를 활용한 에러 처리

하스켈에서 에러를 표현하고 처리하는 방법은 여러가지가 있습니다.
우리는 그 중 하나인 [Either](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html)를 살펴보려고 합니다.
Either는 다음과 같이 정의되어 있습니다:

```haskell
data Either a b
  = Left a
  | Right b
```

간단하게 설명하면, `Either a b` 타입의 값은 `a` 타입의 값이거나 `b` 타입의 값이라고 할 수 있습니다.
다음 생성자를 통해 어떤 타입의 값인지 구분할 수 있습니다:

```haskell
Left True :: Either Bool b
Right 'a' :: Either a Char
```

이러한 타입을 사용하면, `Left` 생성자를 통해 에러 값과 함께 실패를 표현할 수 있고, `Right` 생성자를 통해 예상되는 결과과 함께 성공을 표현할 수 있습니다.

`Either`는 다형적이기 때문에, 실패와 성공을 표현하는 데 두 타입 어느 것을 사용해도 상관없습니다.
때론 실패 모드를 ADT로 표현하는 것이 유용할 때가 있습니다.

예를 들어, `Char`를 숫자로 파싱하여 `Int`로 변환하려고 합시다.
문자가 숫자가 아닐 경우 이 연산은 실패할 수 있습니다.
이러한 실패를 표현하기 위해 다음과 같은 데이터 타입을 정의할 수 있습니다:

```haskell
data ParseDigitError
  = NotADigit Char
  deriving Show
```

그리고 파싱 함수는 다음과 같은 타입을 가질 수 있습니다:

```haskell
parseDigit :: Char -> Either ParseDigitError Int
```

이제 파싱함수를 구현하여, 문제를 설명하는 에러를 `Left`에 담고, 파싱에 성공한 경우에는 `Right`에 담아 반환하면 됩니다:

```haskell
parseDigit :: Char -> Either ParseDigitError Int
parseDigit c =
  case c of
    '0' -> Right 0
    '1' -> Right 1
    '2' -> Right 2
    '3' -> Right 3
    '4' -> Right 4
    '5' -> Right 5
    '6' -> Right 6
    '7' -> Right 7
    '8' -> Right 8
    '9' -> Right 9
    _ -> Left (NotADigit c)
```

`Either`는 또한 `Functor`와 `Applicative` 인스턴스이기 때문에, 이러한 종류의 계산을 결합하려면 몇 가지 조합기를 사용할 수 있습니다.

예를 들어, 세 개의 문자를 파싱하고 그 중 최대 값을 찾으려면 applicative 인터페이스를 사용할 수 있습니다:

```haskell
max3chars :: Char -> Char -> Char -> Either ParseDigitError Int
max3chars x y z =
  (\a b c -> max a (max b c))
    <$> parseDigit x
    <*> parseDigit y
    <*> parseDigit z
```

`Either a`의 `Functor`와 `Applicative` 인터페이스는 함수를 페이로드 값에 적용하고 에러 처리를 **지연**할 수 있도록 해줍니다.
의미적으로, `Left`를 반환하는 첫 번째 Either가 반환 값이 됩니다.
Applicative 인스턴스의 구현에서 이것이 어떻게 작동하는지 살펴보겠습니다:

```haskell
instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r
```

이후에 누군가는 실제로 결과를 **검사**하고 에러(Left 생성자)가 발생했는지, 예상한 값(Right 생성자)이 발생했는지 확인하고 싶을 것입니다.
이를 위해 패턴 매칭을 통해 결과를 검사할 수 있습니다.

## Applicative + Traversable

`Either`의 `Applicative` 인터페이스는 매우 강력하며, 다른 추상화인 [`Traversable`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Traversable.html#g:1)와 결합할 수 있습니다. - 연결 리스트나 이진 트리와 같이 왼쪽에서 오른쪽으로 순회할 수 있는 데이터 구조를 말합니다.
이를 통해 `Traversable`를 구현하는 데이터 구조이기만 하면 임의의 개수의 `Either ParseDigitError Int`와 같은 값들을 결합할 수 있습니다.

예제를 살펴보겠습니다:

```haskell
ghci> :t "1234567"
"1234567" :: String
-- String은 Char의 리스트에 대한 별칭인 것을 기억하세요.
ghci> :info String
type String :: *
type String = [Char]
      -- Defined in ‘GHC.Base’

ghci> :t map parseDigit "1234567"
map parseDigit "1234567" :: [Either ParseDigitError Int]
ghci> map parseDigit "1234567"
[Right 1,Right 2,Right 3,Right 4,Right 5,Right 6,Right 7]

ghci> :t sequenceA
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- `t`를 `[]`로, `f`를 `Either Error`로 대체해서 생각해볼 수 있습니다.

ghci> sequenceA (map parseDigit "1234567")
Right [1,2,3,4,5,6,7]

ghci> map parseDigit "1a2"
[Right 1,Left (NotADigit 'a'),Right 2]
ghci> sequenceA (map parseDigit "1a2")
Left (NotADigit 'a')
```

`map`과 `sequenceA`를 결합하는 대신 `traverse`를 사용할 수도 있습니다.

```haskell
ghci> :t traverse
traverse
  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
ghci> traverse parseDigit "1234567"
Right [1,2,3,4,5,6,7]
ghci> traverse parseDigit "1a2"
Left (NotADigit 'a')
```

`Either a` 또는 `IO`처럼 `Applicative` 인터페이스를 구현한 타입과 `[]` 또는
[`Map k`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#t:Map)
(다른 언어에서는 딕셔너리라고도 함 - 키와 값의 매핑) 와 같이 `Traversable` 인터페이스를 구현한 어떠한 두 타입에 대해서도 `traverse`를 사용할 수 있습니다.

예를 들어 `IO`와 `[]`를 결합할 수 있습니다.
`Map` 데이터 구조는 [`fromList`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#v:fromList) 함수를 사용하여 튜플의 리스트에서 생성할 수 있습니다. - 튜플의 첫 번째 값은 키이고 두 번째 값은 값입니다.

```haskell
ghci> import qualified Data.Map as M -- 컨테이너 패키지에서 가져옵니다.

ghci> file1 = ("output/file1.html", "input/file1.txt")
ghci> file2 = ("output/file2.html", "input/file2.txt")
ghci> file3 = ("output/file3.html", "input/file3.txt")
ghci> files = M.fromList [file1, file2, file3]
ghci> :t files :: M.Map FilePath FilePath -- FilePath는 String의 별칭입니다.
files :: M.Map FilePath FilePath :: M.Map FilePath FilePath

ghci> readFiles = traverse readFile
ghci> :t readFiles
readFiles :: Traversable t => t FilePath -> IO (t String)

ghci> readFiles files
fromList [("output/file1.html","I'm the content of file1.txt\n"),("output/file2.html","I'm the content of file2.txt\n"),("output/file3.html","I'm the content of file3.txt\n")]
ghci> :t readFiles files
readFiles files :: IO (Map String String)
```

위 코드에서 `readFiles`라는 함수를 만들었습니다.
이 함수는 *출력 파일 경로*를 *입력 파일 경로*로 매핑을 수행합니다.
그리고 입력 파일을 읽어서 그 내용을 맵에 바로 쓰는 IO 연산을 반환합니다!
나중에 유용하게 사용할 수 있을 것입니다.

## 에러가 여러 개인 경우

`Either`의 kind는 `* -> * -> *`(두 개의 타입 파라미터를 받습니다)이기 때문에 `Either`는 `Functor`나 `Applicative`의 인스턴스가 될 수 없습니다.
이러한 타입 클래스의 인스턴스는 kind가 `* -> *`이어야 합니다.
다음 타입 클래스 함수 시그니처를 살펴보면:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

그리고 특정한 타입에 대해 이를 구현하고 싶다면(`f`의 자리에), `f`를 대상 타입으로 *치환*할 수 있어야 합니다.
`Either`를 사용하려고 하면 다음과 같은 시그니처를 얻을 수 있습니다:

```haskell
fmap :: (a -> b) -> Either a -> Either b
```

`Either a`와 `Either b`는 둘 다 *구체화된 타입*이 아니기 때문에 이는 타입 오류가 발생합니다.
같은 이유로 `f`를 `Int`로 치환하려고 하면 다음과 같은 시그니처를 얻을 수 있습니다:

```haskell
fmap :: (a -> b) -> Int a -> Int b
```

이 또한 타입 오류가 발생합니다.

`Either`를 사용할 수 없지만, `Either e`의 kind는 `* -> *`이기 때문에 사용할 수 있습니다.
다음 시그니처에서 `f`를 `Either e`로 치환해봅시다:

```haskell
liftA2 :: Applicative => (a -> b -> c) -> f a -> f b -> f c
```

다음과 같은 결과를 얻을 수 있습니다:

```haskell
liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
```

이를 통해 알 수 있는 것은 *`Left` 생성자의 타입이 같은 두 개의 `Either`를 결합할 때만 applicative 인터페이스를 사용할 수 있다는 것*입니다.

그렇다면 두 개의 `Either`를 결합할 때 `Left` 생성자의 타입이 다르다면 어떻게 해야 할까요?
몇 가지 방법이 있지만 가장 적법한 방법은 다음과 같습니다:

1. 같은 에러 타입을 반환하도록 만듭니다. 모든 에러를 하나의 타입으로 통합하는 ADT를 작성합니다.
   이는 일부 경우에는 작동하지만 항상 이상적인 것은 아닙니다.
   예를 들어 `parseDigit`의 입력이 빈 문자열일 수 있는 경우를 사용자가 직접 처리하게 만들어서는 안 됩니다.
2. 각 타입에 대해 특수한 에러 타입을 사용합니다. 그리고 이들을 결합할 때는 일반적인 에러 타입으로 매핑합니다.
   이는 [`first`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Bifunctor.html#v:first) 함수를 사용하여 수행할 수 있습니다.
   `first` 함수는 `Bifunctor` 타입 클래스에 정의되어 있습니다.

## 모나딕 인터페이스

Applicative 인터페이스를 사용하면 여러 개의 `Either` 값(또는 `IO`나 `Parser`와 같은 다른 applicative functor 인스턴스)을 처리할 수 있게 함수를 끌어올릴 수 있습니다.
하지만 더 자주 사용하는 방법은 에러가 발생할 수 있는 한 계산결과를 에러가 발생할 수 있는 다른 계산에 사용하는 것입니다.

예를 들어, 컴파일러는 어휘 분석, 파싱, 타입 체크, 코드 생성 등의 단계로 구성됩니다.
각 단계는 이전 단계의 출력에 의존하며, 각 단계는 실패할 수 있습니다.
각 단계에 대한 함수의 타입은 다음과 같습니다:

```haskell
tokenize :: String -> Either Error [Token]

parse :: [Token] -> Either Error AST

typecheck :: AST -> Either Error TypedAST
```

이러한 함수를 합성해 체인으로 작동하도록 만들려고 합니다.
즉 `tokenize`의 출력은 `parse`로, `parse`의 출력은 `typecheck`로 이동합니다.

우리는 특정 함수를 `Either`에 대해 동작하게 끌어올리는 방법을 이미 알고 있습니다.
`Either`를 반환하는 함수 또한 끌어올릴 수 있습니다:

```haskell
-- fmap 타입은 다음과 같습니다
fmap :: Functor f => (a -> b) -> f a -> f b
-- `Either Error`로 치환하면
fmap :: (a -> b) -> Either Error a -> Either Error b

-- 여기서 `a`는 [Token]이고 `b`는 `Either Error AST`입니다:

> fmap parse (tokenize string) :: Either Error (Either Error AST)
```

위 코드는 컴파일에 성공하지만, 훌륭하지는 않습니다.
왜냐하면 우리는 `Either Error`의 계층을 만들고 있고 `typecheck`에서 이 트릭을 다시 사용할 수 없기 때문입니다!
`typecheck`는 `AST`를 기대하지만 `fmap parse (tokenize string)`에 대해 fmap을 시도하면 `a`는 `Either Error AST`가 됩니다.

우리가 원하는 것은 이러한 계층을 중첩하는 것이 아니라 펼치는 것입니다.
`Either Error (Either Error AST)`의 값이 가질 수 있는 종류를 살펴보면 다음과 같습니다:

- `Left <error>`
- `Right (Left error)`
- `Right (Right <ast>)`

---

**연습문제**: 위 타입에 대해 패턴매칭을 수행하면 어떠한 코드가 나올까요?

<details><summary>정답</summary>

```haskell
case tokenize string of
  Left err ->
    Left err
  Right tokens ->
    case parse tokens of
      Left err ->
        Left err
      Right ast ->
        typecheck ast
```

각 단계에서 에러가 발생하면 에러를 반환하고 중단합니다.
성공하면 다음 단계에 대한 입력으로 사용합니다.

</details>

---

이러한 `Either`의 중첩을 펼치는 과정은 마지막 단계인 `Right tokens`일 때도 동일하게 수행됩니다.

```haskell
flatten :: Either e (Either e a) -> Either e a
flatten e =
  case e of
    Left l -> Left l
    Right x -> x
```

위와 같은 함수를 만들었다면, `fmap parse (tokenize string) :: Either Error (Either Error AST)`
의 결과에 적용할 수 있습니다:

```
> flatten (fmap parse (tokenize string)) :: Either Error AST
```

이제 `typecheck`와 합성하기 위해 다시 사용할 수 있습니다:

```haskell
> flatten (fmap typecheck (flatten (fmap parse (tokenize string)))) :: Either Error TypedAST
```

이러한 `flatten` + `fmap' 조합은 반복되는 패턴이기에, 이를 함수로 결합할 수 있습니다:

```haskell
flatMap :: (a -> Either e b) -> Either a -> Either b
flatMap func val = flatten (fmap func val)
```

이제 코드를 다음과 같이 작성할 수 있습니다:

```haskell
> flatMap typecheck (flatMap parse (tokenize string)) :: Either Error TypedAST

-- 또는 함수를 중위 표기법으로 변환하기 위해 backtick을 사용합니다:
> typecheck `flatMap` parse `flatMap` tokenize string

-- 또는 custom infix operator를 만듭니다: (=<<) = flatMap
> typeCheck =<< parse =<< tokenize string
```

`flatten` (그리고 `flatMap`) 함수는 하스켈에서는 다른 이름으로 사용되며,
[`join`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:join)
과 [`=<<`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-61--60--60-)("reverse bind"로 발음)
로 불립니다.
이들은 하스켈에서 또 다른 매우 유용한 추상화의 핵심입니다.

다음과 같은 항목을 구현한 타입이 있다면:

1. `Functor` 인터페이스, 특히 `fmap` 함수
2. `Applicative` 인터페이스, 특히 `pure` 함수
3. `join` 함수

`Monad` 타입 클래스의 인스턴스를 구현할 수 있습니다.

Functor를 통해 우리는 함수를 "끌어올려" functor 타입 클래스를 구현하는 타입 위에서 작동하도록 할 수 있었습니다:

```haskell
fmap :: (a -> b) -> f a -> f b
```

Applicative functors를 통해 우리는 applicative functor 타입 클래스를 구현한 타입을 가진 여러 인자들을 가진 함수를 끌어올릴 수 있었습니다.
또한 해당 타입으로 어떠한 값을 끌어올릴 수도 있었습니다:

```haskell
pure :: a -> f a

liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

이제 Monad를 통해 우리는 `Monad` 인터페이스를 구현한 타입들을 펼칠 수 (또는 하스켈 용어로 "join"할 수) 있습니다.

```haskell
join :: m (m a) -> m a

-- =<< 의 인자를 반대로 뒤집은 것입니다. "bind"로 발음합니다.
(>>=) :: m a -> (a -> m b) -> m b
```

`>>=`를 사용하면 예제로 소개한 컴파일 파이프라인을 왼쪽에서 오른쪽으로 작성할 수 있습니다.
monad에 대해서는 이 방식을 더 자주 사용합니다:

```haskell
> tokenize string >>= parse >>= typecheck
```

사실 이 함수는 예전에 `IO`에 대해 소개할 때 이미 사용해 보았습니다.
맞습니다. `IO` 또한 `Monad` 인터페이스를 구현합니다.
`IO`의 모나드 인터페이스는 효과의 순서를 구성하는 데 도움이 됩니다.

`Monad` 인터페이스의 핵심은 `join`/`>>=` 함수이며, 우리가 `>>=`를 `join`으로 구현할 수 있었듯이,
`join`을 `>>=`로 구현할 수도 있습니다 (한 번 시도해 보세요!).

`Monad` 인터페이스는 타입에 따라 각각 다른 의미를 가질 수 있습니다.
`IO`의 경우 효과의 순서를 의미하고, `Either`의 경우 조기 종료를 의미하며,
[`Logic`](https://hackage.haskell.org/package/logict-0.7.1.0)에 대해서는 backtracking 계산을 의미합니다.

다시 말하지만, 이론과 비유에 신경쓰지 말고, API와 [법칙](https://wiki.haskell.org/Monad_laws)에 집중하세요.

> 혹시 Monad 법칙을 확인해 보셨나요? 왼쪽 항등, 오른쪽 항등, 결합성에 대한 내용입니다.
> 우리는 이미 이러한 법칙을 가진 타입 클래스에 대해 논의했습니다.
> 바로 `Monoid` 타입 클래스입니다.
> 아마도 이것이 유명한 명언과 관련이 있을지도 모릅니다. - monad is just a monoid in the category of endofunctors.

### Do 표기법

[do 표기법](../05-glue/02-io.md#do-표기법)을 기억하시나요?
이는 `Monad`의 인스턴스인 모든 타입에 대해 동작합니다.
다음과 같은 코드를

```haskell
pipeline :: String -> Either Error TypedAST
pipeline string =
  tokenize string >>= \tokens ->
    parse tokens >>= \ast ->
      typecheck ast
```

아래와 같이 작성할 수 있습니다:

```haskell
pipeline :: String -> Either Error TypedAST
pipeline string = do
  tokens <- tokenize string
  ast <- parse tokens
  typecheck ast
```

또한 `tokenize string >>= parse >>= typecheck`와 같이 특별한 경우에는
[>=>](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-62--61--62-)
또는
[<=<](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-60--61--60-)
를 사용하여 더 간결하게 작성할 수 있습니다:

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- 함수 합성과 비교해 보세요:
(.) ::              (b ->   c) -> (a ->   b) -> a ->   c
```

```haskell
pipeline  = tokenize >=> parse >=> typecheck
```

또는

```haskell
pipeline = typecheck <=< parse <=< tokenize
```

추상화를 통해 간결한 코드를 작성할 수 있는 하스켈의 능력은
추상화에 익숙해지면 더욱 더 좋아집니다.
Monad 추상화에 대해 알게 되면, 이미 많은 라이브러리들이 사용하는 핵심 조합 API를 빠르게 익힐 수 있습니다.
예를 들면:

- [동시성](https://hackage.haskell.org/package/stm)과 [비동기 프로그래밍](https://hackage.haskell.org/package/async)
- [웹 프로그래밍](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board)
- [테스팅](http://hspec.github.io/)
- [상태 계산 모형](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#g:2)
- [계산간의 환경을 공유](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:2)
- 그 외

## 요약

에러 처리를 위해 `Either`를 사용하면 다음과 같은 이점이 있습니다:

1. 타입을 통해 에러를 표현할 수 있습니다. 그리고 사용자가 이러한 **에러를 처리하도록 강제**할 수 있습니다.
   이를 통해 코드는 더욱 견고해지고, 잘못된 동작을 방지할 수 있습니다.
2. `Functor`, `Applicative`, `Monad` 인터페이스를 통해 실패할 수 있는 함수를 **조합**할 수 있습니다.
   이를 통해 보일러 플레이트를 줄이고, 코드에 대한 강력한 보장을 유지하며, 에러를 처리하는 시점을 미룰 수 있습니다.
