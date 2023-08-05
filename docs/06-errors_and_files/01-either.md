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

`Either`의 `Applicative` 인터페이스는 매우 강력하며, 다른 추상화인 [`Traversable`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Traversable.html#g:1)와 결합할 수 있습니다.
- 연결 리스트나 이진 트리와 같이 왼쪽에서 오른쪽으로 순회할 수 있는 데이터 구조를 말합니다.
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
map parseDigit mystring :: [Either ParseDigitError Int]
ghci> map parseDigit "1234567"
[Right 1,Right 2,Right 3,Right 4,Right 5,Right 6,Right 7]

ghci> :t sequenceA
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- `t`를 `[]`로, `f`를 `Either Error`로 대체해서 생각해볼 수 있습니다.

ghci> sequenceA (map parseDigit mystring)
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
`Map` 데이터 구조는 [`fromList`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#v:fromList) 함수를 사용하여 튜플의 리스트에서 생성할 수 있습니다.
- 튜플의 첫 번째 값은 키이고 두 번째 값은 값입니다.

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

이를 통해 알 수 있는 것은 _`Left` 생성자의 타입이 같은 두 개의 `Either`를 결합할 때만 applicative 인터페이스를 사용할 수 있다는 것_입니다.

그렇다면 두 개의 `Either`를 결합할 때 `Left` 생성자의 타입이 다르다면 어떻게 해야 할까요?
몇 가지 방법이 있지만 가장 적법한 방법은 다음과 같습니다:

1. 같은 에러 타입을 반환하도록 만듭니다. 모든 에러를 하나의 타입으로 통합하는 ADT를 작성합니다.
   이는 일부 경우에는 작동하지만 항상 이상적인 것은 아닙니다.
   예를 들어 `parseDigit`의 입력이 빈 문자열일 수 있는 경우를 사용자가 직접 처리하게 만들어서는 안 됩니다.
2. 각 타입에 대해 특수한 에러 타입을 사용합니다. 그리고 이들을 결합할 때는 일반적인 에러 타입으로 매핑합니다.
   이는 [`first`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Bifunctor.html#v:first) 함수를 사용하여 수행할 수 있습니다.
   `first` 함수는 `Bifunctor` 타입 클래스에 정의되어 있습니다.

## 모나딕 인터페이스

The applicative interface allows us to lift a function to work on multiple
`Either` values (or other applicative functor instances such as `IO` and `Parser`).
But more often than not, we'd like to use a value from one computation
that might return an error in another computation that might return an error.

For example, a compiler such has GHC operates in stages, such as lexical analysis,
parsing, type-checking, and so on. Each stage depends on the output of the stage
before it, and each stage might fail. We can write the types for these functions:

```haskell
tokenize :: String -> Either Error [Token]

parse :: [Token] -> Either Error AST

typecheck :: AST -> Either Error TypedAST
```

We want to compose these functions so that they work in a chain. The output of `tokenize`
goes to `parse`, and the output of `parse` goes to `typecheck`.

We know that we can lift a function over an `Either` (and other functors),
we can also lift a function that returns an `Either`:

```haskell
-- reminder the type of fmap
fmap :: Functor f => (a -> b) -> f a -> f b
-- specialized for `Either Error`
fmap :: (a -> b) -> Either Error a -> Either Error b

-- here, `a` is [Token] and `b` is `Either Error AST`:

> fmap parse (tokenize string) :: Either Error (Either Error AST)
```

While this code compiles, it isn't great, because we are building
layers of `Either Error` and we can't use this trick again with
`typecheck`! `typecheck` expects an `AST`, but if we try to fmap it
on `fmap parse (tokenize string)`, the `a` will be `Either Error AST`
instead.

What we would really like is to flatten this structure instead of nesting it.
If we look at the kind of values `Either Error (Either Error AST)` could have,
it looks something like this:

- `Left <error>`
- `Right (Left error)`
- `Right (Right <ast>)`

---

**Exercise**: What if we just used pattern matching for this instead? How would this look like?

<details><summary>Solution</summary>

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

If we run into an error in a stage, we return that error and stop. If we succeed, we
use the value on the next stage.

</details>

---

Flattening this structure for `Either` is very similar to that last part - the body
of the `Right tokens` case:

```haskell
flatten :: Either e (Either e a) -> Either e a
flatten e =
  case e of
    Left l -> Left l
    Right x -> x
```

Because we have this function, we can now use it on the output of
`fmap parse (tokenize string) :: Either Error (Either Error AST)`
from before:

```
> flatten (fmap parse (tokenize string)) :: Either Error AST
```

And now we can use this function again to compose with `typecheck`:

```haskell
> flatten (fmap typecheck (flatten (fmap parse (tokenize string)))) :: Either Error TypedAST
```

This `flatten` + `fmap` combination looks like a recurring pattern which
we can combine into a function:

```haskell
flatMap :: (a -> Either e b) -> Either a -> Either b
flatMap func val = flatten (fmap func val)
```

And now we can write the code this way:

```haskell
> flatMap typecheck (flatMap parse (tokenize string)) :: Either Error TypedAST

-- Or using backticks syntax to convert the function to infix form:
> typecheck `flatMap` parse `flatMap` tokenize string

-- Or create a custom infix operator: (=<<) = flatMap
> typeCheck =<< parse =<< tokenize string
```

This function, `flatten` (and `flatMap` as well), have different names in Haskell.
They are called
[`join`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:join)
and [`=<<`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-61--60--60-)
(pronounced "reverse bind"),
and they are the essence of another incredibly useful abstraction in Haskell.

If we have a type that can implement:

1. The `Functor` interface, specifically the `fmap` function
2. The `Applicative` interface, most importantly the `pure` function
3. This `join` function

They can implement an instance of the `Monad` type class.

With functors, we were able to "lift" a function to work over the type implementing the functor type class:

```haskell
fmap :: (a -> b) -> f a -> f b
```

With applicative functors we were able to "lift" a function of multiple arguments
over multiple values of a type implementing the applicative functor type class,
and also lift a value into that type:

```haskell
pure :: a -> f a

liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

With monads we can now flatten (or, "join" in Haskell terminology) types that implement
the `Monad` interface:

```haskell
join :: m (m a) -> m a

-- this is =<< with the arguments reversed, pronounced "bind"
(>>=) :: m a -> (a -> m b) -> m b
```

With `>>=` we can write our compilation pipeline from before in a left-to-right
manner, which seems to be more popular for monads:

```haskell
> tokenize string >>= parse >>= typecheck
```

We have already met this function before when we talked about `IO`. Yes,
`IO` also implements the `Monad` interface. The monadic interface for `IO`
helped us with creating a proper ordering of effects.

The essence of the `Monad` interface is the `join`/`>>=` functions, and as we've seen
we can implement `>>=` in terms of `join`, we can also implement `join` in terms
of `>>=` (try it!).

The monadic interface can mean very different things for different types. For `IO` this
is ordering of effects, for `Either` it is early cutoff,
for [`Logic`](https://hackage.haskell.org/package/logict-0.7.1.0) this means backtracking computation, etc.

Again, don't worry about analogies and metaphors, focus on the API and the
[laws](https://wiki.haskell.org/Monad_laws).

> Hey, did you check the monad laws? left identity, right identity and associativity? We've already
> discussed a type class with exactly these laws - the `Monoid` type class. Maybe this is related
> to the famous quote about monads being just monoids in something something...

### Do notation?

Remember the [do notation](../05-glue/02-io.md#do-notation)? Turns out it works for any type that is
an instance of `Monad`. How cool is that? Instead of writing:

```haskell
pipeline :: String -> Either Error TypedAST
pipeline string =
  tokenize string >>= \tokens ->
    parse tokens >>= \ast ->
      typecheck ast
```

We can write:

```haskell
pipeline :: String -> Either Error TypedAST
pipeline string = do
  tokens <- tokenize string
  ast <- parse tokens
  typecheck ast
```

And it will work! Still, in this particular case `tokenize string >>= parse >>= typecheck`
is so concise it can only be beaten by using
[>=>](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-62--61--62-)
or
[<=<](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-60--61--60-):

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- compare with function composition:
(.) ::              (b ->   c) -> (a ->   b) -> a ->   c
```

```haskell
pipeline  = tokenize >=> parse >=> typecheck
```

or

```haskell
pipeline = typecheck <=< parse <=< tokenize
```

Haskell's ability to create very concise code using abstractions is
great once one is familiar with the abstractions. Knowing the monad abstraction,
we are now already familiar with the core composition API of many libraries - for example:

- [Concurrent](https://hackage.haskell.org/package/stm)
  and [asynchronous programming](https://hackage.haskell.org/package/async)
- [Web programming](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board)
- [Testing](http://hspec.github.io/)
- [Emulating stateful computation](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#g:2)
- [sharing environment between computations](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:2)
- and many more.

## Summary

Using `Either` for error handling is useful for two reasons:

1. We encode possible errors using types, and we **force users to acknowledge and handle** them, thus
   making our code more resilient to crashes and bad behaviours
2. The `Functor`, `Applicative`, and `Monad` interfaces provide us with mechanisms for
   **composing** functions that might fail (almost) effortlessly - reducing boilerplate while
   maintaining strong guarantees about our code, and delaying the need to handle errors until
   it is appropriate
