# 멋진 옵션 파싱

이번에는 프로그램을 위한 더 멋진 인터페이스를 정의해보려고 합니다.
`getArgs`와 패턴 매칭으로도 무언가를 만들 수 있지만, 라이브러리를 사용하면 더 좋은 결과를 얻을 수 있습니다.
이번 장에서는 [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)라는 패키지를 사용할 것입니다.

`optparse-applicative`는 명령행 인자 파서를 만들기 위한 EDSL을 제공합니다.
명령, 스위치, 플래그 등을 만들고 조합하여 실제로 문자열에 대한 연산을 작성하지 않고도
명령행 인자를 파싱하는 파서를 만들 수 있습니다.
또한 사용법, 도움말, 에러 메시지 등을 자동으로 생성해주는 등의 기능도 제공합니다.

`optparse-applicative`의 의존성 풋프린트는 크지 않지만, 우리 라이브러리를 사용하는 사용자가
명령행 파싱이 필요하지 않을 가능성이 높기 때문에 `.cabal` 파일의 (`libray` 섹션이 아닌) `executable` 섹션에
의존성을 추가하는게 바람직합니다:

```diff
 executable hs-blog-gen
   import: common-settings
   hs-source-dirs: app
   main-is: Main.hs
   build-depends:
       base
+    , optparse-applicative
     , hs-blog
   ghc-options:
     -O
```

## 명령줄 파서 만들기

optparse-applicative 패키지는 꽤 괜찮은
[문서](https://hackage.haskell.org/package/optparse-applicative-0.16.1.0#optparse-applicative),
를 가지고 있습니다. 하지만 이 장에서 중요한 몇 가지 사항을 다룰 것입니다.

일반적으로, 우리가 해야 할 일은 네 가지가 있습니다:

1. 모델을 정의합니다 - 프로그램의 다양한 옵션과 명령을 설명하는 ADT를 정의합니다.
2. 실행할 때 모델 타입의 값을 생성하는 파서를 정의합니다.
3. 파서를 프로그램 인자 입력에 실행합니다.
4. 모델을 패턴 매칭하고 옵션에 따라 올바른 작업을 호출합니다.

### 모델 정의하기

먼저 우리의 명령줄 인터페이스를 상상해봅시다. 어떤 기능이 있을까요?

단일 파일이나 입력 스트림을 파일이나 출력 스트림으로 변환해거나,
디렉토리 전체를 처리하고 새 디렉토리를 생성할 수 있어야 합니다.
우리는 이를 ADT로 모델링 할 수 있습니다:

```haskell
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show
```

> `SingleInput`과 `SingleOutput`을 `Maybe FilePath`로 표현하는 것도 하나의 방법입니다.
> 하지만 이 경우 각 문맥에서 `Nothing`이 무엇을 의미하는지 기억해야 합니다.
> 각 옵션에 대한 적절한 이름을 가진 새로운 타입을 만드는 것이 코드의 의미를 이해하는데 도움이 됩니다.

인터페이스 관점에서, 사용자가 단일 입력 소스를 변환하려면 `convert` 명령어를 사용하고
옵션으로 `--input FILEPATH`와 `--output FILEPATH`를 제공하여 파일에서 읽거나 쓰게할 수 있습니다.
만약 사용자가 하나 이상의 옵션을 제공하지 않으면 표준 입력/출력에서 읽거나 쓸 것입니다.

만약 사용자가 디렉토리를 변환하려면 `convert-dir` 명령어를 사용하고
`--input FILEPATH`와 `--output FILEPATH`를 필수로 입력해야 합니다.

### 파서 만들기

이 과정에서 가장 흥미로운 부분입니다. 어떻게 모델에 맞는 파서를 만들 수 있을까요?

`optparse-applicative` 라이브러리는 `Parser`라는 새로운 타입을 제공합니다.
`Parser`는 `Maybe`나 `IO`와 같이 kind가 `* -> *`인 타입입니다.
`Int`, `Bool` 또는 `Options`와 같은 구체적인 타입을 제공해야 `Parser`는 (값을 가지는) 구체적인 타입이 될 수 있습니다.

`Parser a`는 명령줄 인자가 성공적으로 파싱된 경우 타입 `a`의 값을 생성하는 명령줄 파서의 명세를 의미합니다.
이는 `IO a`가 `a`의 값을 생성하는 프로그램의 명세를 의미하는 것과 유사합니다.
이 두 타입의 주요 차이점은 `IO a`를 `a`로 변환할 수 없지만
(우리는 단지 IO 연산을 연결하고 Haskell 런타임이 그것들을 실행하게 합니다),
`Parser a`를 프로그램 인자를 의미하는 문자열 목록을 받고 인자를 파싱할 수 있으면 `a`를 생성하는 함수로 변환할 수 있다는 것입니다.

이전 EDSL과 마찬가지로, 이 라이브러리는 *조합자 패턴*을 사용합니다.
우리는 파서를 만들기 위한 원시값들과 작은 파서를 큰 파서로 조합하는 방법을 익혀야 합니다.

작은 파서의 예제를 살펴봅시다:

```haskell
inp :: Parser FilePath
inp =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input file"
    )

out :: Parser FilePath
out =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
    )
```

`strOption`는 파서를 만드는 함수입니다. 이 함수는 인자로 조합된 *옵션 수정자*를 받아 문자열을 파싱하는 파서를 반환합니다.
타입을 `FilePath`로 지정할 수도 있는데, `FilePath`가 `String`의 별칭이기 때문입니다.
파서 빌더는 값을 파싱하는 방법을 설명하고, 수정자는 플래그 이름, 플래그 이름의 약어, 사용법 및 도움말 메시지에 대한 내용과 같은 속성을 설명합니다.

> 사실 `strOption`는 `IsString` 인터페이스를 구현하는 어떤 문자열 타입이라도 반환할 수 있습니다.
> 그 예로 `text` 패키지에서 가져온 훨씬 효율적인 유니코드 텍스트 타입인 `Text`가 있습니다.
> `String`은 `Char`의 링크드 리스트로 구현되지만, `Text`는 바이트 배열로 구현되기 때문에 `Text`가 더 효율적입니다.
> 텍스트 값을 위해 `String` 대신 `Text`를 사용하는게 좋습니다.
> 지금까지는 사용하지 않았는데 이는 `String`보다 약간 사용하기 불편하기 때문입니다.
> 하지만 텍스트를 표현할때 많이 사용하는 타입입니다!

보시다시피, 수정자는 `<>` 함수를 사용하여 합성할 수 있습니다.
즉, 수정자는 `Semigroup` 타입클래스의 인스턴스를 구현했다는 것을 의미합니다!

이러한 인터페이스로 인해, 우리는 모든 수정자 옵션을 제공할 필요가 없이 필요한 항목만 제공하면 됩니다.
따라서 우리가 짧은 플래그 이름을 가지고 싶지 않다면, 추가할 필요가 없습니다.

#### Functor

우리가 정의한 데이터 타입을 위해, `Parser FilePath`를 정의했다는 것은
우리가 원하는 방향으로 큰 파서를 만들 수 있는 좋은 시작이지만, `ConvertSingle`에 필요한 것은 아닙니다.
`SingleInput`과 `SingleOutput`을 위한 `Parser`가 필요합니다.
만약 `FilePath`가 있다면, `InputFile` 생성자를 사용하여 `SingleInput`으로 변환할 수 있습니다.
`InputFile`도 함수라는 것을 기억하세요:

```haskell
InputFile :: FilePath -> SingleInput
OutputFile :: FilePath -> SingleOutput
```

그러나 파서를 변환하려면 다음과 같은 타입의 함수가 필요합니다:

```haskell
f :: Parser FilePath -> Parser SingleInput
g :: Parser FilePath -> Parser SingleOutput
```

다행히도, `Parser` 인터페이스는 `FilePath -> SingleInput`와 같은 함수를 "lift"하는 함수를 제공합니다.
즉 `Parser FilePath -> Parser SingleInput`와 같은 타입의 함수를 만들어줍니다.
이 함수는 어떠한 입력과 출력 모두에 대해 작동하기에 `a -> b` 타입의 함수가 있다면
인자로 전달해 `Parser a -> Parser b` 타입의 새 함수를 얻을 수 있습니다.

이러한 함수를 `fmap`이라고 부릅니다:

```haskell
fmap :: (a -> b) -> Parser a -> Parser b

-- 또는 중위 표기법으로
(<$>)  :: (a -> b) -> Parser a -> Parser b
```

우리는 이전에 다른 타입의 인터페이스에서 `fmap`을 보았습니다:

```haskell
fmap :: (a -> b) -> [a] -> [b]

fmap :: (a -> b) -> IO a -> IO b
```

`fmap`은 `<>`, `show`와 같은 타입클래스 함수입니다.
이는 [`Functor`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Functor.html#t:Functor)라는 타입클래스에 속합니다:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

그리고 다음과 같은 법칙이 있습니다:

```haskell
-- 1. 항등 법칙:
--    값들을 변경하지 않으면 아무것도 변경되지 않아야 합니다.
fmap id = id

-- 2. 합성 법칙:
--    lift한 함수를 합성하는 것은 fmap으로 lift한 함수를 합성하는 것과 같습니다.
fmap (f . g) == fmap f . fmap g
```

`fmap`을 구현하고 위 법칙을 따르는 타입은 `Functor`의 인스턴스가 될 수 있습니다.

> `f`의 kind가 `* -> *`라는 것을 기억하세요.
> `fmap`의 타입 시그니처를 통해 `f`의 kind를 추론할 수 있습니다:
>
> 1. `a`와 `b`의 kind는 `*`입니다. 왜냐하면 함수의 인자/반환 타입으로 사용되기 때문입니다.
> 2. `f a`의 kind는 `*`입니다. 왜냐하면 함수의 인자로 사용되기 때문입니다.
> 3. 그러므로 `f`의 kind는 `* -> *`입니다.

데이터 타입 하나를 선택해 `Functor` 인스턴스를 구현해봅시다.
먼저 `* -> *` kind를 가진 데이터 타입을 선택해야 합니다. `Maybe`가 적합합니다.
이제 `fmap :: (a -> b) -> Maybe a -> Maybe b` 함수를 구현해야 합니다.
다음은 매우 간단하고 (그리고 잘못된) 구현입니다:

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe func maybeX = Nothing
```

한 번 컴파일해보세요! 성공적으로 컴파일됩니다! 하지만 불행하게도 첫 번째 법칙을 만족하지 않습니다.
`fmap id = id`는 `mapMaybe id (Just x) == Just x`를 의미합니다.
그러나 정의에서 `mapMaybe id (Just x) == Nothing`이라는 것을 명확히 알 수 있습니다.

이는 하스켈이 법칙을 만족할 수 있게 보장해주지 않는다는 것과 이러한 법칙이 중요하다는 것을 보여줍니다.
법칙을 만족하지 않는 `Functor` 인스턴스는 우리가 기대하는 것과 다르게 동작할 것입니다.
다시 한 번 시도해봅시다!

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe func maybeX =
  case maybeX of
    Nothing -> Nothing
    Just x -> Just (func x)
```

이 `mapMaybe`는 법칙을 만족합니다. 이는 대수학을 통해 증명할 수 있습니다.
- 만약 치환을 통해 등식의 한쪽에서 다른 쪽으로 도달할 수 있다면, 법칙은 성립합니다.

Functor는 매우 중요한 타입클래스이며, 많은 타입이 이 인터페이스를 구현합니다.
우리가 알고 있는 것처럼, `IO`, `Maybe`, `[]`, `Parser` 모두 `* -> *` kind를 가지며
그들의 "payload" 타입에 대해 `fmap`을 사용할 수 있습니다.

> 종종 사람들은 타입클래스가 무엇을 의미하는지에 대한 비유나 은유를 찾으려고 합니다.
> 하지만 Functor`와 같은 재미있는 이름의 타입클래스는 일반적으로 모든 경우에 적합한
> 비유나 은유를 가지고 있지 않습니다. 은유를 포기하고 법칙을 가진 인터페이스로 그 자체로 생각하는 것이 더 쉽습니다.

`Parser`에 대해 `fmap`을 사용해, `FilePath`를 반환하는 파서를 `SingleInput` 또는 `SingleOutput`을 반환하는 파서로 변환할 수 있습니다:

```haskell
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser -- fmap 과 <$> 는 같은 의미입니다.
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )
```

#### Applicative

Now that we have two parsers,
`pInputFile :: Parser SingleInput`
and `pOutputFile :: Parser SingleOutput`,
we want to _combine_ them as `Options`. Again, if we only had
`SingleInput` and `SingleOutput`, we could use the constructor `ConvertSingle`:

```haskell
ConvertSingle :: SingleInput -> SingleOutput -> Options
```

Can we do a similar trick to the one we saw before with `fmap`?
Does a function exist that can lift a binary function to work
on `Parser`s instead? One with this type signature:

```
???
  :: (SingleInput -> SingleOutput -> Options)
  -> (Parser SingleInput -> Parser SingleOutput -> Parser Options)
```

Yes. This function is called `liftA2` and it is from the `Applicative`
type class. `Applicative` (also known as applicative functor) has three
primary functions:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (<*>) :: f (a -> b) -> f a -> f b
```

[`Applicative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Applicative)
is another very popular type class with many instances.

Just like any `Monoid` is a `Semigroup`, any `Applicative`
is a `Functor`. This means that any type that wants to implement
the `Applicative` interface should also implement the `Functor` interface.

Beyond what a regular functor can do, which is to lift a function over
a certain `f`, applicative functors allow us to apply a function to
_multiple instances_ of a certain `f`, as well as "lift" any value of type `a` into an `f a`.

You should already be familiar with `pure`, we've seen it when we
talked about `IO`. For `IO`, `pure` lets us create an `IO` action
with a specific return value without doing IO.
With `pure` for `Parser`, we can create a `Parser` that when run
will return a specific value as output without doing any parsing.

`liftA2` and `<*>` are two functions that can be implemented in
terms of one another. `<*>` is actually the more useful one between
the two. Because when combined with `fmap` (or rather the infix version `<$>`),
it can be used to apply a function with many arguments, instead of just two.

To combine our two parsers to one, we can use either `liftA2` or
a combination of `<$>` and `<*>`:

```haskell
-- with liftA2
pConvertSingle :: Parser Options
pConvertSingle =
  liftA2 ConvertSingle pInputFile pOutputFile

-- with <$> and <*>
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pInputFile <*> pOutputFile
```

Note that both `<$>` and `<*>` associate to the left,
so we have invisible parenthesis that look like this:

```haskell
pConvertSingle :: Parser Options
pConvertSingle =
  (ConvertSingle <$> pInputFile) <*> pOutputFile
```

Let's take a deeper look at the types of the sub-expressions
we have here, to prove that this type-checks:

```haskell
pConvertSingle :: Parser Options

pInputFile :: Parser SingleInput
pOutputFile :: Parser SingleOutput

ConvertSingle :: SingleInput -> SingleOutput -> Options

(<$>) :: (a -> b) -> Parser a -> Parser b
  -- Specifically, here `a` is `SingleInput`
  -- and `b` is `SingleOutput -> Options`,

ConvertSingle <$> pInputFile :: Parser (SingleOutput -> Options)

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- Specifically, here `a -> b` is `SingleOutput -> Options`
  -- so `a` is `SingleOutput` and `b` is `Options`

-- So we get:
(ConvertSingle <$> pInputFile) <*> pOutputFile :: Parser Options
```

With `<$>` and `<*>` we can chain as many parsers (or any applicative really)
as we want. This is because of two things: currying and parametric polymorphism.
Because functions in Haskell take exactly one argument and return exactly one,
any multiple argument function can be represented as `a -> b`.

> You can find the laws for the applicative functors in this article called
> [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Laws_2), which
> talks about various useful type classes and their laws.

Applicative functor is a very important concept and will appear in various
parser interfaces (not just for command-line arguments, but also JSON
parsers and general parsers), I/O, concurrency, non-determinism, and more.
The reason this library is called optparse-applicative is because
it uses the `Applicative` interface as the main API for
constructing parsers.

---

**Exercise**: create a similar interface for the `ConvertDir` constructor of `Options`.

<details><summary>Solution</summary>

```haskell
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir
```

</details>

---

#### Alternative

One thing we forgot about is that each input and output for
`ConvertSingle` could also potentially use the standard input and output instead.
Up until now we only offered one option: reading from or writing to a file
by specifying the flags `--input` and `--output`.
However, we'd like to make these flags optional, and when they are
not specified, use the alternative standard i/o. We can do that by using
the function `optional` from `Control.Applicative`:

```haskell
optional :: Alternative f => f a -> f (Maybe a)
```

`optional` works on types which implement instances of the
[`Alternative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Alternative) type class:

```haskell
class Applicative f => Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a
```

`Alternative` looks very similar to the `Monoid` type class,
but it works on applicative functors. This type class isn't
very common and is mostly used for parsing libraries as far as I know.
It provides us with an interface to combine two `Parser`s -
if the first one fails to parse, try the other.
It also provides other useful functions such as `optional`,
which will help us with our case:

```haskell
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile
```

Note that with `fromMaybe :: a -> Maybe a -> a` we can extract
the `a` out of the `Maybe` by supplying a value for the `Nothing` case.

Now we can use these more appropriate functions in `pConvertSingle` instead:

```haskell
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput
```

#### Commands and subparsers

We currently have two possible operations in our interface,
convert a single source, or convert a directory. A nice interface for
selecting the right operation would be via commands.
If the user would like to convert a single source, they can use
`convert`, for a directory, `convert-dir`.

We can create a parser with commands with the `subparser` and `command`
functions:

```haskell
subparser :: Mod CommandFields a -> Parser a

command :: String -> ParserInfo a -> Mod CommandFields a
```

`subparser` takes _command modifiers_ (which can be constructed
with the `command` function) as input, and produces a `Parser`.
`command` takes the command name (in our case "convert" or "convert-dir")
and a `ParserInfo a`, and produces a command modifier. As we've seen
before these modifiers have a `Monoid` instance and they can be
composed, meaning that we can append multiple commands to serve as alternatives.

A `ParserInfo a` can be constructed with the `info` function:

```haskell
info :: Parser a -> InfoMod a -> ParserInfo a
```

This function wraps a `Parser` with some additional information
such as a helper message, description, and more, so that the program
itself and each sub command can print some additional information.

Let's see how to construct a `ParserInfo`:

```haskell
pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")
```

Note that `helper` adds a helper output screen in case the parser fails.

Let's also build a command:

```haskell
pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo
```

Try creating a `Parser Options` combining the two options with `subparser`.

<details><summary>Solution</summary>

```haskell
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )
```

</details>

#### ParserInfo

Since we finished building a parser, we should wrap it up in a `ParserInfo`
and add some information to it to make it ready to run:

```haskell
opts :: ParserInfo Options
opts =
  info (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )
```

### Running a parser

`optparse-applicative` provides a non-`IO` interface to parse arguments,
but the most convenient way to use it is to let it take care of fetching
program arguments, try to parse them, and throw errors and help messages in case
it fails. This can be done with the function `execParser :: ParserInfo a -> IO a`.

We can place all this options parsing stuff in a new module
and then import it from `app/Main.hs`. Let's do that.
Here's what we have up until now:

<details><summary>app/OptParse.hs</summary>

```haskell
-- | Command-line options parsing

module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

------------------------------------------------
-- * Our command-line options model

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

-- | A single input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

-- | A single output sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

------------------------------------------------
-- * Parser

-- | Parse command-line options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info (pOptions <**> helper)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

-- | Parser for all options
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )

------------------------------------------------
-- * Single source to sink conversion parser

-- | Parser for single source to sink option
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

-- | Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- | Input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

-- | Output file parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

------------------------------------------------
-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir

-- | Parser for input directory
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

-- | Parser for output directory
pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )
```

</details>

### Pattern matching on Options

After running the command-line arguments parser, we can pattern match
on our model and call the right functions. Currently, our program
does not expose this kind of API. So let's go to our `src/HsBlog.hs`
module and change the API. We can delete `main` from that file and
add two new functions instead:

```haskell
convertSingle :: Html.Title -> Handle -> Handle -> IO ()

convertDirectory :: FilePath -> FilePath -> IO ()
```

[`Handle`](https://hackage.haskell.org/package/base-4.16.4.0/docs/System-IO.html#t:Handle)
is an I/O abstraction over file system objects, including `stdin` and `stdout`.
Before, we used `writeFile` and `getContents` - these functions either
get a `FilePath` to open and work on, or they assume the `Handle` is the standard I/O.
We can use the explicit versions that take a `Handle` from `System.IO` instead:

```haskell
convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)
```

We will leave `convertDirectory` unimplemented for now and implement it in the next chapter.

In `app/Main.hs`, we will need to pattern match on the `Options` and
prepare to call the right functions from `HsBlog`.

Let's look at our full `app/Main.hs` and `src/HsBlog.hs`:

<details><summary>app/Main.hs</summary>

```haskell
-- | Entry point for the hs-blog-gen program

module Main where

import OptParse
import qualified HsBlog

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output

    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

------------------------------------------------
-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. use y or n" *>
          confirm
```

</details>

<details><summary>src/HsBlog.hs</summary>

```haskell
-- HsBlog.hs
module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
```

</details>

We need to make a few small changes to the `.cabal` file.

First, we need to add the dependency `directory` to the `executable`,
because we use the library `System.Directory` in `Main`.

Second, we need to list `OptParse` in the list of modules in
the `executable`.

```diff
 executable hs-blog-gen
   import: common-settings
   hs-source-dirs: app
   main-is: Main.hs
+  other-modules:
+    OptParse
   build-depends:
       base
+    , directory
     , optparse-applicative
     , hs-blog
   ghc-options:
     -O
```

## Summary

We've learned about a new fancy library called `optparse-applicative`
and used it to create a fancier command-line interface in a declarative way.
See the result of running `hs-blog-gen --help` (or the equivalent
`cabal`/`stack` commands we discussed in the last chapter):

```
hs-blog-gen - a static blog generator

Usage: hs-blog-gen COMMAND
  Convert markup files or directories to html

Available options:
  -h,--help                Show this help text

Available commands:
  convert                  Convert a single markup source to html
  convert-dir              Convert a directory of markup files to html
```

Along the way we've learned two powerful new abstractions, `Functor`
and `Applicative`, as well as revisited an abstraction
called `Monoid`. With this library we've seen another example
of the usefulness of these abstractions for constructing APIs and EDSLs.

We will continue to meet these abstractions in the rest of the book.

---

**Bonus exercise**: Add another flag named `--replace` to indicate that
if the output file or directory already exists, it's okay to replace them.

---

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/d0d76aad632fe3abd8701e44db5ba687e0c7ac96)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/d0d76aad632fe3abd8701e44db5ba687e0c7ac96).
