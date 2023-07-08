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

이제 `pInputFile :: Parser SingleInput`과 `pOutputFile :: Parser SingleOutput` 두 개의 파서를 가지고 있습니다.
우리는 이 두 파서를 *결합*해 `Options`를 만들려고 합니다.
`SingleInput`과 `SingleOutput`이 있다면 `ConvertSingle` 생성자를 사용해볼 수 있습니다:

```haskell
ConvertSingle :: SingleInput -> SingleOutput -> Options
```

혹시 이전에 `fmap`을 활용해 본 것처럼 비슷한 트릭을 사용할 수 있을까요?
이진 함수(binary function)를 *리프트*해 `Parser`에서도 동작하게 하는 함수가 존재할까요?
존재한다면 아마 이런 타입 시그니처를 가지고 있을 것입니다:

```
???
  :: (SingleInput -> SingleOutput -> Options)
  -> (Parser SingleInput -> Parser SingleOutput -> Parser Options)
```

네. 이런 함수가 존재합니다. 이 함수는 `liftA2`라고 불리며 `Applicative` 타입클래스에 정의되어 있습니다.
`Applicative` (또는 applicative functor)는 세 개의 주요 함수를 가지고 있습니다:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (<*>) :: f (a -> b) -> f a -> f b
```

[`Applicative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Applicative)
는 많은 인스턴스가 존재하는 또 다른 매우 인기있는 타입클래스입니다.

모든 `Monoid`는 `Semigroup`인 것처럼, 모든 `Applicative`은 `Functor`입니다.
이는 `Applicative` 인터페이스를 구현하고자 하는 타입은 `Functor` 인터페이스도 구현해야 한다는 것을 의미합니다.

특정 함수 `f`를 리프트 하는 일반적인 functor의 기능을 넘어, applicative functor는
타입 `a`를 `f a`로 "리프트"하는 것 외에도, 특정 `f`의 *여러 인스턴스*에 함수를 적용할 수 있습니다.

아마 `pure`는 이미 익숙할 것입니다. 예전에 `IO`에 대해 이야기할 때 보았을 것입니다.
`IO`에 대해, `pure`는 IO를 수행하지 않고도 특정 반환 값을 가지는 `IO` 액션을 만들어줍니다.
`Parser`에 대해 `pure`를 사용하면, 파싱을 하지 않고도 특정 값을 반환하는 `Parser`를 만들 수 있습니다.

`liftA2`와 `<*>`는 둘 다 서로를 활용해 구현할 수 있는 함수입니다.
사실 `<*>`가 두 함수 중 더 유용합니다. 
왜냐하면 `fmap` (또는 중위 연산자 `<$>`)과 함께 사용하면 두 개 이상의 많은 인자를 가진 함수에도 적용할 수 있기 때문입니다.

두 파서를 하나로 결합하기위해, `liftA2` 또는 `<$>`와 `<*>`의 조합을 사용할 수 있습니다:

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

`<$>`와 `<*>`는 모두 왼쪽으로 결합되므로, 다음과 같은 보이지 않는 괄호가 있습니다:

```haskell
pConvertSingle :: Parser Options
pConvertSingle =
  (ConvertSingle <$> pInputFile) <*> pOutputFile
```

위 코드가 타입체크에 성공하는지 증명하기 위해, 각 하위 표현식의 타입을 살펴봅시다:

```haskell
pConvertSingle :: Parser Options

pInputFile :: Parser SingleInput
pOutputFile :: Parser SingleOutput

ConvertSingle :: SingleInput -> SingleOutput -> Options

(<$>) :: (a -> b) -> Parser a -> Parser b
  -- 여기서 `a`는 `SingleInput`이고 `b`는 `SingleOutput -> Options`입니다.

ConvertSingle <$> pInputFile :: Parser (SingleOutput -> Options)

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- 여기서 `a -> b`는 `SingleOutput -> Options`이며, `a`는 `SingleOutput`이고 `b`는 `Options`입니다.

-- 따라서
(ConvertSingle <$> pInputFile) <*> pOutputFile :: Parser Options
```

`<$>`과 `<*>`를 사용하면 원하는 만큼 많은 파서를 연결할 수 있습니다.
이는 커링(currying)과 매개변수 다형성(parametric polymorphism) 두 가지 이유 때문입니다.
Haskell에서 함수는 정확히 하나의 인자를 받고 정확히 하나의 값을 반환하기 때문에,
여러 인자를 가진 함수는 `a -> b`와 같이 표현할 수 있습니다.

> applicative functor의 법칙에 대한 내용은 [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Laws_2)글을 참고하세요.
> 이 글은 다양한 유용한 타입클래스와 그들의 법칙에 대해 이야기합니다.

Applicative functor는 매우 중요한 개념이며 다양한 파서 인터페이스 (명령줄 뿐만아니라 JSON이나 일반적인 파서), I/O, 동시성, 비결정성 등에서 사용됩니다.
이 라이브러리의 이름이 optparse-applicative인 이유도 파서를 만들기 위해 `Applicative` 인터페이스를 주요 API로 사용하기 때문입니다.

---

**연습문제**: `Options`의 생성자 `ConvertDir`에 대해서도 유사한 인터페이스를 만드세요.

<details><summary>정답</summary>

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

사실 `ConvertSingle`는 입출력으로 표준 입력과 출력을 사용할 수도 있습니다.
지금까지 `--input`과 `--output` 플래그를 사용하여 파일을 읽거나 쓰는 하나의 옵션만 제공했습니다.
이제 우리는 이 플래그를 선택적으로 사용할 수 있게해, 지정하지 않았다면 표준 입출력을 사용하도록 만들려고 합니다.
`Control.Applicative`의 `optional` 함수를 사용하여 이를 수행할 수 있습니다:

```haskell
optional :: Alternative f => f a -> f (Maybe a)
```

`optional`은 [`Alternative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Alternative) 타입 클래스의 인스턴스인 타입에 대해 작동합니다:

```haskell
class Applicative f => Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a
```

`Alternative`는 `Monoid` 타입 클래스와 매우 유사하지만, applicative functor에서 작동합니다.
이 타입 클래스는 자주 사용되지 않으며, 주로 파싱 라이브러리에서만 사용됩니다.
이는 두 개의 `Parser`를 결합할 수 있는 인터페이스를 제공합니다 - 첫 번째 파서가 파싱에 실패하면 다른 파서를 시도합니다.
또한 현재 우리에게 도움이 되는 `optional`과 같은 유용한 함수를 제공합니다.

```haskell
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile
```

`fromMaybe :: a -> Maybe a -> a`를 사용하여 `Maybe` 값을 `a`로 추출할 수 있습니다.
이는 `Nothing`이 아닌 경우에는 `a`를 반환하고, `Nothing`인 경우에는 기본값을 반환합니다.


이제 더 적절한 함수들을 사용해 `pConvertSingle`을 다시 작성할 수 있습니다:

```haskell
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput
```

#### 명령과 서브파서

현재 인터페이스는 두 가지 가능한 동작을 가지고 있습니다.
단일 소스를 변환하거나 디렉토리를 변환하는 것입니다.
적절한 동작을 선택하기 위한 좋은 인터페이스는 명령을 통해 선택하는 것입니다.
사용자가 단일 소스를 변환하려면 `convert`, 디렉토리를 변환하려면 `convert-dir`를 사용할 수 있습니다.

우리는 `subparser`와 `command` 함수를 사용하여 이를 수행할 수 있습니다:

```haskell
subparser :: Mod CommandFields a -> Parser a

command :: String -> ParserInfo a -> Mod CommandFields a
```

`subparser`는 *명령 수정자(command modifiers)* (이는 `command` 함수로 만들 수 있습니다)를 입력으로 받아 `Parser`를 생성합니다.
`command`는 명령 이름("convert" 또는 "convert-dir")과 `ParserInfo a`를 입력으로 받아 명령 수정자를 생성합니다.
이전에 보았던 것처럼 이러한 수정자들은 `Monoid` 인스턴스를 가지고 있으며, 여러 명령을 옵션으로 사용할 수 있도록 합칠 수 있습니다.

`ParserInfo a`는 `info` 함수를 사용하여 생성할 수 있습니다:

```haskell
info :: Parser a -> InfoMod a -> ParserInfo a
```

이 함수는 주어진 `Parser`를 도움말, 설명과 같은 추가정보를 감싸, 프로그램과 각 서브 명령이 추가정보를 출력할 수 있도록 합니다.

`ParserInfo`는 어떻게 만드는지 살펴보겠습니다:

```haskell
pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")
```

`helper`는 파서가 실패할 때 출력할 도움말을 추가합니다.

명령도 생성해봅시다:

```haskell
pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo
```

`subparser`를 사용하여 두 옵션을 결합해 `Parser Options`를 만들어보세요.

<details><summary>정답</summary>

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

이제 파서를 완성했으므로, `ParserInfo`로 감싸고 몇 가지 정보를 추가하여 실행할 준비가 되었습니다:

```haskell
opts :: ParserInfo Options
opts =
  info (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )
```

### 파서 실행하기

`optparse-applicative`는 인자를 파싱하는데 `IO`를 사용하지 않는 인터페이스를 제공합니다.
하지만 가장 편리한 방법은 인자를 가져오고, 파싱을 시도하고, 실패하면 오류와 도움말 메시지를 출력하는 것을 `optparse-applicative`에 맡기는 것입니다.
이는 `execParser :: ParserInfo a -> IO a` 함수로 수행할 수 있습니다.

새로운 모듈에 이러한 옵션을 파싱하는 코드를 작성하고, `app/Main.hs`에서 가져올 수 있습니다.
다음은 지금까지 작업한 코드입니다:

<details><summary>app/OptParse.hs</summary>

```haskell
-- | 명령줄 옵션 파싱

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
-- * 명령줄 옵션 모델

-- | 모델
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

-- | 단일 입력 소스
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

-- | 단일 출력 sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

------------------------------------------------
-- * Parser

-- | 명령줄 옵션 파싱
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info (pOptions <**> helper)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

-- | 모든 옵션에 대한 파서
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
-- * 단일 소스를 위한 파서

-- | 단일 소스를 위한 옵션 파서
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | 단일 입력 소스를 위한 파서
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

-- | 단일 출력 sink를 위한 파서
pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- | 입력 파일 파서
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

-- | 출력 파일 파서
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
-- * 디렉토리 변환 파서

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir

-- | 입력 디렉토리 파서
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

-- | 출력 디렉토리 파서
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

### Options에 대한 패턴 매칭

명령줄 파서를 실행하고 나면, 모델에 대한 패턴 매칭을 통해 올바른 함수를 호출할 수 있습니다.
현재 프로그램은 이러한 종류의 API를 노출하지 않습니다. 
따라서 `src/HsBlog.hs` 모듈로 이동하고 API를 변경합니다.
`main`을 삭제하고 대신 두 개의 새 함수를 추가할 수 있습니다:

```haskell
convertSingle :: Html.Title -> Handle -> Handle -> IO ()

convertDirectory :: FilePath -> FilePath -> IO ()
```

[`Handle`](https://hackage.haskell.org/package/base-4.16.4.0/docs/System-IO.html#t:Handle)
은 파일 시스템 객체에 대한 I/O 추상화입니다. `stdin`과 `stdout`를 포함합니다.
이전에는 `writeFile`과 `getContents`를 사용했습니다. - 이 함수들은 `Handle`이 표준 I/O라고 가정하거나, `FilePath`를 열고 작업할 수 있습니다.
이제는 `System.IO`에서 `Handle`을 가져오는 명시적인 버전을 사용할 수 있습니다:

```haskell
convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)
```

지금은 `convertDirectory`의 구현은 남겨두고, 다음 장에서 구현하겠습니다.

`app/Main.hs`에서 `Options`에 대한 패턴 매칭이 필요하며 `HsBlog`의 함수들을 호출해야 합니다.

`app/Main.hs`와 `src/HsBlog.hs`의 전체 코드를 살펴보겠습니다:

<details><summary>app/Main.hs</summary>

```haskell
-- | hs-blog-gen 프로그램의 진입점

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

`.cabal` 파일에 몇 가지 작은 수정이 필요합니다.

먼저, `executable` 섹션에 `directory` 의존성을 추가해야 합니다.
왜냐하면 `Main`에서 `System.Directory` 라이브러리를 사용하기 때문입니다.

다음으로, `OptParse` 모듈을 `executable` 섹션의 `other-modules`에 추가해야 합니다.

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

## 요약

지금까지 `optparse-applicative` 라이브러리에 대해 배웠습니다.
이 라이브러리는 선언적인 방법으로 강력한 명령줄 인터페이스를 만드는 데 사용할 수 있습니다.
`hs-blog-gen --help` (또는 이전 장에서 논의한 `cabal`/`stack` 명령어를 사용해서)를 실행해, 결과를 확인해 보세요:

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

그동안 우리는 `Functor`와 `Applicative`이라는 두 가지 강력한 새로운 추상화를 배웠습니다.
또한 `Monoid`라는 추상화를 다시 살펴보았습니다.
이 라이브러리를 통해 이러한 추상화가 API와 DSL을 구축하는 데 얼마나 유용한지 알아보았습니다.

우리는 이 책의 나머지 부분에서 이러한 추상화를 계속 만나게 될 것입니다.

---


**추가 연습문제**: 출력 파일이나 디렉토리가 이미 존재하는 경우에도 덮어써도 괜찮다는 것을 의미하는 `--replace` 플래그를 추가하세요.

---

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/d0d76aad632fe3abd8701e44db5ba687e0c7ac96)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/d0d76aad632fe3abd8701e44db5ba687e0c7ac96) 를 확인할 수 있습니다.
