# 테스트

이제 블로그 생성기에 몇 가지 테스트를 추가하고자 합니다.
적어도 몇 가지 회귀 테스트를 추가하여 마크업 구문 분석 코드, HTML 생성 코드 또는 마크업에서 HTML 코드로의 변환 작업에서 실수를 하더라도 문제를 알려주는 안전장치를 마련하기 위함입니다.

테스트를 작성하기 위해 [Hspec](https://hspec.github.io/) 테스트 프레임워크를 사용할 예정입니다.
[tasty](https://hackage.haskell.org/package/tasty)와 같은 다른 테스트 프레임워크도 있지만, 개인적으로 Hspec의 문서가 좋아서 이를 사용하겠습니다.

## 초기 설정

### Cabal 파일 수정

새로운 테스트 스위트를 정의하기 위해 `hs-blog-gen.cabal` 파일에 새로운 섹션을 정의할 것입니다.
`test-suite`라는 섹션이며, `library`와 `executable` 섹션과 매우 유사합니다.

[Cabal 문서](https://cabal.readthedocs.io/en/stable/cabal-package.html#test-suites)
에 테스트 스위트를 정의하는 방법에 대한 설명이 있습니다.
우리는 `exitcode-stdio-1.0` 인터페이스를 사용할 것입니다. 다양한 설정과 옵션을 살펴보겠습니다.

```cabal
test-suite hs-blog-gen-test
  import: common-settings
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs

  -- other-modules:
  build-depends:
      base
    , hspec
    , hspec-discover
    , raw-strings-qq
    , hs-blog
  ghc-options:
    -O -threaded -rtsopts -with-rtsopts=-N
  build-tool-depends:
    hspec-discover:hspec-discover
```

- `hs-source-dirs: test` - 테스트 스위트의 소스 파일 디렉토리
- `main-is: Spec.hs` - 테스트 스위트의 진입점
- `other-modules` - 테스트 스위트의 모듈정의, 현재는 존재하지 않아서 주석처리 하였습니다
- `build-depends` - 사용할 패키지:
  - [`base`](https://hackage.haskell.org/package/base) -
    하스켈 표준 라이브러리, 이전에 사용했던 것과 같습니다
  - [`hspec`](https://hackage.haskell.org/package/hspec) -
    이번에 사용할 테스트 프레임워크
  - [`hspec-discover`](https://hackage.haskell.org/package/hspec-discover) -
    자동으로 Hspec 테스트를 찾아줍니다
  - [`raw-strings-qq`](https://hackage.haskell.org/package/raw-strings-qq) -
    문자열 리터럴을 작성하기 위한 추가적인 문법을 제공합니다
  - `hs-blog` - 우리가 작성한 라이브러리
- [`ghc-options`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-ghc-options) -
  GHC의 추가 옵션과 플래그:
  - [`-O`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-optimisation.html#options-optimise) -
    최적화 옵션을 사용하여 컴파일합니다
  - [`-threaded`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--threaded) -
    단일 코어 런타임 대신 멀티 코어 런타임을 사용합니다.
    경험상 멀티 코어 런타임이 약간 느리지만, 실제로 여러 코어를 사용하는 코드(예를 들어 테스트 프레임워크에서 테스트를 병렬로 실행하는 경우)를 작성할 때는 성능 향상을 얻을 수 있습니다.
  - [`-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--rtsopts[=%E2%9F%A8none|some|all|ignore|ignoreAll%E2%9F%A9]) -
    애플리케이션에 명령행 인수를 전달하여 하스켈 런타임 시스템을 구성할 수 있습니다.
  - [`-with-rtsopts=-N`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--with-rtsopts=%E2%9F%A8opts%E2%9F%A9) -
    링크 시간에 프로그램에 대한 특정 기본 옵션을 설정합니다.
    특히, [`-N`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-concurrent.html#rts-flag--N%20%E2%9F%A8x%E2%9F%A9)
    옵션은 프로그램에서 사용할 코어의 수를 설정합니다.
- [`build-tool-depends`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-build-tool-depends) -
  패키지 빌드를 위한 특별한 실행파일을 지정합니다.
  예제에서는 [`hspec-discover`](https://hackage.haskell.org/package/hspec-discover) 패키지에 포함된 `hspec-discover` 실행파일을 사용합니다.
  이 실행파일은 테스트 소스 디렉토리를 검사하여 모든 `Spec` 파일을 찾고, 찾은 테스트를 실행하는 프로그램의 진입점을 생성합니다.

### Hspec 탐색

`hspec-discover`가 작동하려면, 테스트 스위트의 "main" 파일에 다음을 추가해야 합니다.
우리의 경우 `test/Spec.hs`입니다:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

이제 `hspec-discover`가 자동으로 `main`을 정의할 것입니다.
`stack test` 또는 `cabal test` (선택한 항목에 따라)를 사용하여 테스트를 실행할 수 있습니다.
아직 테스트를 정의하지 않았으므로 출력은 다음과 같습니다:

```sh
Finished in 0.0000 seconds
0 examples, 0 failures
```

새로운 Hspec 테스트를 추가할 때마다 `hspec-discover`가 자동으로 찾아서 실행할 것입니다.
(물론 `cabal` 파일의 `other-modules` 섹션에 추가해야 합니다).

`hspec-discover`가 모듈을 테스트 모듈로 인식하려면, 다음 규칙을 따라야 합니다:

1. 모듈 이름은 `Spec`로 끝나야 합니다.
2. (테스트 의미하는) `spec :: Spec`이라는 값을 정의해야 합니다. 그리고 모듈 외부로 내보내야 합니다.
   (예를 들어, 모듈의 내보내기 목록에 추가해야 합니다)

## 테스트 작성하기

첫 번째 테스트를 작성해 보겠습니다.
마크업 파싱을 테스트하는 모듈을 작성해봅시다.
`MarkupParsing.hs`라는 새로운 파일을 생성하고, 다음과 같은 모듈을 가져옵니다:

```haskell
module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup
```

`Hspec`은 테스트 사양(`Spec`)을 기술하고, 조합하고, 중첩하는 monadic 인터페이스를 제공합니다.

`describe` 함수를 사용하여 테스트 그룹을 기술할 수 있습니다.
또한 `it` 함수를 사용하여 새로운 테스트를 추가할 수 있습니다.
`shouldBe`와 같은 함수를 사용하여 두 값을 비교하고, `Eq` 인스턴스를 사용하여 두 값이 동일한지 확인할 수 있습니다.
만약 동일하다면 테스트는 통과하고, 그렇지 않다면 테스트는 실패하고 에러 메시지를 출력합니다.

무조건 실패하는 테스트를 먼저 작성해보겠습니다!

```haskell
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        [Heading 1 "bug"]
```

cabal 파일의 `other-modules` 목록에 이 모듈을 추가한 후

```haskell
  other-modules:
    MarkupParsingSpec
```

테스트를 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```haskell
MarkupParsing
  Markup parsing tests
    empty FAILED [1]

Failures:

  test/MarkupParsingSpec.hs:10:7:
  1) MarkupParsing, Markup parsing tests, empty
       expected: [Heading 1 "bug"]
        but got: []

  To rerun use: --match "/MarkupParsing/Markup parsing tests/empty/"

Randomized with seed 763489823

Finished in 0.0004 seconds
1 example, 1 failure
```

계층 트리(모듈, 그룹, 테스트) 형태로 실행되는 테스트 결과가 출력됩니다.
이를 통해 어떤 테스트가 실행되었고, 테스트가 통과했는지 실패했는지, 실패했다면 어떤 출력과 기대 출력이 있는지 알 수 있습니다.

테스트를 통과하도록 수정해보겠습니다.

```haskell
      shouldBe
        (parse "")
        []
```

이제 테스트가 통과하고 다음과 같은 결과를 얻을 수 있습니다:

```haskell
MarkupParsing
  Markup parsing tests
    empty

Finished in 0.0001 seconds
1 example, 0 failures
```

몇 가지 테스트를 더 추가해보겠습니다:

```haskell
    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]
```

이후 테스트를 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```sh
MarkupParsing
  Markup parsing tests
    Test empty
    paragraph
    heading 1
    code

Finished in 0.0003 seconds
4 examples, 0 failures
```

지금까지 Hspec을 사용하여 테스트를 작성하는 방법을 살펴보았습니다.
`describe`를 사용하여 `Spec`을 중첩한 트리구조를 만들 수 있다는 점이 중요합니다.
물론 테스트를 다른 함수와 모듈로 이동시켜서 테스트 스위트를 더 잘 구성할 수도 있습니다.

예들 들어, 다음과 같이 테스트를 작성할 수 있습니다:

```haskell
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple

simple :: Spec
simple = do
  describe "simple" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]
```

또한 테스트에서 사용할 수 있는, `shouldBe`와 비슷한 다른 "검증문(expectations)"도 있습니다.
[Hspec 튜토리얼](https://hspec.github.io/expectations.html)
또는
[haddock 문서](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html)
에서 그 목록을 확인할 수 있습니다.

### Raw 문자열

다중 라인 문자열을 작성하거나, "code" 테스트 케이스에서 했던 것처럼 문자열을 이스케이프 하지 않으려면
[raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq)
라이브러리를 사용할 수 있습니다.
이 라이브러리는
[`QuasiQuotes`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/template_haskell.html#extension-QuasiQuotes)
언어 확장을 사용합니다.
`QuasiQuotes`는 하스켈의 문법을 확장하는 메타 프로그래밍 확장입니다.

quasi-quote는 `[quoter| string |]` 형태를 가지며, 여기서 quoter는 우리가 사용하고자 하는 문법을 제공하는 함수의 이름이고, string은 입력입니다.

예를 들어
[raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq-1.1/docs/Text-RawString-QQ.html)
에 정의된 quoter `r`을 사용하여 다중 라인과 이스케이프되지 않은 문자열을 작성할 수 있습니다!
이를 활용해 [이전에 작성한 코드](./04-markup/01-data-type.md#exercises)에 대한 테스트를 작성해보겠습니다.

```haskell
{-# language QuasiQuotes #-}

...

import Text.RawString.QQ

...

example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]
```

이제 다중 라인 테스트를 추가합니다:

```haskell
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    multiline


multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result


example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example3Result :: Document
example3Result =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]
```

테스트를 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```haskell
MarkupParsing
  Markup parsing tests
    simple
      Test empty
      paragraph
      heading 1
      code
    Multi-line tests
      example3

Finished in 0.0004 seconds
5 examples, 0 failures
```

---

**연습문제**: [이전 연습문제](./04-markup/01-data-type.md#exercises)의 네 번째 예제에 대한 테스트를 추가하세요.

<details><summary>정답</summary>

```haskell
multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result

    it "example4" $
      shouldBe
        (parse example4)
        example4Result


example4 :: String
example4 = [r|
* Compiling programs with ghc

Running ghc invokes the Glasgow Haskell Compiler (GHC),
and can be used to compile Haskell modules and programs into native
executables and libraries.

Create a new Haskell source file named hello.hs, and write
the following code in it:

> main = putStrLn "Hello, Haskell!"

Now, we can compile the program by invoking ghc with the file name:

> ➜ ghc hello.hs
> [1 of 1] Compiling Main             ( hello.hs, hello.o )
> Linking hello ...

GHC created the following files:

- hello.hi - Haskell interface file
- hello.o - Object file, the output of the compiler before linking
- hello (or hello.exe on Microsoft Windows) - A native runnable executable.

GHC will produce an executable when the source file satisfies both conditions:

# Defines the main function in the source file
# Defines the module name to be Main, or does not have a module declaration

Otherwise, it will only produce the .o and .hi files.
|]

example4Result :: Document
example4Result =
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock
    [ "main = putStrLn \"Hello, Haskell!\""
    ]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock
    [ "➜ ghc hello.hs"
    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]
```

</details>

---

## 병렬 테스트 실행

특별한 설정을 하지 않았다면, Hspec은 모든 테스트를 메인 스레드에서 순차적으로 실행합니다.

테스트를 병렬로 실행할 수 있는 몇 가지 방법이 있습니다.
하나는 `Spec`을 직접 `parallel` 함수에 전달하여 병렬로 실행하도록 표시하는 것이고,
다른 하나는 `hspec-discover`를 사용하여 자동으로 `parallel`을 적용하는 /hook/ 을 생성하는 것입니다.

[Hspec 문서](https://hspec.github.io/parallel-spec-execution.html#running-all-tests-in-parallel-with-hspec-discover)
를 참고하여 두 가지 방법을 모두 시도해보세요.
우리는 이미 cabal 파일에서 스레드 기반 런타임을 활성화하고 멀티 코어를 사용하도록 설정했습니다.

## 요약

이번 장에서 소개한 테스트 방법에 대한 내용은 빙산의 일각에 불과합니다.
[속성 테스트](https://www.scs.stanford.edu/16wi-cs240h/slides/testing.html) 또는
[골든 테스트](https://ro-che.info/articles/2017-12-04-golden-tests)
에 대해서는 언급조차 하지 않았습니다.
또한 예외 테스트, IO 코드 테스트, 테스트 분석, 벤치마크 등등에 대해서도 다루지 않았습니다.
이를 다루기에는 너무 방대한 주제이기 때문입니다!

이번 장을 통해 프로젝트에 테스트 작성을 시작하기 위한 기본적인 내용을 전달하고자 했습니다.
선택한 테스트 프레임워크의 튜토리얼을 참고하고, 직접 테스트에 대한 추가적인 문서를 읽어보세요.

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/da1615b6e0a2a4ff2728528240d790754853bf02)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/da1615b6e0a2a4ff2728528240d790754853bf02) 를 확인할 수 있습니다.
