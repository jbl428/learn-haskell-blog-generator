# 문서 생성하기

사용자에게 우리의 프로젝트와 라이브러리를 쉽게 도입할 수 있도록 도와주는 [다양한 방법](https://documentation.divio.com/)이 있습니다.
예를 들어, 튜토리얼을 작성하거나 실행 가능한 예제를 제공하거나 시스템의 내부를 설명하거나 API 문서를 만들 수 있습니다.

이번 장에서는 [Haddock](http://www.haskell.org/haddock/)을 사용하여 주석이 달린 Haskell 소스 코드를 통해 (Hackage에서 볼 수 있는) API 문서를 생성하는 방법을 살펴보겠습니다.

## Haddock 실행하기

선호하는 패키지 관리자를 사용하여 (Haskell 세계에서는 haddock로 알려진) 프로젝트의 API 문서 를 생성할 수 있습니다.

### Cabal

`cabal haddock`을 실행하여 haddock을 생성할 수 있습니다.

```sh
➜ cabal haddock
Resolving dependencies...
Build profile: -w ghc-9.0.1 -O1
In order, the following will be built (use -v for more details):
 - hs-blog-0.1.0.0 (lib) (first run)
Configuring library for hs-blog-0.1.0.0..
Preprocessing library for hs-blog-0.1.0.0..
Running Haddock on library for hs-blog-0.1.0.0..
Haddock coverage:
   0% (  0 /  3) in 'HsBlog.Env'
  Missing documentation for:
    Module header
    Env (src/HsBlog/Env.hs:3)
    defaultEnv (src/HsBlog/Env.hs:10)
  21% (  7 / 33) in 'HsBlog.Html.Internal'
  Missing documentation for:
    Module header
    Html (src/HsBlog/Html/Internal.hs:8)
...
Documentation created:
/tmp/learn-haskell-blog-generator/dist-newstyle/build/x86_64-linux/ghc-9.0.1/hs-blog-0.1.0.0/doc/html/hs-blog/index.html
```

Cabal과 Haddock은 프로젝트를 빌드하고 HTML 페이지를 생성합니다.

```html
./dist-newstyle/build/<platform
  >/<compiler
    >/<package
      >-<version>/doc/html/<package>/</package></version></package
    ></compiler
  ></platform
>
```

이후 웹 브라우저에서 해당 디렉토리의 `index.html` 파일을 열어 패키지 문서를 볼 수 있습니다.

### Stack

`stack haddock`을 실행하여 haddock을 생성할 수 있습니다.

```sh
➜ stack haddock
...
hs-blog> build (lib + exe)
Preprocessing library for hs-blog-0.1.0.0..
Building library for hs-blog-0.1.0.0..
[1 of 7] Compiling HsBlog.Env
[2 of 7] Compiling HsBlog.Html.Internal
...
hs-blog> haddock
Preprocessing library for hs-blog-0.1.0.0..
Running Haddock on library for hs-blog-0.1.0.0..
Haddock coverage:
   0% (  0 /  3) in 'HsBlog.Env'
  Missing documentation for:
    Module header
    Env (src/HsBlog/Env.hs:3)
    defaultEnv (src/HsBlog/Env.hs:10)
  21% (  7 / 33) in 'HsBlog.Html.Internal'
  Missing documentation for:
    Module header
    Html (src/HsBlog/Html/Internal.hs:8)
...
Documentation created:
.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/index.html,
.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/hs-blog.txt
Preprocessing executable 'hs-blog-gen' for hs-blog-0.1.0.0..
...
```

Stack과 Haddock은 프로젝트를 빌드하고 HTML 페이지를 생성합니다.

```html
./.stack-work/dist/<platform
  >/Cabal-<version>/doc/html/<package>/</package></version></platform
>
```

이후 웹 브라우저에서 해당 디렉토리의 `index.html` 파일을 열어 패키지 문서를 볼 수 있습니다.

### Haddock 커버리지

Haddock은 실행하면 커버리지 보고서를 출력하고 사용자에게 공개되었지만 문서가 없는 항목들을 보여줍니다.
이러한 항목들은 모듈 헤더, 타입, 데이터 생성자, 타입 클래스, 함수, 값 등이 될 수 있습니다.

예를 들어:

```haskell
Haddock coverage:
...
   0% (  0 /  3) in 'HsBlog.Convert'
  Missing documentation for:
    Module header
    convert (src/HsBlog/Convert.hs:8)
    convertStructure (src/HsBlog/Convert.hs:23)
  67% (  2 /  3) in 'HsBlog.Directory'
  Missing documentation for:
    buildIndex (src/HsBlog/Directory.hs:80)
...
```

우리는 `HsBlog.Convert`를 전혀 문서화하지 않았고, 모듈 헤더, `convert` 함수, `convertStructure` 함수에 대한 문서가 없다는 사실을 알 수 있습니다.

반면에 `HsBlog.Directory` 모듈에는 일부 문서가 있다는 것을 알 수 있습니다!
왜 그런지는 이후에 다루겠습니다.
그전에 우선 haddock을 생성해보고 모듈 계층 구조를 살펴보고, 다른 모듈을 둘러보고, 타입의 링크를 따라가고, API 문서의 형태를 상상해보고, 어떻게 개선할 수 있는지 살펴보겠습니다.

## Haddock 마크업

Haddock은 프로젝트를 빌드하고, 내보낸 모듈과 내보낸 정의를 추적하고, 특별한 마크업 형식으로 작성된 소스 코드 주석을 통해 API 문서를 생성합니다.

마크업 형식에 대해 살펴보겠습니다.
몇 가지 중요한 부분을 다루겠지만, Haddock 마크업에 대한 전체 가이드는 [Haddock 문서](https://haskell-haddock.readthedocs.io/en/latest/markup.html)에서 확인할 수 있습니다.

### 정의 문서화하기

모든 haddock 주석은 일반적인 Haskell 주석의 일부로 표현됩니다.
단일 라인 형식 (`--`)과 다중 라인 형식 (`{-` 및 `-}`) 모두 사용할 수 있습니다.
주석 블록과 haddock 마커의 배치는 haddock 문자열이 어떤 하스켈 정의에 연결되는지를 결정합니다.

하스켈 정의에 대한 주석을 작성하려면 _정의 이전_에 `|`로 시작하는 주석 블록을 작성하거나, _정의 이후_에 `^`로 시작하는 주석 블록을 작성하면 됩니다.

예를 들어:

```haskell
-- | `Head`와 `Structure`를 통해
--    HTML 페이지를 생성합니다.
html_
  :: Head -- ^ HTML 파일의 @\<head\>@ 섹션을 나타냅니다.
  -> Structure -- ^ HTML 파일의 @\<body\>@ 섹션을 나타냅니다.
  -> Html
html_ = ...
...
```

또 다른 예제를 살펴보겠습니다:

```haskell
{- | 다음과 같은 단일 마크업 구조를 표현합니다.

- 문단
- 순서 없는 목록
- 코드 블록
-}
data Structure
  = Heading Natural String
  -- ^ A 크기를 가지는 섹션 제목
  | Paragraph String
  -- ^ 문단
  | UnorderedList [String]
  -- ^ 순서 없는 문자열 목록
  | OrderedList [String]
  -- ^ 순서 있는 문자열 목록
  | CodeBlock [String]
  -- ^ 코드 블록
```

```haskell
{- | 마크업을 HTML로 변환하는 모듈입니다.

이 모듈은 우리의 커스텀 마크업 언어로 작성된 문서를 HTML 페이지로 변환합니다.
-}
module HsBlog.Convert where
```

보시다시피, `|`와 `^`를 사용하여 함수, 함수 인자, 타입, 데이터 생성자, 모듈 등을 문서화할 수 있습니다.
이들은 Haddock 주석을 작성하기 위해 기억해야 할 중요한 항목이라고 생각합니다. (사실 `|`만 기억해도 충분합니다)

:::tip
프로젝트에서 내보낸 모듈, 타입 및 최상단 정의에 상세한 설명이나 (최소한) 어떤 용도로 사용되는지 주석으로 작성하세요.

모듈 사용자 및 참여자들이 감사할 것입니다!
:::

### 섹션 제목

제목을 추가하여 모듈을 섹션으로 나눌 수 있습니다.
제목은 (다른 마크업 언어와 유사하게) `*`로 시작하는 주석으로 표현됩니다.

예를 들어:

```haskell
-- * HTML EDSL

html_ :: Head -> Structure -> Html
html_ = ...

-- ** Structure

p_ :: Content -> Structure
p_ = ..

h_ :: Content -> Structure
h_ = ..

...

-- ** Content

txt_ :: String -> Content
txt_ = ...

link_ :: FilePath -> Content -> Content
link_ = ...
```

제목을 내보내기 목록에 추가할 수도 있습니다:

```haskell
module HsBlog.Html
  ( -- * HTML EDSL
    Html
  , html_

    -- ** @\<head\>@ 섹션을 구성하기 위한 조합자
  , Head
  , title_
  , stylesheet_
  , meta_

    -- ** @\<body\>@ 섹션을 구성하기 위한 조합자
  , Structure
  , p_
  , h_
  , ul_
  , ol_
  , code_

    -- ** 구조 안 본문을 구성하기 위한 조합자
  , Content
  , txt_
  , img_
  , link_
  , b_
  , i_

    -- ** HTML을 문자열로 출력
  , render
  )
  where
```

모듈의 구성 요소들을 섹션으로 분리하면 중요한 부분을 모아두고, haddock이 모듈 페이지 상단에 목차를 생성하도록 할 수 있습니다.

제목을 통해 섹션을 분리한 이후, 때때로 하나의 모듈을 여러 모듈로 분리하는 것이 좋을지 여부를 쉽게 파악할 수 있습니다.

---

**연습문제**: 프로젝트의 모듈을 원하는 대로 재배열하고 섹션에 제목을 추가해 보세요.

---

### 서식 지정

앞서 살펴본 것처럼 댓글 본문에 서식을 추가할 수도 있습니다. 예를 들어 다음과 같이 할 수 있습니다:

- `` ` ``로 둘러싸서 하이퍼링크 식별자를 추가합니다.

  예를 들어: `` `Heading` ``

- `@`로 둘러싸서 `고정폭 텍스트`를 추가합니다.

  예를 들어: `@Paragraph "Hello"@`

- `/`로 둘러싸서 *강조 텍스트*를 추가합니다.

  예를 들어: `/this is emphasised/`
 
- `__`로 둘러싸서 **굵은 텍스트**를 추가합니다.

  예를 들어: `__this is bold__`

### 더 나아가

이번 장에서는 haddock 마크업 언어의 기본 사항을 다루었습니다.
더 알고 싶다면 [Haddock 마크업 가이드](https://haskell-haddock.readthedocs.io/en/latest/markup.html)를 참고하세요.
코드 블록, 그리드 테이블, 이미지 및 예제와 같은 더 흥미로운 문서 구조를 생성하는 방법에 대한 정보가 있습니다.

## 요약

지금까지 하스켈 프로그램을 문서화하는 한 가지 방법을 간단히 살펴보았습니다:
haddock 마크업으로 사용해 소스 코드 주석을 작성하고 이를 통해 API 문서를 생성합니다.

API 문서도 매우 유용하지만, 예제와 튜토리얼과 같은 다른 형태의 문서를 통해서도 사용자가 빠르게 시작할 수 있도록 도울 수 있습니다.

---

**연습문제**: 우리의 프로젝의 최상단 정의에 hadddock 주석을 추가해 보세요.
그리고 프로그램과 다양한 부분을 잘 이해하고 있는지 테스트해 보세요.
때로는 무언가를 설명하려고 하면 더 잘 이해하게 됩니다!

---
