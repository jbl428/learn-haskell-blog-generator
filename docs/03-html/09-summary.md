# 요약

이번 장에서는 매우 최소한의 HTML EDSL을 만들었습니다.
이후에 이 라이브러리를 사용하여 커스텀 마크업 형식의 텍스트를 HTML로 변환할 것입니다.

이번 장에서 우리는 다음과 같은 것들을 배웠습니다:

- 함수를 정의하고 사용하는 방법
- 타입과 타입 시그니처
- 내장 도메인 특화 언어
- `.` 연산자를 사용하여 함수를 합성
- `newtype`을 사용하여 잘못된 사용을 방지
- 모듈을 정의하는 방법과 `내부` 모듈 패턴
- `newtype`과 모듈을 사용하여 캡슐화하기

지금까지 작성한 라이브러리 코드는 다음과 같습니다:

```haskell title="hello.hs"
import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )
```

```haskell title="Html.hs"
module Html
  ( Html
  , Title
  , Structure
  , html_
  , h1_
  , p_
  , ul_
  , ol_
  , code_
  , append_
  , render
  )
  where

import Html.Internal
```

```haskell title="Html/Internal.hs"
module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar
```

> [저장소 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/2a4691de627bcb280e92f3d02a88d5404179dc86)에서 확인할 수도 있습니다.
