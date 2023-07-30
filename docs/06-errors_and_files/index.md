# 에러 처리와 여러 파일 다루기

이전 장에서 구현하지 않은 함수가 하나 남아 있습니다.
그리고 우리의 프로그램을 정적 블로그 생성기라고 부르기 위해서는 아직 해야 할 일이 몇 가지 더 있습니다.
디렉터리 안 여러 파일을 처리하고 다른 페이지로의 링크가 있는 인덱스 랜딩 페이지를 만들어야 합니다.

## HTML 링크

우리의 HTML EDSL은 아직 링크나 굵은 글씨, 이탤릭체 등의 기능을 지원하지 않습니다.
이 기능들을 추가해서 인덱스를 만들 때 사용할 수 있도록 해야 합니다.

지금까지는 `String`을 `Structure`에 전달해서 `p_`나 `h_`와 같은 함수를 만들었습니다.
대신에 텍스트, 링크, 이미지 등을 의미하는 새로운 타입인 `Content`를 만들어서 이 타입을 전달하면 됩니다.

---

**연습문제**: 방금 언급한 기능들을 추가해 보세요. 컴파일러 에러를 해결하고 필요하다면 리팩토링을 진행하세요.

<details><summary>정답</summary>

<details><summary>src/Html/Internal.hs</summary>

```haskell
module HsBlog.Html.Internal where

import Numeric.Natural

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

newtype Content
  = Content String

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

-- * Structure

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString structure =
  case structure of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content str -> str

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

</details>

<details><summary>src/Html.hs</summary>

```haskell
module HsBlog.Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h_
  , ul_
  , ol_
  , code_
  , Content
  , txt_
  , img_
  , link_
  , b_
  , i_
  , render
  )
  where

import HsBlog.Html.Internal
```

</details>

<details><summary>src/Convert.hs</summary>

```haskell
module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

</details>

</details>

---

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/110a19029f0be42eb2ac656f5d38356dbf9c5746)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/110a19029f0be42eb2ac656f5d38356dbf9c5746) 를 확인할 수 있습니다.

## 인덱스 페이지 만들기

이제 확장된 HTML EDSL을 사용해, 다른 페이지로의 링크가 있는 인덱스 페이지를 만들어 봅시다.

인덱스 페이지를 만들기 위해 *타겟 경로*와 `마크업`(첫 제목과 문단을 가져와 인덱스 페이지에 사용하기 위한)을 가진 파일 목록이 필요합니다.
출력은 `Html` 페이지여야 합니다.

---

다음과 같은 함수를 구현해보세요:

```haskell
buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
```

<details><summary>정답</summary>

```haskell
buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 3 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Blog"
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )
```

</details>

---

## 디렉터리 처리하기

디렉터리를 처리하는 일반적인 전략은 다음과 같습니다:

- 출력 디렉터리를 만듭니다
- 디렉터리의 모든 파일 이름을 가져옵니다
- 확장자에 따라 필터링합니다, `txt` 파일을 처리하고 다른 파일은 수정 없이 복사합니다
- 각 텍스트 파일을 파싱하고, 인덱스를 만들고, 파일을 HTML로 변환하고, 모든 것을 출력 디렉터리에 씁니다

파싱 함수는 실패할 가능성이 거의 없으나 파일 시스템에서 파일을 읽거나 쓰는 것은 다양한 원인으로 인해 실패할 수 있습니다.
정적 블로그 생성기는 하나의 파일이 문제가 있어도 전체 과정이 실패하지는 않도록 동작하려고 합니다.
이는 하스켈에서 에러 처리에 대해 배우기 좋은 기회입니다.
부수 효과가 없는 코드와 I/O 코드에 대한 에러 처리 모두에 대해 배워보겠습니다.

다음 몇 장에 걸쳐 하스켈레어 에러 처리에 대한 전반적인 내용을 살펴보고, 우리의 경우에 적합한 접근 방식을 찾아보겠습니다.
