# 모듈을 사용하여 잘못된 사용 방지하기

이번 장에서는 HTML 생성 라이브러리를 모듈로 옮겨보겠습니다.

## 모듈

각 하스켈 소스 파일은 모듈입니다. 모듈 이름은 소스 파일 이름과 같아야 하며 대문자로 시작해야 합니다. 
하위 디렉토리도 모듈 이름에 포함되어야 하며 하위 디렉토리를 나타내기 위해 `.`을 사용합니다.
다음 장에서 이를 자세히 살펴보겠습니다.

위 규칙의 유일한 예외는 프로그램의 진입점 모듈입니다.
`main`의 정의가 존재하는 이름이 'Main'인 모듈을 말합니다.
이 모듈의 소스 파일 이름은 원하는 대로 지을 수 있습니다.

모듈 정의는 다음과 같습니다:

```haskell
module <모듈 이름>
  ( <내보내기(export) 목록>
  )
  where
```

모듈 내의 모든 항목을 내보내고 싶다면 내보내기 목록을 생략할 수 있지만 그렇게 하지 않겠습니다.
우리의 작은 라이브러리를 사용하는 방법을 제어하기 위해 원하는 함수와 타입만 내보내겠습니다.

`Html.hs`라는 이름의 새 소스 파일을 만들고 다음과 같이 모듈 선언 코드를 파일 상단에 추가합니다:

```haskell
module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where
```

다음 항목은 내보내지 않습니다:

1. 새로 정의한 타입의 생성자는 제외하고 타입만 내보냅니다.
   생성자도 내보내고 싶다면 `Html(Html)` 또는 `Html(..)`와 같이 작성하면 됩니다.
   이렇게 하면 사용자가 `Structure "Hello"`와 같이 직접 `Structure`를 만들 수 없습니다.

2. 라이브러리가 사용하는 내부 함수, 예를 들어 `el`과 `getStructureString`입니다.

`hello.hs` 파일에서 HTML과 관련된 함수들도 `Html.hs`로 옮깁니다:

```haskell
newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
```

이제 `import`구문을 사용해서 이 모듈을 가져오는 경우 우리가 내보낸 것만 가져올 수 있습니다.

`hello.hs` 파일 상단에 다음 코드를 추가합니다:

```haskell
import Html
```

이제 `hello.hs` 파일은 다음과 같이 변경됩니다:

```haskell
-- hello.hs

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

그리고 `Html.hs` 파일은 다음과 같이 변경됩니다:

```haskell
-- Html.hs

module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
```

> HTML 값을 만드는 함수의 접미사로 밑줄(`_`)을 사용한것을 눈치채신 분도 있을겁니다.
> 이는 미학적인 결정으로, 개인적으로 EDSL을 더 쉽게 인식할 수 있다고 생각했기 때문입니다.
> 또한 `head`와 같은 하스켈 표준 라이브러리에 정의된 함수와의 이름 충돌을 피할 수 있기에 유용합니다.
> 이 아이디어는 `lucid`라는 하스켈 HTML 라이브러리에서 가져왔습니다!
