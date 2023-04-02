# 내부 기능 노출하기 (내부 모듈)

지금까지 하스켈로 작지만 편리하고 안전하게 HTML 코드를 만드는 프로그램을 만들었습니다.
이것은 [Hackage](https://hackage.haskell.org/)와 같은 패키지 저장소에 업로드하여 전 세계에 공유할 수 있는 *라이브러리*로 만들 수 있습니다.
라이브러리에 관심이 있는 사용자는 패키지 관리자를 사용하여 프로젝트에 라이브러리를 포함시키고 이를 통해 HTML 페이지를 만들 수 있습니다.

사용자는 우리가 노출한 API를 사용하여 프로젝트를 빌드하며 패키지 관리자는 일반적으로 소스 코드에 대한 접근을 제공하지 않는다는 점을 유의해야 합니다.
예들 들어, 사용자는 (우리가 노출한) `Html` 모듈을 직접 수정할 수 없습니다.

이는 우리의 `Html` EDSL을 안전하게 만들기 위해 **사용자로부터 내부 구현을 숨겼기 때문입니다**.
현재 라이브러리와 상호 작용하는 유일한 방법은 우리가 제공하는 API를 사용하는 것입니다.

이는 우리가 원하는 안전성을 제공하지만, 이 경우에는 사용자가 라이브러리를 확장하는 것을 *차단*합니다.
예를 들어, 아직 구현하지 않은 목록이나 코드 블록과 같은 기능을 *사용자의 프로젝트*에서 확장할 수 없습니다.

사용자가 라이브러리에 문제를 겪을 때 (예를 들어, 누락된 기능) 가장 좋은 방법은 저장소에서 이슈를 열거나 풀 리퀘스트를 만드는 것입니다.
하지만 때로는 사용자가 지금 _당장_ 기능이 필요할 수 있습니다.

우리는 완벽하지 않다는 것을 인정합니다. 라이브러리를 위한 모든 사용 사례를 생각해내는 것이 불가능하기 때문입니다.
우리가 라이브러리에 적용한 제약사항이 강력해, 프로그램 내부 동작을 잘 알고 있고 특정 기능이 필요한 고급 사용자들의 사용성 또한 제한합니다.

### 내부 모듈

이를 위해 내부 모듈을 노출하여 고급 사용자에게 일부 유연성을 제공할 수 있습니다.
내부 모듈은 언어 개념이 아니라 하스켈에서 흔히 볼 수 있는 디자인 패턴(또는 관용구)입니다.

내부 모듈은 `<something>.Internal`과 같이 이름이 지정된 모듈로, 해당 모듈의 모든 기능과 구현 세부 사항을 노출합니다.

예를 들어, 내부 구현을 `Html` 모듈에 작성하는 대신 모든 항목을 내보내는 `Html.Internal` 모듈에 작성합니다.
그리고 해당 모듈을 `Html` 모듈에 가져와서 (이전처럼) 우리가 원하는 API만 명시적으로 내보냅니다.

`Internal` 모듈은 보통 불안정하고 사용하기 위험하다고 알려져 있습니다.
만약 외부 하스켈 라이브러리를 사용할 때 `Internal` 모듈을 사용하게 되면, 모든 문제를 해결하고 나서 라이브러리 저장소에 이슈를 열어주세요!

### 수정하기

이제 `Html` 이라는 디렉터리를 만들고 그 안에 `Internal.hs`라는 파일을 만듭니다.
이 모듈의 이름은 `Html.Internal`이어야 합니다.

이 모듈은 이전에 `Html` 모듈에 있었던 모든 코드를 포함하지만, **모듈 선언문을 수정해 내보내기 목록을 *생략*할 것입니다**:

```haskell title="Html/Internal.hs"
module Html.Internal where

...
```

이제 `Html.hs` 파일에서 `Html/Internal.hs`로 옮긴 코드를 제거하고 대신 내부 모듈을 가져옵니다:

```haskell title="Html.hs"
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

import Html.Internal
```

이제 사용자는 `Html` 모듈을 가져와 안전하게 라이브러리를 사용할 수 있습니다.
만약 사용자가 라이브러리를 통해 순서없는 목록(ul, ui 태그)기능을 구현하고 싶다면, `Html.Internal` 모듈을 사용할 수 있습니다.

<details>
  <summary><b>수정된 Html.hs 과 Html/Internal.hs</b></summary>

```haskell title="Html.hs"
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

</details>

## 요약

사실 우리 프로젝트 내에서만 사용한다면, `Internal` 모듈은 필요하지 않습니다.
프로그램과 HTML EDSL의 소스 코드는 동일한 프로젝트에 있으며, `Html` 모듈에 직접 접근 할 수 있기 때문입니다.
우리는 원할 때 언제든지 모듈을 수정 할 수 있습니다.

하지만 만약 다른 개발자가 사용하기 위해 HTML EDSL을 라이브러리로 출시하려고 한다면,
사용자가 내부 구현에 접근할 수 있도록 `Internal` 모듈을 노출하는 것이 좋습니다.
이렇게 하면 사용자가 라이브러리를 사용하는 데 어려움을 겪지 않을 수 있습니다!

소스 코드로부터 패키지를 만드는 방법에 대해서는 이후에 다룰 예정입니다.
