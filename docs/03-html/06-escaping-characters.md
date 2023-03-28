# 문자 이스케이프 처리하기

이제 `Html`에는 자체 소스 파일과 모듈이 있으며, 내보내기 한 함수들만 사용하여 HTML 코드를 만들 수 있습니다.
또한 우리의 메타 언어와 충돌하는 문자를 포함할 수 있는 사용자 입력을 처리해야 합니다.
그러한 문자 중 하나는 HTML 태그를 만드는 데 사용되는 `<`와 `>`입니다.

이러한 문자를 HTML이 처리할 수 있는 다른 문자열로 변환할 수 있습니다.

다음 [Stack overflow 질문](https://stackoverflow.com/questions/7381974/which-characters-need-to-be-escaped-in-html)에서 처리해야 하는 문자 목록을 확인할 수 있습니다.

`escape` 함수를 만들어봅시다:

```haskell
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

함수 정의에서 몇 가지 새로운 것을 볼 수 있습니다:

1. `Let` 표현식: 이 구문을 사용하여 지역 이름을 정의할 수 있습니다:

   ```haskell
   let
     <이름> = <표현식>
   in
     <표현식>
   ```

   이렇게 하면 `<이름>`을 `in` 다음에 나오는 두 번째 `<표현식>`에서 사용할 수 있습니다.

2. 여러 패턴과 일치하는 패턴 매칭: 다른 문자를 일치시키고 문자열로 변환합니다.
   `_`는 항상 성공하는 "모든 것" 패턴입니다.

3. 새로운 함수: `map`과 `concat`입니다. 이들에 대해 더 자세히 알아보겠습니다.

4. 어떤 이유로 인해 이 코드 블록의 구문 강조가 약간 깨졌습니다. 걱정하지 마십시오.

## 연결 리스트

하스켈에서 연결 리스트(linked lists)는 매우 일반적인 데이터 구조입니다.
그래서 이를 위한 특별한 구문(syntax)이 존재합니다:

1. 리스트의 유형은 대괄호로 표시되며 대괄호 안에는 요소의 타입이 있습니다. 예를 들어:
   - `[Int]` - 정수의 리스트
   - `[Char]` - 문자의 리스트
   - `[String]` - 문자열의 리스트
   - `[[String]]` - 문자열의 리스트의 리스트
   - `[a]` - 임의의 단일 유형의 리스트 (모든 요소는 동일한 타입이어야 함)
2. 빈 리스트는 다음과 같이 작성됩니다: `[]`
3. 리스트에 요소를 추가하는 것은 `:` 연산자를 사용하여 수행합니다 (이를 `cons`라고 함).
   이 연산자는 오른쪽 결합성(right-associative)을 가집니다 (예: `->`).
   예를 들어: `1 : []`, 또는 `1 : 2 : 3 : []`.
4. 위 리스트는 다음과 같이 작성할 수 있습니다: `[1]` 및 `[1, 2, 3]`.

또한, 문자열은 문자의 연결 리스트입니다 - 문자열은 다음과 같이 정의됩니다:
`type String = [Char]`이므로 우리는 문자열을 리스트와 동일한 방식으로 사용할 수 있습니다.

:::note
연결 리스트는 편리한 데이터 구조이지만, 모든 경우에 적합한 것은 아닙니다.
특히 공간 효율성이 떨어지고, 데이터 추가, 임의 접근(random access) 등에 느립니다.
따라서 `String`을 사용하는 것이 효율적이지 않을 수 있습니다.
그래서 외부 패키지에서 제공하는 `Text`라는 다른 문자열 타입을 사용하는 것을 권장합니다.
`Text`와 다른 데이터 구조에 대해서는 이후에 다루겠습니다!
:::

리스트에 대한 연산을 패턴 매칭과 재귀(recursion)를 사용하여 구현할 수 있습니다.
이에 대한 자세한 내용은 ADT를 소개할 때 다루겠습니다.

지금은, [Data.List](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html) 모듈에 있는 다양한 함수를 사용하겠습니다.
그 중에서 [map](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html#v:map)과
[concat](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html#v:concat) 함수를 사용하겠습니다.

### `map`

`map`을 사용하면 리스트의 각 요소에 함수를 적용할 수 있습니다.
이 함수의 타입 시그니처는 다음과 같습니다:

```haskell
map :: (a -> b) -> [a] -> [b]
```

예를 들면:

```haskell
map not [False, True, False] == [True, False, True]
```

또는 이전에 정의한 `escape` 함수처럼, 각 문자열을 이스케이프할 때 사용할 수 있습니다:

```haskell
map escapeChar ['<','h','1','>'] == ["&lt;","h","1","&gt;"]
```

하지만 `escapeChar`의 타입은 `Char -> String`이므로,
`map escapeChar ['<','h','1','>']`의 반환 타입은 `[String]`이자만, 원하는 것은 `String`입니다.

그래서 리스트를 평평하게(flatten) 만들어 줄 `concat` 함수가 필요합니다.

### `concat`

`concat`의 타입은 다음과 같습니다:

```haskell
concat :: [[a]] -> [a]
```

이 함수는 리스트의 리스트를 받아서 리스트를 반환합니다.
예제의 경우, `concat`은 `[String]`을 받아서 `String`을 반환합니다.
`String`은 `[Char]`의 타입 별칭(type alias)이므로, 실제로는 `[[Char]] -> [Char]`입니다.

## GHCi

우리가 작성한 코드를 빠르게 확인하는 방법 중 하나는 **GHCi**라는 대화식 개발 환경(interactive development environment)을 사용하는 것입니다.
`ghci`를 실행하면 대화식 프롬프트가 열리고, 하스켈 표현식을 작성하고 평가할 수 있습니다.
이를 "Read-Evaluate-Print Loop" (REPL)이라고 합니다.

예를 들면:

```
ghci> 1 + 1
2
ghci> putStrLn "Hello, world!"
Hello, world!
```

새로운 이름도 정의할 수 있습니다:

```
ghci> double x = x + x
ghci> double 2
4
```

`:{`와 `:}`로 코드 블록을 감싸면 여러 줄의 코드를 작성할 수 있습니다:

```
ghci> :{
| escape :: String -> String
| escape =
|   let
|     escapeChar c =
|       case c of
|         '<' -> "&lt;"
|         '>' -> "&gt;"
|         '&' -> "&amp;"
|         '"' -> "&quot;"
|         '\'' -> "&#39;"
|         _ -> [c]
|   in
|     concat . map escapeChar
| :}

ghci> escape "<html>"
"&lt;html&gt;"

```

하스켈 소스 파일은 `:load`(또는 `:l`로 줄여서) 명령어로 불러올 수 있습니다:

```
ghci> :load Html.hs
[1 of 1] Compiling Html    ( Html.hs, interpreted )
Ok, one module loaded.
ghci> render (html_ "<title>" (p_ "<body>"))
"<html><head><title>&lt;title&gt;</title></head><body><p>&lt;body&gt;</p></body></html>"
```

라이브러리 모듈도 불러올 수 있습니다:

```
ghci> import Data.Bits
ghci> shiftL 32 1
64
ghci> clearBit 33 0
32
```

표현식의 타입을 알고 싶다면 `:type`(또는 `:t`로 줄여서) 명령어를 사용할 수도 있습니다:

```
λ> :type escape
escape :: String -> String
```

`ghci`를 종료하려면, `:quit`(또는 `:q`로 줄여서) 명령어를 사용합니다:

```
ghci> :quit
Leaving GHCi.
```

GHCi는 빠른 실험과 탐색에 유용한 도구입니다.
위에서 몇 가지 예를 보았습니다 - `escape` 함수에 문자열 `"<html>"`을 전달하면 `"&lt;html&gt;"`라는 문자열을 반환합니다.
이 문자열은 브라우저에서 HTML 태그 대신 `<html>`로 렌더링됩니다.

만약 특정한 함수가 무엇을 하는지 이해하기 어렵다면, GHCi에서 테스트해보세요.
다양한 입력을 넣어보고, 예상한 결과와 일치하는지 확인하세요.
구체적인 예제를 실행하는 것은 코드를 이해하는 데 많은 도움이 될 수 있습니다!

:::tip
GHCi에 대해 더 자세히 알고 싶다면, [GHC 사용자 가이드](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)에서 더 자세한 소개를 찾을 수 있습니다.
:::

## 이스케이프

---

현재 우리가 작성한 라이브러리는 오직 다음 항목만 지원합니다:

1. 페이지 제목
2. 문단
3. 제목

더 진행하기 전에, escape 함수를 적용해 HTML 생성을 안전하게 만들려고 합니다.

escape 함수를 적용해보세요.

<details>
  <summary>정답</summary>

```haskell
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
```

</details>

---

<details>
  <summary><b>수정된 Html.hs</b></summary>

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

`hello.hs` 파일에서 유효하지 않은 HTML을 제공하면 제대로 동작하는지 확인해보세요!

이제 우리는 작은 HTML 라이브러리를 안전하게 사용할 수 있습니다.
그러나 사용자가 예상하지 못한 유효한 사용 사례, 예를 들어 순서 없는 목록을 추가하려는 경우에는 어떻게 해야 할까요?
우리는 라이브러리를 확장하는 것을 완전히 막고 있습니다.
다음에는 이에 대해 이야기 해보겠습니다.
