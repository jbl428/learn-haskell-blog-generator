# 마크업 파싱하기 02 (패턴 매칭)

## Maybe

이전에 부분 함수를 만들지 않는 방법 중 하나로, `Maybe`를 사용하여 결과가 없는 경우를 표현하는 것을 살펴 보았습니다.

```haskell
data Maybe a
  = Nothing
  | Just a
```

`Maybe`는 표준 라이브러리([base](https://hackage.haskell.org/package/base))에서 제공하는 데이터 타입으로,
값의 부재를 의미하는 추가적인 값을 타입에 추가하는 데 사용됩니다.
예를 들어, `Just` 생성자는 일반적인 불리언 값이 있음을 나타내고, (`Just True`와 `Just False`)
`Nothing` 생성자는 불리언 값이 없음을 나타냅니다.

이를 통해 주어진 리스트의 첫 번째 요소를 반환하는 `head` 함수를 부분 함수로 만들지 않고 항상 값을 반환하게 만들 수 있습니다.

```haskell
safeHead :: [a] -> Maybe a
```

위 방식은 리스트가 비어있다면, `Nothing`을 반환하고, 비어있지 않다면 `Just <첫 번째 요소>`를 반환합니다.
[Data.Maybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Maybe.html) 모듈의
[listToMaybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Maybe.html#v:listToMaybe) 함수가 이 역할을 합니다.

`Maybe <무언가>` 또는 다른 `data`로 생성된 타입의 값을 *꺼내기*위해, 패턴 매칭을 사용할 수 있습니다.

## 패턴 매칭

이전에 패턴 매칭에 대해 이미 몇 번 보았습니다.
패턴 매칭은 하스켈의 매우 유용한 기능으로, 주로 두 가지 주요 목적으로 사용합니다:

1. 복잡한 값을 분해
2. 흐름 제어

이전 [newtype](../03-html/04-safer-construction.md#using-newtypes)을 소개할 때,
**case 표현식**과 **함수 정의**를 사용하여 `newtype`을 분해하는 방법을 살펴보았습니다.
`data` 타입에 대해서도 같은 방법을 적용할 수 있습니다:

```haskell
-- | 색상을 표현하는 데이터 타입
data Color
  = RGB Word8 Word8 Word8

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue
```

`getBluePart` 함수는 주어진 합성 값을 분해하여 RGB에서 세 번째 요소인 파란색 값을 추출합니다.

`blue`는 `color`의 세 번째 요소에 준 이름으로 오른쪽 화살표 다음에 오는 패턴에 바인딩됩니다.
이는 함수 인수와 유사합니다.
또한 `_`는 이름을 바인딩하지 않고 모든 값과 일치하는 패턴입니다.

값을 두 개 이상의 패턴과 비교할 수도 있습니다:

```haskell
data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Dark Red ->
      RGB 170 0 0
    AnsiColor Bright Red ->
      RGB 255 85 85
    -- and so on
```

다음 항목을 주목하세요:

1. 패턴은 중첩될 수 있습니다. `ansicolor`를 여러 단계로 분해하는 것을 알 수 있습니다.
2. 패턴은 위에서 아래로 매칭되므로, 패턴이 중첩되면 위에 있는 패턴이 우선합니다.
3. 어떤 값이 주어진 모든 패턴과 일치하지 않으면 런타임에 에러가 발생합니다.

GHC에게 우리가 실수로 패턴을 중복해서 작성했거나, 모든 가능한 값을 매칭할 수 있도록 작성하지 않았다는 것을 알려주도록 할 수 있습니다.
`ghc` 또는 `runghc`에 `-Wall` 플래그를 전달하면 됩니다.

**항상 `-Wall`을 사용하는 것을 권장합니다**!

:::note
함수를 여러번 정의하는 방식을 통해, 패턴 매칭을 함수 정의에도 사용할 수도 있습니다.
하지만 [개인적으로 그 기능을 썩 좋아하지 않습니다](https://twitter.com/_gilmi/status/1257225601079029760)
가능하면 case 표현식을 사용하는 것을 권장합니다.
하지만 원한다면 case 표현식 대신 사용해도 좋습니다.
:::

### 연결 리스트 패턴 매칭

연결 리스트에는 [특별한 문법](../03-html/06-escaping-characters.md#linked-lists-briefly)이 있는데, 패턴 매칭에도 특별한 문법이 있습니다.
리스트를 만들 때 사용한 특별한 문법을 통해, 리스트의 *요소*를 패턴으로 사용할 수 있습니다.
예를 들어:

```haskell
safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    -- 빈 리스트
    [] -> Nothing

    -- cons 셀 패턴, 리스트의 첫 번째 요소를 x에 매칭
	x : _ -> Just x
```

```haskell
exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
  case list of
    -- 정확히 두 개의 요소를 가진 리스트와 매칭
	[x, y] -> Just (x, y)

    -- 나머지 모든 패턴과 매칭
	_ -> Nothing
```

```haskell
-- 다음 함수도 같은 결과를 반환합니다
exactlyTwoVersion2 :: [a] -> Maybe (a, a)
exactlyTwoVersion2 list =
  case list of
    -- 정확히 두 개의 요소를 가진 리스트와 매칭
	x : y : [] -> Just (x, y)

    -- 나머지 모든 패턴과 매칭
	_ -> Nothing
```

---

연습문제:

1. 주어진 색이 밝은 색인지를 확인하는 `isBright :: AnsiColor -> Bool` 함수를 작성하세요.
2. [이 표](https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit)를 사용하여 `ansiToUbuntu` 함수를 작성하세요.
3. `listToMaybe`를 사용해 리스트가 비어있는지 확인하는 `isEmpty :: [a] -> Bool` 함수를 작성하세요.
4. `listToMaybe`를 _사용하지 않고_ 리스트가 비어있는지 확인하는 `isEmpty :: [a] -> Bool` 함수를 작성하세요.

정답:

<details><summary>연습문제 (1)</summary>

```haskell
isBright :: AnsiColor -> Bool
isBright ansiColor =
  case ansiColor of
    AnsiColor Bright _ -> True
    AnsiColor Dark _ -> False
```

</details>
<details><summary>연습문제 (2)</summary>

```haskell
ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> RGB 0 0 0
            Red -> RGB 194 54 33
            Green -> RGB 37 188 36
            Yellow -> RGB 173 173 39
            Blue -> RGB 73 46 225
            Magenta -> RGB 211 56 211
            Cyan -> RGB 51 187 200
            White -> RGB 203 204 205

        Bright ->
          case color of
            Black -> RGB 129 131 131
            Red -> RGB 252 57 31
            Green -> RGB 49 231 34
            Yellow -> RGB 234 236 35
            Blue -> RGB 88 51 255
            Magenta -> RGB 249 53 248
            Cyan -> RGB 20 240 240
            White -> RGB 233 235 235
```

위 코드처럼 패턴 매칭은 한 없이 깊어질 수 있기에, 하나의 `case` 표현식을 사용해 모든 경우를 매칭할 수도 있습니다.

```haskell
ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor Dark Black -> RGB 0 0 0
    AnsiColor Dark Red -> RGB 194 54 33
    AnsiColor Dark Green -> RGB 37 188 36
    AnsiColor Dark Yellow -> RGB 173 173 39
    AnsiColor Dark Blue -> RGB 73 46 225
    AnsiColor Dark Magenta -> RGB 211 56 211
    AnsiColor Dark Cyan -> RGB 51 187 200
    AnsiColor Dark White -> RGB 203 204 205
    AnsiColor Bright Black -> RGB 129 131 131
    AnsiColor Bright Red -> RGB 252 57 31
    AnsiColor Bright Green -> RGB 49 231 34
    AnsiColor Bright Yellow -> RGB 234 236 35
    AnsiColor Bright Blue -> RGB 88 51 255
    AnsiColor Bright Magenta -> RGB 249 53 248
    AnsiColor Bright Cyan -> RGB 20 240 240
    AnsiColor Bright White -> RGB 233 235 235
```

하지만 이 방식은 `AnsiColor`, `Dark` 그리고 `Bright`가 많이 반복되는 단점이 있습니다.

</details>
<details><summary>연습문제 (3)</summary>

```haskell
isEmpty :: [a] -> Bool
isEmpty list =
  case listToMaybe list of
    Nothing -> True
    Just _ -> False
```

</details>
<details><summary>연습문제 (4)</summary>

```haskell
isEmpty :: [a] -> Bool
isEmpty list =
  case list of
    [] -> True
    _ : _ -> False
```

</details>

---

## 풍부한 문맥을 통해 파싱하기

이전에는 문서를 여러 문단으로 분리하는 파서를 작성했습니다.
새로운 기능을 추가하여 이제는 우리가 어떤 문맥(context)에 있는지(텍스트 단락, 목록, 또는 코드 블록) 정확히 기억하고 그에 따라 작동할 수 있습니다!

이전에 작성한 파싱 코드를 다시 살펴보겠습니다:

```haskell
parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest
          else
            parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words
```

위 코드에서 `currentParagraph`이 문맥을 의미하며, 인접한 줄들을 그룹으로 묶는 역할을 합니다.

이번에는 인접한 줄을 `[String]`이 아닌 문맥을 나타내는 `Structure` 타입으로 표현해보겠습니다.

하지만 `Structure` 타입으로 문맥을 표현할 때의 문제점 중 하나는, 파싱을 시작할 때는 어떤 문맥도 가지고 있지 않다는 것입니다.
그러나 `Maybe`를 사용하여 값이 없음을 나타내는 방법을 배웠습니다!
그래서 우리의 새로운 문맥 유형은 `Maybe Structure` 이 될 수 있습니다.

위 코드를 새로운 문맥 타입으로 수정해보겠습니다:

```haskell
parse :: String -> Document
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context -- (2)
    -- 문단인 경우
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest) -- (3)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest -- (4)
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```

1. 아직 문맥이 없으므로 `Nothing`을 사용합니다.
2. `maybeToList`가 무엇인지 모르겠다면 [Hoogle](https://hoogle.haskell.org)을 사용해보세요!
3. 이 줄을 두 가지 중요한 부분으로 나눌 수 있습니다:
 
   1. `maybe id (:) context` - 문맥을 문서의 나머지 부분에 앞에 붙입니다.
   2. `parseLines Nothing rest` - 문서의 나머지 부분을 파싱합니다.

   먼저 첫 번째 부분을 살펴보겠습니다.
   우리는 `context`를 문서의 나머지 요소 앞에 붙이고 싶지만, `context`가 `Maybe Structure` 타입을 가지고 있기 때문에 `context : parseLines Nothing rest`와 같이 작성할 수 없습니다.
   또한 `Structure` 타입이 아니기 때문에 `context`가 `Structure`를 가지고 있을 수도 있고, 그렇지 않을 수도 있습니다.
   만약 `Structure`를 가지고 있다면 그것을 앞에 붙여야 하고, 그렇지 않다면 `parseLines Nothing rest`의 결과를 그대로 반환해야 합니다.
   이를 패턴 매칭을 사용해 작성해보세요!

   <details><summary>정답</summary>

   ```haskell
   case context of
     Nothing -> parseLines Nothing rest
     Just structure -> structure : parseLines Nothing rest
   ```

   </details>

   [maybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:maybe) 함수를 사용하면 위 작업을 더 간결하게 할 수 있습니다.
   이 함수는 `Maybe`에 대해 패턴 매칭을 하는 것과 비슷하게 작동합니다:
   `maybe`의 세 번째 인자는 패턴 매칭할 값이고,
   두 번째 인자는 `Just`인 경우에 적용할 함수이며,
   첫 번째 인자는 패턴 매칭한 결과가 `Nothing`인 경우 반환할 값입니다.
   `maybe id (:) context (parseLines Nothing rest)`를 패턴 매칭을 사용한 코드로 바꿔보세요!

   <details><summary>정답</summary>

   ```haskell
   ( case context of
       Nothing -> id
       Just structure -> (:) structure
   ) (parseLines Nothing rest)
   ```

   case 표현식의 결과가 타입이 `Document -> Document`인 함수라는 것에 주목하세요.
   `(:)`에 `structure`을 부분적으로 적용하여 `structure`을 앞에 붙이는 함수를 만들고,
   `parseLines Nothing rest`를 case 표현식에 적용하는 방식을 살펴보세요.

   </details>

   함수를 사용하여 패턴 매칭을 인코딩하는 이러한 방식은 자주 사용됩니다.

   `id`, `(:)` 그리고 `maybe id (:)`의 타입을 GHCi에서 확인해보세요!

4. 앗! 전에 (`unwords` 함수가 하는) `String`이나 리스트 뒤에 요소를 추가하는 것은 느리다고 하지 않았나요?
   맞습니다! 하지만 우리의 `Structure` 타입에서는 문단이 `Paragraph String`으로 정의되어 있고, 
   `Paragraph [String]`이 아니기 때문에 리스트를 뒤집는 방법을 사용할 수 없습니다.

   그럼 어떻게 해야 할까요?
   이를 처리하는 방법에는 여러 가지가 있지만, 한 가지 간단한 방법은 올바른 모양으로 다른 타입을 만드는 것입니다:

   ```haskell
   data Context
     = CtxHeading Natural String
     | CtxParagraph [String]
     | CtxUnorderedList [String]
     | CtxOrderedList [String]
     | CtxCodeBlock [String]
   ```

   하스켈에서 새로운 타입을 만드는 것은 비용이 저렴하기에, 이 방법은 매우 유용합니다.

   하지만 이번에는 위와 같은 방법을 사용하지 않겠습니다.
   왜냐하면 나중에 필요한 경우 쉽게 수정할 수 있는 로컬 코드 조각이기 때문입니다.

다음 파싱 단계로 넘어가 이번에는 제목과 리스트를 처리해보겠습니다.
줄의 첫 번째 문자를 검사하여 이를 처리할 수 있습니다:

```haskell
parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- 종료 조건
    [] -> maybeToList context

    -- 제목 1 인 경우
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- 순서 없는 목록인 경우
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- 문단인 경우
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```

---

연습문제: `코드 블록`과 `순서 있는 목록`의 경우도 처리해보세요!

<details>
  <summary>정답</summary>

```haskell title="Markup.hs"
module Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe (maybeToList)

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)    -- (1)


parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```

</details>

---

### 우리의 파서가 제대로 동작하는지 어떻게 알 수 있을까요?

이전 장에서, 우리는 몇 가지 마크업 언어 예제를 직접 파싱해봤습니다. ([관련 연습문제](./01-data-type.md#exercises))
`Structure` 데이터 타입의 `Eq` 인스턴스를 만들었으므로 (위 정답에서 (1) 표시된 부분)
이제 `==` 연산자를 사용하여 두 결과가 같은지 비교할 수 있습니다.

GHCi를 사용해 확인해보세요! 다음과 같은 문법을 사용하여 텍스트 파일을 읽을 수 있습니다:

```haskell
ghci> txt <- readFile "/tmp/sample.txt"
```

그리고 이전 손으로 작성한 답안과 이번에 작성한 정답과 비교해보세요
(모듈에 추가하고 GHCi에서 불러온 후 수행하세요):

```haskell
ghci> parse txt == example4
```

이후 장에서, 테스트 프레임워크를 사용하여 자동화된 테스트를 작성할 것입니다.
하지만 그 전에, 다음과 같은 작업들을 하나로 묶는 작업을 진행하고자 합니다.

1. 파일에서 마크업 텍스트 읽기
2. 텍스트 파싱하기
3. HTML EDSL로 변환하기
4. HTML 코드 생성하기

또한 하스켈에서 IO 를 어떻게 다루는지도 함께 살펴보겠습니다.

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1) 를 확인할 수 있습니다.
