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

## Parsing with rich context

Previously we wrote a parser that separates documents into different paragraphs.
With new features under our belt we can now remember the exact context we are in
(whether it is a text paragraph, a list, or a code block) and act accordingly!

Let's look again at the parsing code we wrote previously:

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

Previously our context, `currentParagraph`, was used to group adjacent lines in an accumulative list.

Next, instead of using a `[String]` type to denote adjacent lines, we can instead use a `Structure` to denote the context.

One issue we might have though with representing context with the `Structure` type,
is that when we start parsing we don't have any context.
But we have learned of a way to represent the absence of a value with `Maybe`! So our new context type can be `Maybe Structure` instead.

Let's rewrite our code above with our new context type:

```haskell
parse :: String -> Document
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context -- (2)
    -- Paragraph case
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

1. We can now pass `Nothing` when we don't have a context
2. Unsure what `maybeToList` does? [Hoogle](https://hoogle.haskell.org) it!
3. We can split this line into two important parts:

   1. `maybe id (:) context` - prepending the context to the rest of the document
   2. `parseLines Nothing rest` - parsing the rest of the document

   Let's focus on the first part.
   We want to prepend `context` to the rest of the document, but we can't write
   `context : parseLines Nothing rest` because `context` has the type `Maybe Structure`
   and not `Structure`, meaning that we _might_ have a `Structure` but maybe not.
   If we do have a `Structure` to prepend, we wish to prepend it. If not, we want to return
   the result of `parseLines Nothing rest` as is. Try writing this using pattern matching!

   <details><summary>Solution</summary>

   ```haskell
   case context of
     Nothing -> parseLines Nothing rest
     Just structure -> structure : parseLines Nothing rest
   ```

   </details>

   The [maybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:maybe)
   function let's us do the same thing in a more compact way. It is a function
   that works similarly to pattern matching on a `Maybe`:
   the third argument to `maybe` is the value on which we pattern match,
   the second argument is a function to apply to the value found in a `Just` case,
   and the first argument is the value to return in case the value
   we pattern match on is `Nothing`. A more faithful translation of
   `maybe id (:) context (parseLines Nothing rest)`
   to pattern matching would look like this:

   <details><summary>Solution</summary>

   ```haskell
   ( case context of
       Nothing -> id
       Just structure -> (:) structure
   ) (parseLines Nothing rest)
   ```

   Note how the result of this case expression is a function of type `Document -> Document`,
   how we partially apply `(:)` with `structure` to create a function that prepends `structure`,
   and how we apply `parseLines Nothing rest` to the case expression.

   </details>

   This way of encoding pattern matching using functions is fairly common.

   Check out the types of `id`, `(:)` and `maybe id (:)` in GHCi!

4. Hey! Didn't we say that appending `String`s/lists is slow (which is what `unwords` does)? Yes, it is.
   Because in our `Structure` data type, a paragraph is defined as `Paragraph String` and not `Paragraph [String]`,
   we can't use our trick of building a list of lines and then reverse it in the end.

   So what do we do?
   There are many ways to handle that, one simple way is to create a different type with the right shape:

   ```haskell
   data Context
     = CtxHeading Natural String
     | CtxParagraph [String]
     | CtxUnorderedList [String]
     | CtxOrderedList [String]
     | CtxCodeBlock [String]
   ```

   Since creating new types in Haskell is cheap, this is a very viable solution.

   In this case I'm going with the approach of not worrying about it too much,
   because it's a very local piece of code that can easily be fixed later if needed.

Let's cover more parsing cases, we want to handle headings and lists as well.
We can do that by examining the first characters of a line:

```haskell
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

---

Exercise: Add the `CodeBlock` and `OrderedList` cases.

<details>
  <summary>Final module</summary>

```haskell
-- Markup.hs

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

### How do we know our parser works correctly?

In an earlier chapter, we parsed a few examples of our markup language [by hand](./01-data-type.md#exercises).
Now, we can try to test our parser by comparing our solutions to our parser.
By deriving `Eq` for our `Structure` data type
(marked with (1) in "final module" above),
we can compare solutions with the `==` (equals) operator.

Try it in GHCi! You can read a text file in GHCi using the following syntax:

```haskell
ghci> txt <- readFile "/tmp/sample.txt"
```

And then compare with the hand written example values from the solutions
(after adding them to the module and loading them in GHCi):

```haskell
ghci> parse txt == example4
```

In a later chapter, we'll write automated tests for our parser using a testing framework.
But before that, I'd like to glue things together
so we'll be able to:

1. Read markup text from a file
2. Parse the text
3. Convert the result to our HTML EDSL
4. Generate HTML code

And also discuss how to work with IO in Haskell while we're at it.

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1).
