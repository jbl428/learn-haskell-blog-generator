# 마크업을 HTML로 변환하기

통합 작업을 수행하기 전에 한 가지 중요한 작업이 남아 있습니다.
바로 `Markup` 데이터 타입을 `Html` 데이터 타입으로 변환하는 것입니다.

먼저 새로운 모듈을 만들고 `Markup`과 `Html` 모듈을 가져옵니다.

```haskell
module Convert where

import qualified Markup
import qualified Html
```

## 한정된 가져오기 (Qualified Imports)

이번에는 모듈을 가져올 때 한정된 가져오기(qualified imports)를 사용했습니다.
한정된 가져오기는 가져온 모듈의 이름을 일반 모듈 이름 공간에 노출하지 않으며,
모듈을 사용하려면 모듈 이름으로 접두사를 붙여야 합니다.

예를 들어 `parse`를 사용하려면 `Markup.parse`로 작성해야 합니다.
만약 `Html.Internal`을 한정된 가져오기로 가져왔다면 `Html.Internal.el`로 작성해야 합니다.
이는 약간 길어 보입니다.

`as` 키워드를 사용하여 모듈에 새 이름을 지정할 수도 있습니다:

```haskell
import qualified Html.Internal as HI
```

그러면 `HI.el`로 작성할 수 있습니다.

개인적으로 한정된 가져오기를 사용하는 것을 선호합니다.
왜냐하면 코드를 읽는 사람이 어디에서 가져온 이름인지 추측할 필요가 없기 때문입니다.
또한 한정된 가져오기를 사용하도록 설계된 모듈들도 있습니다.
예를 들어 `map`, `set`, `vector`와 같은 컨테이너 타입의 API는 매우 유사합니다.
만약 하나의 모듈에서 여러 컨테이너를 사용하려면 `singleton`과 같은 함수를 작성할 때,
한정된 가져오기를 사용해야 GHC가 어떤 `singleton` 함수를 참조하는지 알 수 있습니다.

일부 사람들은 한정된 가져오기 대신 가져오기 목록(import list)을 선호합니다.
한정된 이름들이 약간 지저분하고 장황하기 때문입니다.
저는 종종 가져오기 목록 대신 한정된 가져오기를 선호하지만,
두 가지 방법 모두 사용해 보고 어떤 것이 더 좋은지 확인해 보세요.
가져오기에 대한 더 많은 정보는 [위키 문서](https://wiki.haskell.org/Import)를 참고하세요.

## `Markup.Structure`를 `Html.Structure`로 변환하기

마크업 구조를 HTML 구조로 변환하는 과정은 직관적입니다.
마크업 구조를 패턴 매칭하고 연관된 HTML API를 사용하면 됩니다.

```haskell
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading 1 txt ->
      Html.h1_ txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

위 코드를 `-Wall` 플래그와 함께 컴파일하면 패턴 매칭이 완전하지 않다(non-exhaustive)는 경고가 나옵니다.
이는 현재 `h1`이 아닌 제목을 처리할 방법이 없기 때문입니다.
이 문제를 해결하는 몇 가지 방법이 있습니다:

- 경고 무시하기- 이 방법은 언젠가 런타임에서 문제가 발생하고 사용자가 슬퍼할 것입니다.
- 다른 경우를 패턴 매칭하고 `error` 함수를 사용하여 사용자에게 적절한 오류 메시지를 표시합니다. - 위와 동일한 단점이 있지만, 컴파일 시 경고가 발생하지 않습니다.
- 패턴을 매칭하고 잘못된 작업을 수행합니다. - 사용자는 여전히 슬퍼할 것입니다.
- `Either`를 사용하여 타입 시스템에 오류를 인코딩합니다. 이후 장에서 이 방법을 살펴보겠습니다.
- 입력을 제한하기 - `Markup.Heading`을 숫자 대신 지원하는 특정 제목으로 변경합니다. 이것은 합리적인 접근 방식입니다.
- 임의의 제목을 지원하는 함수를 구현하기 - 간단하게 구현할 수 있습니다.

---

연습문제: 임의의 제목(예: `<h1>`, `<h2>` 등)을 지원하는 함수 `h_ :: Natural -> String -> Structure`를 구현하세요.

<details><summary>정답</summary>

```haskell
import Numeric.Natural

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape
```

`Html.hs`에서 함수를 내보내는 것을 잊지 마세요!

</details>

연습문제: `convertStructure`를 `h_`를 사용하도록 수정하세요.

<details><summary>정답</summary>

```haskell
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

</details>

---

## Document -> Html

`Html` 문서를 생성하기 위해서는 `html_` 함수를 사용해야 합니다.
이 함수는 `Title`과 `Structure` 두 인자를 받습니다.

제목의 경우, 단순하게 파일의 이름을 사용할 수 있습니다.

마크업 `Document`(마크업 `Structure`의 리스트)를 HTML `Structure`로 변환하려면 각 마크업 `Structure`를 변환하고 이를 하나로 합쳐야 합니다.

각 마크업 `Structure`를 변환하는 함수 `convertStructure`를 이미 구현했기에, `map`을 활용하여 다음과 같은 함수를 얻을 수 있습니다.

```
map convertStructure :: Markup.Document -> [Html.Structure]
```

`Html.Structure`의 리스트를 하나로 합치기 위해 재귀 함수를 사용할 수 있습니다.
하지만 리스트가 비어있는 경우를 처리하는 기본 사례에서 문제가 발생합니다.
이를 어떻게 처리할 수 있을까요?

단순히 빈 HTML 구조를 표현하는 더미 `Html.Structure`를 만들어서 사용할 수 있습니다.

`Html.Internal` 모듈에 추가해 보겠습니다:

```haskell
empty_ :: Structure
empty_ = Structure ""
```

---

이를 활용해 재귀 함수를 작성해 보세요!

<details><summary>정답</summary>

```haskell
concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure xs
```

</details>

---

이전에 `<>` 함수를 `Semigroup`의 인스턴스로 구현한 것을 기억하시나요?
`Semigroup`는 `(<>) :: a -> a -> a`를 구현하는 것을 추상화한 것이었고,
`<>`는 결합법칙을 만족해야 했습니다(`a <> (b <> c) = (a <> b) <> c`).

`Semigroup` 인스턴스가 있고 또한 "빈" 값을 가지는 값도 가지는 상황이 자주 발생합니다.
예를 들어 문자열은 결합할 수 있고, 빈 문자열은 "빈" 값을 나타냅니다.
이는 실제로 **monoid**라는 **추상화**로 알려진 패턴입니다.

## Monoids

사실 "빈 값"이라는 의미는 모든 것을 잘 설명하지 못하며 추상화에 그렇게 유용하지도 않습니다.
대신 "항등원(identity element)"이라는 설명을 사용할 수 있으며 이는 다음 법칙을 만족해야 합니다:

- `x <> <identity> = x`
- `<identity> <> x = x`

다르게 말하면, 이 "빈" 값을 이용하여 `<>`에 다른 하나의 인자를 전달하면 다른 인자를 그대로 얻을 수 있습니다.

`String`의 경우, 빈 문자열 `""`이 이를 만족합니다:

```haskell
"" <> "world" = "world"
"hello" <> "" = "hello"
```

"world"와 "hello" 뿐만 아니라 어떤 문자열에 대해서도 이 법칙이 성립합니다.

잠시 하스켈 세계에서 벗어나 생각해봅시다.
정수집합과 `+` 연산의 경우, `+`를 결합법칙을 만족하는 이항 연산자로(`<>` 대신), `0`을 항등원으로 보면 monoid를 만족하는 것을 알 수 있습니다:

```haskell
17 + 0 = 17
0 + 99 = 99
```

따라서 정수집합과 `+` 연산 semigroup을 만족하며 `0`과 함께 monoid를 만족합니다.

이를 통해 다음과 같은 사실을 알 수 있습니다:

1. monoid는 semigroup보다 더 구체적인 추상화입니다. semigroup에 항등원이라는 새로운 조건을 추가했습니다.
2. 이 추상화는 유용할 수 있습니다! `concatStructure`라는 일반적인 함수를 작성할 수 있습니다. 이 함수는 monoid에 대해서만 작동합니다.

실제로, `base` 패키지에는 `Monoid`라는 타입 클래스가 있으며 `Semigroup`를 **슈퍼 클래스(super class)**로 가집니다.

```haskell
class Semigroup a => Monoid a where
  mempty :: a
```

:::note
사실 위 정의는 간략하게 표현한 버전입니다.
[실제 정의](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#t:Monoid)는 하위 호환성 및 성능을 고려하기에 조금 더 복잡합니다.
하스켈에서 `Semigroup`은 `Monoid`가 나온 이후에 도입되었습니다!
:::

이제 HTML `Structure` 데이터 타입에 `Monoid` 인스턴스를 추가할 수 있습니다:

```haskell
instance Monoid Structure where
  mempty = empty_
```

이제 `concatStructure` 함수대신 다음과 같은 라이브러리 함수를 사용할 수 있습니다:

```haskell
mconcat :: Monoid a => [a] -> a
```

이 함수는 이론적으로 다음과 같이 구현되어 있습니다:

```haskell
mconcat :: Monoid a => [a] -> a
mconcat list =
  case list of
    [] -> mempty
    x : xs -> x <> mconcat xs
```

`Semigroup`은 `Monoid`의 *슈퍼 클래스*이므로, `=>` 왼쪽에 `Semigroup a`를 추가하지 않아도 `<>` 함수를 사용할 수 있습니다.
`Monoid a` 제약을 추가하면 `Semigroup a` 제약도 자동으로 추가됩니다!

`mconcat` 함수는 `concatStructure` 함수와 매우 유사하지만, `Structure` 이외의 모든 `Monoid`에 대해서도 작동합니다!
추상화를 통해 공통 패턴을 식별하고 코드를 **재사용**할 수 있습니다!

:::note
사실 하스켈에서 정수, `+` 연산, `0`은 `Monoid`의 인스턴스가 아닙니다.
왜냐하면 정수는 `*` 연산과 `1`과도 monoid를 이룰 수 있기 때문입니다!
하스켈에서 **하나의 타입에는 하나의 인스턴스만 존재**할 수 있습니다.
대신, [Sum](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Monoid.html#t:Sum)과 [Product](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Monoid.html#t:Product)
라는 두 개의 `newtype`이 존재하며 이들은 `Monoid`의 인스턴스입니다.
`ghci`에서 이들을 사용하는 방법은 다음과 같습니다:

```haskell
ghci> import Data.Monoid
ghci> Product 2 <> Product 3 -- 여기서 Product는 데이터 생성자입니다
Product {getProduct = 6}
ghci> getProduct (Product 2 <> Product 3)
6
ghci> getProduct $ mconcat $ map Product [1..5]
120
```

:::

## 또 다른 추상화?

지금까지 계속 `map`과 `mconcat`을 연속해서 사용했습니다.
이와 같은 패턴을 통합할 수 있는 함수가 있을까요?
실제로 [`foldMap`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Foldable.html#v:foldMap)
과 같은 함수가 있으며, 리스트뿐만 아니라 "접거나(folded)" "축소(reduced)"할 수 있는 모든 데이터 구조에 대해서 작동합니다.
이러한 추상화와 타입 클래스를 **Foldable**이라고 합니다.

더 간단한 `Foldable`의 이해를 위해 `fold`를 살펴보겠습니다:

```haskell
fold :: (Foldable t, Monoid m) => t m -> m

-- 다음 함수와 비교해보세요
mconcat :: Monoid m            => [m] -> m
```

`mconcat`은 단지 리스트에 대한 `fold`의 특수한 경우입니다.
또한 `fold`는 `Foldable`과 `Monoid`를 구현한 어떠한 쌍에 대해서도 사용할 수 있습니다.
예를 들어 `[]`와 `Structure` 또는 `Maybe`와 `Product Int` 또는 특별한 이진 트리와 `String`을 사용할 수 있습니다.
하지만 `Foldable`의 *kind*는 `* -> *`이어야 합니다.
따라서 `Html`은 `Foldable`이 될 수 없습니다.

`foldMap` 함수는 `<>` 함수를 사용하여 요소를 결합하기 전에, `Foldable`의 인자(payload)타입에 원하는 함수를 적용할 수 있도록 해줍니다:

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- 다음 특수한 경우와 비교해보세요
-- - t ~ []
-- - m ~ Html.Structure
-- - a ~ Markup.Structure
foldMap
  :: (Markup.Structure -> Html.Structure)
  -> [Markup.Structure]
  -> Html.Structure
```

이름에서 알 수 있듯이, `foldMap`은 "접기(fold)" 전에 "매핑(map)"을 수행합니다.
여기서 잠시 멈추고 고민해보면 "이 '매핑(map)'은 리스트 뿐만 아니라 다른 추상화에도 적용할 수 있지 않을까?"라고 생각할 수 있습니다.
네 맞습니다! 매우 중요하고 기본적인 추상화인 `Functor`가 존재합니다.
하지만 이번 장에서는 충분히 많은 추상화를 다루었으므로, 이에 대해서는 다음 장에서 다루도록 하겠습니다!

## 변환 모듈 마무리하기

`convert` 함수를 구현하면서 이번 장을 마무리하겠습니다:

```haskell
convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
```

이제 모든 구현을 완료했으며 마크업 문서를 HTML로 변환할 수 있습니다:

```haskell title="Convert.hs"
module Convert where

import qualified Markup
import qualified Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

## 요약

이번 장에서는 다음과 같은 내용을 배웠습니다:

- 한정된 가져오기
- 오류 처리 방법
- `Monoid` 타입 클래스와 추상화
- `Foldable` 타입 클래스와 추상화

다음에는 하스켈에서 IO를 다루는 방법을 다루겠습니다!

> [이곳](https://github.com/soupi/learn-haskell-blog-generator/commit/ad34f2264e9114f2d7436ff472c78da47055fcfe)에서 Git 커밋을 확인할 수 있고,
> [지금까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/ad34f2264e9114f2d7436ff472c78da47055fcfe)를 확인할 수 있습니다.
