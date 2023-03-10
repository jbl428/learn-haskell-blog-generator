# 타입으로 안전한 HTML 구성하기

이번 장에서는 HTML을 위한 구분된 타입을 만드는 방법을 배웁니다.
또한, HTML 문자열을 잘못 구성하는 것을 방지하는 데 도움이 되는 방법을 배울 것입니다.

하스켈에서 새로운 타입을 정의하는 방법은 여러 가지가 있습니다.
이번 장에서는 `newtype`과 `type` 두 가지 방법을 알아보겠습니다.

## `newtype`

`newttype` 선언은 기존 값 집합에 대해 새로운 타입을 정의하는 방법입니다.
이는 기존 값을 재사용하지만 다른 의미를 부여하고 두 가지를 혼동되지 않도록 하기에 유용합니다.
예를 들어, 초, 분, 그램, 엔을 정수 값으로 표현할 수 있지만,
초와 그램을 잘못 섞어 쓰는걸 원하진 않을것입니다.

우리가 만드는 프로그램의 경우 구조화된 HTML을 텍스트 값으로 표현하고 싶지만,
유효한 HTML이 아닌 평범한 문자열과 구분하고 싶습니다.

`newtype` 선언은 다음과 같이 생겼습니다:

```
newtype <타입 이름> = <생성자> <기존 타입>
```

예를들면 `Html` 타입을 다음과 같이 정의할 수 있습니다:

```hs
newtype Html = Html String
```

등호 왼쪽의 `Html`은 `_타입_` 이름 공간(namespace)에 존재합니다.
즉, `::` 뒤에만 나올 수 있는 이름입니다.

등호 오른쪽의 `Html`은 _표현식 (혹은 terms/values) 이름 공간에 존재합니다.
즉, 표현식에서만 나올 수 있는 이름입니다. (곧 그 위치를 살펴보겠습니다).

두 이름, `<타입 이름>`과 `<생성자>`는 같을 필요는 없지만, 일반적으로 같습니다.
그리고 두 이름 모두 대문자로 시작해야 합니다.

newtype 선언의 오른쪽은 해당 타입의 모양을 의미합니다.
위의 경우, `Html` 타입의 값은 `Html` 생성자와 그 뒤에 오는 문자열 타입의 표현식을 가지고 있습니다.
예를 들면 `Html "hello"` 또는 `Html ("hello " <> "world")`가 될 수 있습니다.

생성자는 인자를 받아서 새로운 타입의 반환하는 함수로 생각할 수 있습니다:

```hs
Html :: String -> Html
```

**주의**: `Html` 타입의 표현식을 `String` 타입의 표현식과 같은 방식으로 사용할 수 없습니다.
즉, `"hello " <> Html "world"`는 타입 에러가 발생합니다.

이것은 *캡슐화(encapsulation)*가 필요할 때 유용합니다.
기본 타입에 대한 표현과 함수를 정의하고 사용할 수 있지만, 관련 없는(우리 도메인과 관련이 없는) 타입들과 혼동되지 않도록 합니다.
미터와 피트가 모두 숫자가 될 수 있지만, 우리는 변환 없이 실수로 미터와 피트를 더하는 것을 원치 않습니다.

---

이제 유용한 몇 가지 타입을 더 만들어 보겠습니다.
우리는 다음 두 가지 타입을 사용할 것입니다:

1. 완전한 HTML 문서
2. `<body>` 태그 안에 들어갈 수 있는 제목과 문단과 같은 HTML 구조에 대한 타입

우리는 두 가지 타입이 혼용되지 않기를 원합니다.

<details>
  <summary>정답</summary>

```hs
newtype Html = Html String

newtype Structure = Structure String
```

</details>

---

## `newtype` 사용하기

newtype이 감싸고 있는 내부 타입을 사용하려면, 우선 그 타입을 추출해야 합니다.
이를 위해 패턴 매칭을 사용합니다.

패턴 매칭은 두 가지 방법으로 사용할 수 있습니다.
하나는 case 표현식이고, 다른 하나는 함수 정의입니다.

1. case 표현식은 switch 표현식과 비슷하며 다음과 같이 생겼습니다:

   ```
   case <표현식> of
     <패턴> -> <표현식>
     ...
     <패턴> -> <표현식>
   ```

   `<표현식>`은 추출하고자 하는 것이고, `<패턴>`은 그것의 구체적인 모양입니다.
   예를 들어, `Structure` 타입을 정의했다고 가정해 봅시다.
   이 타입의 `String` 값을 추출하고 싶다면 다음과 같이 합니다:

   ```hs
   getStructureString :: Structure -> String
   getStructureString struct =
     case struct of
       Structure str -> str
   ```

   이렇게 하면 `Structure`에서 `String`을 추출하고 반환할 수 있습니다.

   > 이후 장에서 `data` 선언을 소개할 것입니다.
   > (이는 struct + enum의 혼성체와 비슷합니다.)
   > 여기서는 여러 생성자를 가진 타입을 정의할 수 있습니다.
   > 그러면 case 표현식의 여러 패턴이 더 의미가 있게 될 것입니다.

2. 다른 방법으로, 함수를 정의할 때, 인자에 대해 패턴 매칭을 사용할 수 있습니다:

   ```
   func <패턴> = <표현식>
   ```

   예를 들면:

   ```hs
   getStructureString :: Structure -> String
   getStructureString (Structure str) = str
   ```

   이제 이전에 정의한 `html_`, `body_`, `p_` 등의 함수를 `String` 대신에 새로운 타입을 사용하도록 변경할 수 있습니다.

   그 전에, 우리의 코드를 더 간결하게 만들 수 있는 또 다른 연산자를 만나보겠습니다.


`newtype`의 또 다른 유용한 점은, 표현식을 감싸고 추출하는 것이 실제로는 성능에 영향을 주지 않는다는 것입니다!
컴파일러는 `newtype` 생성자의 감싸고 추출하는 것을 제거하고 기본 타입을 사용합니다.

우리가 정의한 타입과 생성자는 오직 *우리가 코드를 작성할 때* 기존 타입과 새로운 타입을 *구분*하는 데 도움을 주는 것이고,
*코드가 실행 중*에는 필요하지 않습니다.

`newtype`은 타입 안전성을 제공하면서도 성능에 영향을 주지 않습니다!

## 함수 합성

또 다른 흥미롭고 굉장히 자주 쓰이는 연산자로 (하스켈 일반 라이브러리 함수인) `.` (compose)가 있습니다.
이 연산자는 수학에서 알고 있는 합성 연산자(`∘`)와 비슷하게 생겼습니다.

연산자의 타입과 구현을 살펴보겠습니다:

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

이 연산자는 3 개의 인자를 받습니다: 두 개의 함수(여기서는 `f`와 `g`라고 부릅니다)와 세 번째 인자 `x`입니다.
그런 다음 `x`를 두 번째 함수 `g`에 전달하고, `g x`의 결과를 첫 번째 함수 `f`에 전달합니다.

`g` 함수가 `a` 타입의 값을 받고 `b` 타입의 값을 반환하고, `f` 함수가 `b` 타입의 값을 받고 `c` 타입의 값을 반환한다는 점을 주목하세요.

또 다른 주목해야 할 점으로 _소문자_로 시작하는 타입이 있는데 이는 **타입 변수(type variables)**라 합니다.
이는 일반적인 변수와 비슷합니다. `content`는 `hello`나 `world`처럼 어떤 문자열이 될 수 있듯이,
타입 변수는 `Bool`, `String`, `String -> String` 등 어떤 타입이 될 수 있습니다.
이러한 기능을 **매개변수 다형성(parametric polymorphism)**이라고 합니다.
(다른 언어에서는 이를 제네릭(generics)이라고 부릅니다.)

주의해야 할 점은 타입 변수는 반드시 서명(signature)과 일치해야 한다는 것입니다.
예를 들어, `a -> a`라는 서명을 가진 함수를 정의하려고 한다면, 입력 타입과 반환 타입은 반드시 일치해야 하지만,
어떤 타입이든 될 수 있습니다. 그래서 이 서명을 가진 함수를 구현하는 유일한 방법은 다음과 같습니다:

```hs
id :: a -> a
id x = x
```

항등 함수를 의미하는 `id`는 받은 값을 그대로 반환합니다.
만약 다른 방법을 사용하려고 하면, 예를 들어 `"hello"`와 같은 임의의 값을 반환하거나,
`x`를 알고 있는 타입의 값처럼 사용해 `x + x`와 같은 표현식을 작성하면, 타입 오류가 발생합니다.

`->` 연산자는 오른쪽으로 결합되는 것을 기억하시나요? 이 서명은 다음과 같이 해석할 수 있습니다:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

이는 마치 함수가 두 함수를 받아서 합성한 함수를 반환하는 것처럼 보이지 않나요?

이제 이 연산자를 사용해 HTML 함수를 다시 작성해보겠습니다.
`p_` 함수부터 시작해보겠습니다.

기존 정의는 다음과 같았습니다:

```hs
p_ :: String -> String
p_ = el "p"
```

이제, 다음과 같이 작성할 수 있습니다:

```hs
p_ :: String -> Structure
p_ = Structure . el "p"
```

`p_` 함수는 문단을 의미하는 임의의 `String`을 받아서 `<p>`와 `</p>` 태그로 감싼 후,

타입을 더 자세히 살펴보겠습니다:

- `Structure :: String -> Structure`
- `el "p" :: String -> String`
- `(.) :: (b -> c) -> (a -> b) -> (a -> c)`
- `Structure . el "p" :: String -> Structure`

표현식 `Structure . el "p"`가 타입 검사를 통과하는 이유와, 타입이 `String -> Structure`인 이유를 살펴보겠습니다.

### 펜과 종이로 타입 검사하기
### Type checking with pen and paper

표현식이 어떻게 타입 검사를 통과하는지 이해하고 싶다면, 체계적으로 타입 검사를 해보는 것이 좋습니다.
다음 예제를 살펴보겠습니다. 이 예제에서 우리는 다음 표현식을 타입 검사하려고 합니다.

```hs
p_ = Structure . el "p"
```

우선, 가장 바깥쪽 함수의 타입을 적습니다. 이 경우에는 연산자 `.`의 타입입니다:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

그런 다음, 우리는 이 함수에 적용할 인자의 타입과 함수서명의 타입과 **비교**합니다.

먼저, `.`의 두 개의 인자의 타입을 확인합니다:

1. `Structure :: String -> Structure`
2. `el "p" :: String -> String`

다행히 `.` 연산자는 두 개의 인자를 받고, 다음과 같은 타입을 가집니다:

1. `b -> c`
2. `a -> b`

> 주의: 함수가 받을 수 있는 인자의 수보다 더 많은 인자를 적용하면 타입 오류가 발생합니다.

`.` 연산자는 우리가 제공한 인자의 수만큼 인자를 받기 때문에, 다음 단계로 넘어갑니다:
타입 검사의 다음 단계는 입력 타입과 (연산자의 서명을 통해) 예상되는 타입을 비교하는 것입니다.

두 타입을 비교할 때, 둘 간의 *동등성(equivalence)*를 확인합니다.
여기에는 몇 가지 가능한 시나리오가 있습니다:

1. 두 타입이 `Int`, `Bool`과 같은 **구체적(concrete)**(타입 변수가 아닌)이고 **단순한** 타입인 경우,
   두 타입이 같은지 확인합니다. 만약 같다면, 타입 검사를 통과하고 계속 진행합니다.
   만약 다르다면, 타입 검사를 통과하지 못하고 오류를 발생시킵니다.
2. 두 타입이 더 **복잡한** 경우(예를 들어 둘 다 함수인 경우),
   입력과 출력을 비교합니다. 만약 입력과 출력이 일치한다면, 두 타입이 일치한다고 판단합니다.
3. 두 타입중 하나가 **타입 변수**인 특별한 경우가 있습니다.
   이 경우, 타입 검사를 방정식처럼 처리합니다. 그리고 어딘가에 적어둡니다.
   다음에 이 타입 변수를 볼 때, *방정식에 있는 값으로 대체*합니다.
   이것을 타입 변수에 값을 *할당(assign)* 한다고 생각하면 됩니다.

위 예제에서, 우리는 다음 두 가지 타입을 비교해야 합니다:

1. `String -> Structure` 과 `b -> c`
2. `String -> String` 과 `a -> b`

하나씩 살펴보겠습니다. 먼저, `String -> Structure`과 `b -> c`를 비교해보겠습니다:

1. 두 타입이 복잡하므로, 두 타입이 모두 함수라는 것을 확인하고, 입력과 출력을 비교합니다.
   `String`과 `b`, 그리고 `Structure`와 `c`를 비교합니다.
2. `b`는 **타입 변수**이므로, 어딘가에 적어둡니다. `b`는 `String`과 동등하다고 적어둡니다.
   `b ~ String` (우리는 `~`를 동등함을 나타내는 기호로 사용합니다).
3. 같은 방식으로 `c`도 `Structure`와 동등하다고 적어둡니다. `c ~ Structure`.

지금까지 문제는 없습니다. 이제 `String -> String`과 `a -> b`를 비교해보겠습니다:

1. 두 타입이 복잡하므로, 두 타입이 모두 함수라는 것을 확인하고, 입력과 출력을 비교합니다.
2. `String`과 `a`를 비교합니다. `a ~ String`으로 적어둡니다.
3. `String`과 `b`를 비교합니다. `b`는 이미 적어둔 방정식을 가지고 있습니다.
   `b ~ String`이라고 적어둔 것을 기억합니다. `b`를 `String`으로 대체하고,
   이 타입과 비교합니다. `String`과 `String`을 비교하므로, 타입 검사를 통과합니다.

우리는 무사히 표현식을 타입 검사했고, 다음과 같은 타입 변수의 동등성을 찾았습니다:

1. `a ~ String`
2. `b ~ String`
3. `c ~ Structure`

이제, 다음 표현식의 타입을 찾아보겠습니다:

```hs
p_ = Structure . el "p"
```

이 표현식의 타입을 찾기 위해, 우리가 찾은 방정식을 사용하여 타입 변수를 *대체*하고,
우리가 적용한 인자를 *제거*합니다. 그래서 다음과 같은 타입을 얻습니다:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

그리고 다음과 같이 타입 변수를 대체합니다:

```hs
(.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)
```

그리고 함수를 적용하면서 두 개의 인자를 제거합니다:

```hs
Structure . el "p" :: String -> Structure
```

이제, 우리는 이 표현식의 타입을 찾았습니다!

다행히, 하스켈은 이 과정을 대신 해줄 수 있습니다. 하지만 하스켈이 타입 에러를 발생시키는 경우,
그 이유를 이해하지 못하는 상황이 발생할 수 있습니다. 이럴때 위와 같은 과정을 거치면,
타입이 일치하지 않는 부분을 찾을 수 있고, 그 부분을 해결할 수 있습니다.


> **주의**: 만약 우리가 *매개변수화된 다형성*을 가진 함수를 두 번 이상 사용하거나,
> 비슷한 타입 변수 이름을 가진 다른 함수를 사용한다면,
> 이름이 같다고 해서 모든 인스턴스에서 타입 변수가 일치할 필요는 없습니다.
> 각 인스턴스는 고유한 타입 변수 집합을 가집니다. 예를 들어:
> 
> ```hs
> id :: a -> a
> ord :: Char -> Int
> chr :: Int -> Char
> 
> incrementChar :: Char -> Char
> incrementChar c = chr (ord (id c) + id 1)
> ```
> 
> 위의 코드에서 `id`를 두 번 사용합니다. (예시로 사용하기 위함일 뿐 좋은 구조는 아닙니다)
> 첫 번째 `id`는 `Char`를 인자로 받고, `a`는 `Char`와 동등합니다.
> 두 번째 `id`는 `Int`를 인자로 받고, *구별된* `a`는 `Int`와 동등합니다.
> 
> 이 상황은 오직 최상위(top-level)에 정의한 함수에만 적용됩니다.
> 만약 우리가 `incrementChar`에 인자로 전달할 지역 함수를 정의하고,
> `id`와 같은 타입 시그니처를 가진다면, 모든 사용처에서 타입이 일치해야 합니다.
> 다음 코드의 경우:
> 
> ```hs
> incrementChar :: (a -> a) -> Char -> Char
> incrementChar func c = chr (ord (func c) + func 1)
> ```
> 
> 타입 에러가 발생합니다. 직접 확인해보세요!

## Appending Structure

Before when we wanted to create richer HTML content and appended
nodes to one another, we used the append (`<>`) operator.
Since we are now not using `String` anymore, we need another way
to do it.

While it is possible to overload `<>` using a feature in
Haskell called type classes, we will instead create a new function
and call it `append_`, and cover type classes later.

`append_` should take two `Structure`s, and return a third `Structure`,
appending the inner `String` in the first `Structure` to the second and wrapping the result back in `Structure`.

---

Try implementing `append_`.

<details>
  <summary>Solution</summary>

```hs
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)
```

</details>

---

## Converting back `Html` to `String`

After constructing a valid `Html` value, we want to be able to
print it to the output so we can display it in our browser.
For that, we need a function that takes an `Html` and converts it to a `String`, which we can then pass to `putStrLn`.

---

Implement the `render` function.

<details>
  <summary>Solution</summary>

```hs
render :: Html -> String
render html =
  case html of
    Html str -> str
```

</details>

---

## `type`

Let's look at one more way to give new names to types.

A `type` definition looks really similar to a `newtype` definition - the only
difference is that we reference the type name directly without a constructor:

```
type <type-name> = <existing-type>
```

For example in our case we can write:

```hs
type Title = String
```

`type`, in contrast with `newtype`, is just a type name alias.
When we declare `Title` as a *type alias* of `String`,
we mean that `Title` and `String` are interchangeable,
and we can use one or the other whenever we want:

```hs
"hello" :: Title

"hello" :: String
```

Both are valid in this case.

We can sometimes use `type`s to give a bit more clarity to our code,
but they are much less useful than `newtype`s which allow us to
*distinguish* two types with the same type representation.

## The rest of the owl

---

Try changing the code we wrote in previous chapters to use the new types we created.

> **Tips**
>
> We can combine `makeHtml` and `html_`, and remove `body_` `head_` and `title_`
> by calling `el` directly in `html_`, which can now have the type
> `Title -> Structure -> Html`.
> This will make our HTML EDSL less flexible but more compact.
>
> Alternatively, we could create `newtype`s for `HtmlHead` and `HtmlBody` and
> pass those to `html_`, and we might do that at later chapters, but I've chosen
> to keep the API a bit simple for now, we can always refactor later!

<details>
  <summary>Solution</summary>

```hs
-- hello.hs

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

</details>

---

## Are we safe yet?

We have made some progress - now we can't write `"Hello"`
where we'd expect either a paragraph or a heading, but we can still
write `Structure "hello"` and get something that isn't a
paragraph or a heading. So while we made it harder for the user
to make mistakes by accident, we haven't really been able to **enforce
the invariants** we wanted to enforce in our library.

Next we'll see how we can make expressions such as `Structure "hello"` illegal
as well using *modules* and *smart constructors*.
