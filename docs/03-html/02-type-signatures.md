# 타입 시그니처를 추가하기

하스켈은 **정적 타입** 프로그래밍 언어입니다.
이는 모든 표현식이 타입을 가지고 있고, 프로그램을 실행하기 전에 표현식들의 타입이 올바른지 검사한다는 뜻입니다.
만약 올바르지 않다면 에러 메시지가 출력되고 프로그램은 실행되지 않습니다.

예를 들어, 함수가 2개의 인자를 받아야 하는데 3개의 인자를 받았다거나, 숫자 대신 문자열을 제공하면 타입 에러가 발생합니다.

하스켈은 또한 **타입 추론**을 사용하기 때문에, 표현식의 타입을 명시적으로 지정할 필요는 없습니다.
하스켈은 표현식의 문맥으로부터 해당 타입이 무엇이 되야하는지 추론할 수 있습니다.
이는 우리가 지금까지 사용한 방식입니다. 하지만 **타입을 명시하는 것은 유용합니다**.
프로그램을 나중에 다시 보거나 다른 사람이 코드를 읽을 때, 타입 시그니처는 코드에 대한 문서 역할을 하기 때문입니다.
또한, 타입 시그니처는 표현식이 의도한 대로 작성되었는지 확인하는데 도움이 됩니다.
보통 **모든 최상위 정의**에 타입 시그니처를 추가하는 것이 좋습니다.

타입 시그니처는 `::` 기호를 사용하여 작성합니다.
대부분의 경우, 타입 시그니처는 정의된 이름 바로 위에 작성합니다.

여기 몇 가지 타입의 예시가 있습니다:

- `Int` - 정수 타입
- `String` - 문자열 타입
- `Bool` - 불리언 타입
- `()` - `()` 표현식의 타입, `unit`이라고도 불림
- `a -> b` - `a` 타입의 표현식을 `b` 타입의 표현식으로 변환하는 함수의 타입
- `IO ()` - `()` 값을 반환하는 IO 서브루틴을 나타내는 표현식의 타입

`title_` 함수의 타입 시그니처를 작성해봅시다:

```haskell
title_ :: String -> String
```

코드를 통해서 `title_` 함수는 `String` 타입의 인자를 받아서 `String` 타입의 값을 반환한다는 것을 알 수 있습니다.

`makeHtml` 함수의 타입 시그니처를 작성해봅시다:

```haskell
makeHtml :: String -> String -> String
```

이전에 우리는 `makeHtml` 함수가 두 개의 문자열을 받아서 문자열을 반환하는 함수라고 생각했습니다.

하지만 실제로는 하스켈에서 모든 함수는 **정확히 하나의 인자**를 받고 **정확히 하나의 값**을 반환합니다.
편의상 `makeHtml`가 여러 개의 인자를 받는 함수라고 부르는 것일 뿐입니다.

`makeHtml` 함수는 **하나의** 문자열 인자를 받아서 **함수**를 반환하는 함수라고 볼 수 있습니다.
_반환된 함수_ 또한 **하나의** 문자열 인자를 받아서 최종적으로 문자열을 반환합니다.

이렇게 해석할 수 있는 이유는 `->` 기호가 오른쪽으로 결합되기 때문입니다.
즉 다음과 같은 `makeHtml` 함수를

```haskell
makeHtml :: String -> String -> String
```

하스켈은 다음과 같이 해석합니다:

```haskell
makeHtml :: String -> (String -> String)
```

결과적으로 표현식 `makeHtml "My title"` 또한 함수입니다!
문자열 인자(본문, `makeHtml`의 두 번째 인자)를 받아서 제목으로 "My title"을 가지는 HTML 문자열을 반환하는 함수입니다.

이러한 것을 **부분 적용(partial application)**이라고 합니다.

설명을 위해, `el` 함수를 새로 정의해 `html_`와 `body_`를 다른 방식으로 구현해봅시다.

```haskell
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

`el` 함수는 태그와 내용을 받아서 태그로 감싼 내용을 반환합니다.

이제 `el` 함수를 부분 적용하여 `html_`과 `body_`를 구현할 수 있습니다.

```haskell
html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"
```

등호의 왼쪽에 인자를 작성하지 않았다는 점에 주목하세요.
이는 하스켈에서 함수는 "**일급(first class)**"이기 때문입니다 - 일반적인 표현식과 동일하게 동작합니다.
일반적인 값(`Int` 또는 `String`과 같은)처럼 이름을 부여할 수 있고, 데이터 구조에 넣을 수 있고, 함수에 전달할 수 있습니다.


하스켈에서 이름을 처리하는 방식은 복사 붙여넣기와 매우 유사합니다.
코드에서 `html_`을 볼 때마다 이를 `el "html"`로 대체할 수 있습니다.
이는 두 값이 동일하다는 것이며 수학에서의 등호의 의미와 같다는 것을 의미합니다.
이러한 속성, 즉 등호의 양쪽을 서로 **대체**할 수 있는 것을 **참조 투명성(referential transparency)** 이라고 합니다.
이는 하스켈(그리고 이와 매우 유사한 언어인 PureScript와 Elm)에서만 가능한 고유한 특징입니다!
이후 장에서 이에 대해 더 자세히 다루겠습니다.

### 익명/람다 함수


하스켈 함수는 일급이며 모든 함수는 정확히 하나의 인자를 받는다는 점에 나아가, 
지금까지 사용한 함수 정의 문법은 단지 문법적 설탕(syntactic sugar)이라는 점을 언급하려 합니다.
우리는 어디에서나 **익명 함수** (이름이 없는 함수)를 정의할 수 있습니다.
익명함수는 **람다 함수(lambda function)** 라고도 불립니다.
이는 가장 원시적인 함수형 프로그래밍 언어인 람다 대수(lambda calculus)의 영향을 받은 것입니다.

`"hello"`와 같은 표현식이 위치할 수 있는 모든곳에 다음과 같은 문법으로 익명 함수를 정의할 수 있습니다.

```haskell
\<argument> -> <expression>
```

`\` 기호(그리스 문자 람다 'λ'와 유사한 모양)는 람다 함수의 시작을 나타내며,
`->` 기호는 함수의 본문의 시작지점을 나타냅니다.
람다 함수를 체인으로 연결하여 "다중 인자 함수"를 만들 수도 있습니다:

```haskell
three = (\num1 -> \num2 -> num1 + num2) 1 2
```

이전과 같이, 함수의 인자를 주어진 값으로 대체하여 함수를 평가합니다.
위의 예제에서 `num1`을 `1`로 대체하면 `(\num2 -> 1 + num2) 2`가 됩니다.
그리고 `num2`를 `2`로 대체하면 `1 + 2`가 됩니다.
추후에 이에 대해 더 자세히 다루겠습니다.

이제 다음과 같은 함수가 있다면

```haskell
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

하스켈은 실제로 다음과 같이 해석합니다:

```haskell
el :: String -> (String -> String)
el = \tag -> \content ->
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

이러한 형태는 하스켈 함수가 왜 항상 하나의 인자만 받는지에 대한 이유를 잘 보여줍니다.

익명 함수에 대한 문법적 설탕을 하나 더 소개하겠습니다:
다중 인자 익명 함수를 정의할 때, 다음과 같이 작성할 수 있습니다:

```haskell
\<arg1> <arg2> ... <argN> -> <expression>
```

예를 들면:

```haskell
three = (\num1 num2 -> num1 + num2) 1 2
```

하지만 내부적으로 어떻게 작동하는지는 아는것 또한 중요합니다.

지금까지는 익명/람다 함수가 필요하지는 않았지만, 추후에 이를 유용하게 사용할 수 있는 상황을 보게 될 것입니다.

---

연습문제:

1. 지금까지 만든 모든 함수에 타입을 추가하세요.

2. 지금까지 만든 HTML 함수를 `el`을 사용하도록 구현하세요.

3. 문단과 제목을 정의하는 몇 가지 함수를 추가하세요:
   1. `p_`는 `<p>` 태그를 사용하는 문단을 정의합니다.
   2. `h1_`는 `<h1>` 태그를 사용하는 제목을 정의합니다.

4. `Hello, world!` 문자열을 더욱 풍부한 내용으로 바꾸고, `h1_`와 `p_`를 사용하세요.
   `h1_`와 `p_`로 만든 HTML 문자열을 이어붙이기 위해 `<>` 연산자를 사용할 수 있습니다.

---

정답:

<details>
  <summary>연습문제 #1 정답</summary>

```haskell
myhtml :: String
myhtml = makeHtml "Hello title" "Hello, world!"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"
```

</details>

<details>
  <summary>연습문제 #2 정답</summary>

```haskell
html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"
```

</details>


<details>
  <summary>연습문제 #3 정답</summary>

```haskell
p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"
```

</details>

<details>
  <summary>연습문제 #4 정답</summary>

```haskell
myhtml :: String
myhtml =
 makeHtml
   "Hello title"
   (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")
```

</details>



---

<details>
  <summary>최종 프로그램</summary>

```haskell title="hello.hs"
main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml =
 makeHtml
   "Hello title"
   (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")


makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

el :: String -> String -> String
el tag content =
 "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

</details>
