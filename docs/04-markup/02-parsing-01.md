# 마크업 파싱하기 01 (재귀)

이전 장에서 정의한 `Document` 타입을 사용하여 사용자가 작성한 여러 줄의 마크업 텍스트를 파싱하는 방법을 살펴보겠습니다.

전략은 마크업 문자열을 다음과 같이 처리하는 것입니다:

1. 각 줄을 개별적인 요소로 분할하고
2. 요소를 순회하면서 줄 별로 처리하고, 필요하다면 이전 줄의 정보를 기억합니다

따라서 처음 해야할 일은 문자열을 줄 단위로 처리하는 것입니다.
이를 위해 문자열을 문자열의 리스트로 변환할 수 있습니다.
다행히도 하스켈 표준 라이브러리 [`base`](https://hackage.haskell.org/package/base)의
[`Prelude`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)
모듈에서 [`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)
함수를 제공하고 있습니다.
`Prelude` 모듈은 기본적으로 모든 하스켈 파일에서 사용할 수 있으므로 가져올(import) 필요가 없습니다.

줄 처리를 위해, 먼저 마크업 문법은 무시하고 줄을 그룹화하여 단락(paragraph)으로 만들어 보겠습니다.
(단락은 빈 줄로 구분됩니다) 그리고 나중에 장의 뒷부분에서 새로운 기능을 반복적으로 추가할 것입니다.

명령형 프로그램에서의 일반적인 해결책은 _loop_ 구조를 사용하여 순회하고 그룹화해야 하는 줄을 중간 가변 변수에 누적하는 것입니다.
빈 줄에 도달하면 그 변수의 내용을 다른 가변 변수에 누적하여 결과를 저장합니다.

하스켈의 접근 방식은 루프나 가변 변수를 사용하지 않는다는 점을 제외하고는 크게 다르지 않습니다.
하스켈은 대신 **재귀**를 사용합니다.

## 재귀와 정보 누적

loop 대신, 하스켈에서는 재귀를 사용하여 반복을 모델링합니다.

다음과 같은 인위적인 예를 생각해 보겠습니다: 두 개의 자연수를 더하는 알고리즘이 필요하다고 가정해 보겠습니다.
하지만 일반적인 덧셈 연산자 `+`를 사용할 수 없고, 각 숫자에 대해 `increment`와 `decrement`라는 두 개의 연산을 사용할 수 있습니다.

이 문제를 해결하기 위한 방법은 하나의 숫자를 다른 숫자로 점진적으로 "전달"하는 것입니다.
즉, 하나를 증가시키고 다른 하나를 감소시키고, 이를 반복하여 두 번째 숫자가 0이 될 때까지 수행합니다.

예를 들어 `3`과 `2`를 더한다고 가정해 보겠습니다:

- `3`과 `2`를 시작으로 합니다. `3`을 증가시키고 `2`를 감소시킵니다.
- 다음 단계에서는 `4`와 `1`이 됩니다. `4`를 증가시키고 `1`을 감소시킵니다.
- 다음 단계에서는 `5`와 `0`이 됩니다. `2`가 `0`이므로 `5`를 결과로 반환합니다.

이를 명령형으로 작성하면 다음과 같습니다:

```js
function add(n, m) {
  while ((m /= 0)) {
    n = increment(n);
    m = decrement(m);
  }
  return n;
}
```

하스켈에서는 변수의 변경없이 재귀를 사용하여 동일한 알고리즘을 작성할 수 있습니다:

```haskell
add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n
```

하스켈에서는 *가변 상태를 활용해 반복을 구현*하는 대신, 함수를 다시 호출하여 변수의 값을 다음 반복에 사용하도록 합니다.

### 재귀의 평가

배귀는 일반적으로 loop보다 느리고, 안전하지 않다고 알려져 있습니다.
이는 명령형 언어에서 함수 호출이 새로운 호출 스택을 생성을 요구하기 때문입니다.

그러나 함수형 언어(특히 하스켈)는 다른 규칙을 따르며, _꼬리 호출 제거(tail call elimination)_ 이라는 기능을 구현합니다.
함수 호출의 결과가 함수 자체의 결과인 경우(이를 _꼬리 위치(tail position)_ 이라고 함),
현재 스택 프레임을 삭제하고 호출하는 함수를 위한 하나의 스택 프레임을 할당하여 `N`번의 반복에 대해 `N`개의 스택 프레임이 필요하지 않도록 합니다.

물론, 이는 꼬리 호출 제거를 수행하는 한 가지 방법일 뿐이며,
우리가 위에서 재귀적으로 작성한 `add` 함수와 같은 코드를 반복 버전으로 변환하는 것과 같은 다른 전략도 존재합니다.

#### 지연성

Haskell은 흔히 사용되는 엄격한 평가 전략(strict evaluation strategy)이 아닌 *지연 평가 전략(lazy evaluation strategy)*을 사용하기 때문에 조금 다른 규칙을 따릅니다.
여기서 *평가 전략(evaluation strategy)*이란 "언제 계산을 평가할 것인가"를 의미합니다.
엄격한 언어에서의 법칙은 간단합니다: _함수에 전달되는 인수를 함수에 진입하기 전에 평가합니다_.

예를 들어 `add (increment 3) (decrement 2)`를 엄격하게 평가하면 다음과 같습니다:

1. `increment 3`을 평가하여 `4`를 얻습니다.
2. `decrement 2`를 평가하여 `1`을 얻습니다.
3. `add 4 1`을 평가합니다
4. Evaluate `increment 3` to `4`
5. Evaluate `decrement 2` to `1`
6. Evaluate `add 4 1`

또는 (언어에 따라서) (1)과 (2)의 순서를 반대로 하고 인수를 오른쪽에서 왼쪽으로 평가합니다.

반면 지연 평가 전략에서는, _실제로 필요한 시점에만 계산을 평가합니다_.
예를 들어 표준 출력에 연산을 출력하거나 네트워크로 보내는 등, 외부 세계에 영향을 미치는 연산을 포함할 때 평가합니다.

따라서 계산이 필요하지 않은 경우, 평가하지 않습니다. 예들 들면:

```haskell
main =
  if add (increment 2) (decrement 3) == 5
    then putStrLn "Yes."
    else putStrLn "No."
```

이 경우 `add (increment 2) (decrement 3)`의 결과가 필요하므로 평가합니다. 하지만:

```haskell
main =
  let
    five = add (increment 2) (decrement 3)
  in
    putStrLn "Not required"
```

이 경우는 `five`가 필요하지 않으므로 평가하지 않습니다!

그렇다면 `add (increment 2) (decrement 3)`가 필요하다고 판단하면 엄격한 평가를 수행할까요?
그렇지 않습니다 - 왜냐하면 계산을 완료하기 위해 인자를 평가할 필요가 없을 수도 있기 때문입니다.
예를 들어 다음과 같은 경우:

```haskell
const a b = a

main =
  if const (increment 2) (decrement 3) == 3
    then putStrLn "Yes."
    else putStrLn "No."
```

`const`는 두 번째 인자를 무시하고 첫 번째 인자를 반환하므로, 계산의 결과를 제공하고 화면에 출력하기 위해 `decrement 3`을 평가할 필요가 없습니다.

지연 평가 전략에서는 필요할 때만 계산을 평가하며 (사용자가 무언가를 수행하기 위해 필요할 때),
바깥에서 안으로 평가합니다 - 먼저 함수에 진입하고, 그 다음에 필요할 때 인자를 평가합니다 (대부분의 경우 `if` 표현식의 조건이나 패턴 매칭의 패턴에 나타날 때).

---

하스켈에서 이런 전략이 어떻게 이루어지는지에 대한 자세한 내용을 다음 블로그 포스트에서 확인할 수 있습니다:
[Substitution and Equational Reasoning](https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning).

읽어본 후, 다음 프로그램을 직접 평가해 보세요:

```haskell
import Prelude hiding (const) -- 이 줄은 무시해도 좋습니다

increment n = n + 1

decrement n = n - 1

const a b = a

add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n

main =
  if const (add 3 2) (decrement 3) == 5
    then putStrLn "Yes."
    else putStrLn "No."
```

평가는 언제나 `main`에서 시작하는 점을 명심하세요.

<details>
  <summary>정답</summary>

`main` 평가

```haskell
if const (add 3 2) (decrement 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`const` 평가

```haskell
if add 3 2 == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`add` 평가

```haskell
if (if 2 /= 0 then add (increment 3) (decrement 2) else 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `2 /= 0` 평가

```haskell
if (if True then add (increment 3) (decrement 2) else 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`then` 분기 평가

```haskell
if (add (increment 3) (decrement 2)) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`add` 평가

```haskell
if
  ( if decrement 2 /= 0
    then add
      (increment (increment 3))
      (decrement (decrement 2))
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문에서 `decrement 2` 평가 (두 곳이 변경되는 것에 주목하세요!)

```haskell
if
  ( if 1 /= 0
    then add
      (increment (increment 3))
      (decrement 1)
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `1 /= 0` 평가

```haskell
if
  ( if True
    then add
      (increment (increment 3))
      (decrement 1)
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`then` 분기 평가

```haskell
if
  ( add
    (increment (increment 3))
    (decrement 1)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`add` 평가

```haskell
if
  ( if decrement 1 /= 0
    then add
      (increment (increment (increment 3)))
      (decrement (decrement 1))
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `decrement 1` 평가

```haskell
if
  ( if 0 /= 0
    then add
      (increment (increment (increment 3)))
      (decrement 0)
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `0 /= 0` 평가

```haskell
if
  ( if False
    then add
      (increment (increment (increment 3)))
      (decrement 0)
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

`else` 분기 평가

```haskell
if
  (increment (increment 3)) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `increment (increment 3)` 평가

```haskell
if
  (increment 3 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `increment 3` 평가

```haskell
if
  (3 + 1 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `3 + 1` 평가

```haskell
if
  (4 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `4 + 1` 평가

```haskell
if
  5 == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

제어문 `5 == 5` 평가

```haskell
if
  True
  then putStrLn "Yes."
  else putStrLn "No."
```

`then` 분기 평가

```haskell
putStrLn "Yes."
```

화면에 `Yes.`를 출력합니다.

</details>

---

### 일반 재귀

일반적으로, 재귀적으로 문제를 해결하려면 다음 세 가지 부분을 고려해야 합니다:
In general, when trying to solve problems recursively, it is useful to think
about the problem in three parts:

1. **기저 사례(base case)** (가장 간단한 경우 - 이미 답을 알고 있는 경우)
2. 문제를 더 간단한 것으로 **축소(reduce)**하는 방업을 찾습니다. (기저 사례에 가까워지도록)
3. 축소된 버전과 제공해야 하는 해결책 간의 **차이를 완화(mitigate)**합니다.

축소와 완화 단계를 합쳐서 *재귀 단계(recursive step)*라고 부릅니다.

또 다른 예제 문제를 살펴보겠습니다: 특정 크기로 리스트를 생성하고 각 요소에 특정 값을 넣는 것입니다.

하스켈에서는, 다음과 같은 시그니처를 가진 함수가 있습니다:

```haskell
replicate :: Int -> a -> [a]
```

다음은 `replicate`의 몇 가지 사용 예입니다:

```haskell
ghci> replicate 4 True
[True,True,True,True]
ghci> replicate 0 True
[]
ghci> replicate (-13) True
[]
```

어떻게 이 함수를 재귀적으로 구현할 수 있을까요? 위의 세 단계를 어떻게 적용할 수 있을까요?

1. **기저 사례**: 이미 알고 있는 경우는 리스트의 길이가 0(또는 그 이하)인 경우입니다. 이 경우에는 빈 리스트를 반환합니다.
2. **축소**: (양수인) `N` 크기의 리스트를 생성하는 방법을 모르더라도, `N-1` 크기의 리스트를 생성하는 방법을 알고 있다면 유도할 수 있습니다.
3. **완화**: `N-1` 크기일 때의 해답에 `:` (cons) 연산자를 사용하여 다른 요소를 추가합니다.

---

하스켈로 이를 구현해보세요!

<details>
<summary>정답</summary>

```haskell
replicate :: Int -> a -> [a]
replicate n x =
  if n <= 0    -- 기저 사례인 경우
    then
      []       -- 기저 사례의 답
    else
        x : replicate (n - 1) x
  --   ---  -------------------
  --    ^           ^
  --    |           |
  --    |           +-------- 축소
  --    |
  --    +--- 완화
```

</details>

---

### 상호 재귀

함수를 재귀적으로 해결할 때 우리는 보통 같은 함수를 다시 호출합니다.
하지만 꼭 그렇게 할 필요는 없습니다. 다른 함수를 호출하여 문제를 더 간단한 것으로 축소할 수도 있습니다.
만약, 그 함수가 (또는 호출 체인의 또 다른 함수가) 다시 기존 함수를 호출한다면, 이는 **상호 재귀적(mutual recursive)** 이라고 할 수 있습니다.

예를 들어, 주어진 자연수가 짝수인지 홀수인지를 판별하는 두 개의 함수를 작성해보겠습니다.
이 함수들은 주어진 수를 감소시키는 방법을 사용하여 문제를 해결할 수 있습니다.

```haskell
even :: Int -> Bool

odd :: Int -> Bool
```

먼저 `even`을 구현해보겠습니다. 어떻게 하면 이 함수를 재귀적으로 구현할 수 있을까요?

1. **기저 사례**: 이미 알고 있는 경우는 `0`인 경우입니다. 이 경우에는 `True`를 반환합니다.
2. **축소**: 일반적인 `N`에 대한 답을 모르더라도, `N - 1`에 대한 답은 확인할 수 있습니다.
3. **완화**: `N - 1`이 홀수인 경우, `N`은 짝수입니다! `N - 1`이 홀수가 아닌 경우, `N`은 짝수가 아닙니다.

`odd`는 어떻게 구현할 수 있을까요?

1. **기저 사례**: 이미 알고 있는 경우는 `0`인 경우입니다. 이 경우에는 `False`를 반환합니다.
2. **축소**: 일반적인 `N`에 대한 답을 모르더라도, `N - 1`에 대한 답은 확인할 수 있습니다.
3. **완화**: `N - 1`이 짝수인 경우, `N`은 홀수입니다! `N - 1`이 짝수가 아닌 경우, `N`은 홀수가 아닙니다.

---

이제 하스켈로 구현해보세요!

<details>
<summary>정답</summary>

```haskell
even :: Int -> Bool
even n =
  if n == 0
    then
      True
    else
      odd (n - 1)

odd :: Int -> Bool
odd n =
  if n == 0
    then
      False
    else
      even (n - 1)

```

</details>

---

## 부분 함수

위 예제에서 음수에 대한 경우는 다루지 않았기때문에, 음수를 입력으로 받으면 무한 루프에 빠질 수 있습니다.
특정 값에 대해 결과를 반환하지 않는 함수(함수가 종료되지 않거나 에러를 발생시키는 경우)를
(가능한 입력의 일부에 대해서만 결과를 반환하기 때문에) **부분 함수(partial function)** 라고 합니다.

부분 함수는 런타임에 예상하지 못한 동작(런타임 예외 또는 무한루프)을 할 수 있기 때문에, **나쁜 관례(bad practice)**로 여겨집니다.
그래서, 부분 함수 **사용을 지양하고**, 부분 함수 **작성을 지양해야**합니다.

부분 함수를 작성하지 않는 가장 좋은 방법은 모든 입력을 처리하는 것입니다!
위의 경우에는 음수를 처리할 수 있기 때문에, 이를 처리해야합니다!
또는 함수가 `Int` 대신 `Natural`을 받도록 만들 수 있고, 이 경우 타입 시스템이 우리가 처리하지 않은 값을 사용하는 것을 막아줄 것입니다.

모든 입력을 처리할 수 없는 경우에는, 코드를 다시 검토해보고 타입을 사용하여 입력을 더 제한할 수 있는지 확인해야합니다.

예를 들어, `Prelude` 모듈의 `head :: [a] -> a` 함수는 리스트의 첫 번째 요소를 반환할 것으로 기대합니다.
하지만 빈 목록을 제공한다면, 이 함수가 기대한 대로 동작할 수 있을까요?

아쉽게도, 그렇지 않습니다. 하지만 정상적으로 동작하는 다른 함수가 있습니다:

바로 [`Data.List.NonEmpty`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List-NonEmpty.html) 모듈의
`head :: NonEmpty a -> a` 함수입니다!
이 함수는 일반 리스트가 아닌 완전히 다른 타입을 입력으로 받습니다.
이 타입은 적어도 하나의 요소가 있음을 보장하기 때문에, 함수는 기대한 대로 동작할 수 있습니다!

이전 장에서 본 것처럼, `newtype`을 활용하는 스마트 생성자를 사용해 타입 시스템에 어떤 제한을 강제할 수도 있습니다.
하지만 이 해결책은 때때로 사용하기에 불편할 수 있습니다.

또 다른 해결책은 `data` 타입을 사용하여 결과가 없는 경우를 표현하는 것입니다.
예를 들어, `Maybe` 타입을 사용할 수 있습니다.
이에 대해서는 이후 장에서 자세히 다루겠습니다.

함수를 작성할 때에는 입력을 제한하거나, 결과가 없는 경우를 표현하는 타입을 사용해서 모든 입력에 대해 결과를 반환할 수 있도록 해야합니다.

## 마크업 파싱하기

우리의 원래 목표로 돌아가봅시다.

이전에 언급했듯이, 마크업을 파싱하는 전략은 다음과 같습니다:

1. 문자열을 줄 단위로 분할해 리스트를 만듭니다.
   (이는 [`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)) 함수를 사용하여 수행할 수 있습니다.)
2. 리스트를 순회하면서, 각 줄을 처리합니다.
   필요한경우 이전 줄의 정보를 기억합니다.

처음에는 모든 마크업 문법은 무시하고 줄을 그룹화하여 문단을 만들기로 한 것을 기억하세요. (문단은 빈 줄로 구분됩니다)
그리고 이후 장에서 점진적으로 새로운 기능을 추가합니다.

```haskell
parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
  in
    case txts of -- (4)
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest -- (5)
          else
            parseLines (currentLine : currentParagraph) rest -- (6)

trim :: String -> String
trim = unwords . words
```

주목할 점:

1. 문단으로 그룹화할 리스트를 전달합니다. (문단은 빈 줄로 구분됩니다)
2. 지연평가로 인해, `paragraph`는 필요할 때까지 계산되지 않습니다.
   따라서, 아직 문단을 그룹화하는 중이라면 성능을 걱정할 필요가 없습니다.
3. 왜 `currentParagraph`를 뒤집을까요? (6번을 참고하세요)
4. case 표현식을 사용해 `newtype`과 `Char`를 구조분해 한것처럼, 다른 ADT와 리스트에 대해서도 패턴 매칭을 할 수 있습니다!
   이 경우에는 두 가지 패턴을 사용합니다. 빈 리스트 (`[]`)과 "cons cell" - 적어도 하나의 요소가 있는 리스트 (`currentLine : rest`)입니다.
   "cons" 패턴의 본문에서, 첫 번째 요소를 `currentLine`이라는 이름으로 바인딩하고, 나머지 요소를 `rest`라는 이름으로 바인딩합니다.

   이것이 어떻게 작동하는지는 곧 알아볼 예정입니다!

5. 빈 줄에 도달한 경우 누적한 문단을 리스트에 추가하고 (`Document`는 `Structure`의 리스트입니다) 나머지 입력으로 함수를 다시 호출합니다.
6. 새로운 줄을 문단에 추가할 때 **역순으로** 추가합니다.
   이는 성능을 고려했기 때문입니다 - 단일 연결 리스트의 특성 때문에, 요소를 추가하는 것은 느리지만, 앞에 추가하는 것은 빠릅니다.
   요소를 앞에 추가하는 것은 값과 리스트를 가리키는 포인터를 담은 새로운 cons (`:`) 셀을 생성하는 것만으로 충분하지만,
   요소를 뒤에 추가하는 것은 리스트의 끝까지 이동하고 cons 셀을 재구성해야 합니다.
   마지막 cons 셀은 리스트의 마지막 값을 가리키고 추가할 리스트를 가리키는 포인터를 포함하고,
   그 다음 cons 셀은 리스트의 마지막 값의 앞의 값을 가리키고 마지막 요소와 추가된 리스트를 가리키는 포인터를 포함합니다.
   이와 같은 과정이 반복됩니다.

위 함수는 이제 우리가 원하는 기능을 제공할것입니다.
하지만 함수의 결과를 어떻게 확인할 수 있을까요?
다음 장에서는 타입 클래스에 대해 간략히 살펴보고, 이를 사용하여 결과를 확인하는 방법을 알아보겠습니다.
