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

명령형 프로그램에서의 일반적인 해결책은 *loop* 구조를 사용하여 순회하고 그룹화해야 하는 줄을 중간 가변 변수에 누적하는 것입니다.
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

그러나 함수형 언어(특히 하스켈)는 다른 규칙을 따르며, *꼬리 호출 제거(tail call elimination)* 이라는 기능을 구현합니다.
함수 호출의 결과가 함수 자체의 결과인 경우(이를 *꼬리 위치(tail position)* 이라고 함),
현재 스택 프레임을 삭제하고 호출하는 함수를 위한 하나의 스택 프레임을 할당하여 `N`번의 반복에 대해 `N`개의 스택 프레임이 필요하지 않도록 합니다.

물론, 이는 꼬리 호출 제거를 수행하는 한 가지 방법일 뿐이며,
우리가 위에서 재귀적으로 작성한 `add` 함수와 같은 코드를 반복 버전으로 변환하는 것과 같은 다른 전략도 존재합니다.

#### 지연성

Haskell은 흔히 사용되는 엄격한 평가 전략(strict evaluation strategy)이 아닌 *지연 평가 전략(lazy evaluation strategy)*을 사용하기 때문에 조금 다른 규칙을 따릅니다.
여기서 *평가 전략(evaluation strategy)*이란 "언제 계산을 평가할 것인가"를 의미합니다.
엄격한 언어에서의 법칙은 간단합니다: *함수에 전달되는 인수를 함수에 진입하기 전에 평가합니다*.

예를 들어 `add (increment 3) (decrement 2)`를 엄격하게 평가하면 다음과 같습니다:

1. `increment 3`을 평가하여 `4`를 얻습니다.
2. `decrement 2`를 평가하여 `1`을 얻습니다.
3. `add 4 1`을 평가합니다
1. Evaluate `increment 3` to `4`
2. Evaluate `decrement 2` to `1`
3. Evaluate `add 4 1`

또는 (언어에 따라서) (1)과 (2)의 순서를 반대로 하고 인수를 오른쪽에서 왼쪽으로 평가합니다.

반면 지연 평가 전략에서는, *실제로 필요한 시점에만 계산을 평가합니다*.
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

### General recursion

In general, when trying to solve problems recursively, it is useful to think
about the problem in three parts:

1. Finding the **base case** (the most simple cases - the ones we already know how to answer)
2. Figuring out how to **reduce** the problem to something simpler (so it gets closer to the base case)
3. **Mitigating the difference** between the reduced version and the solution we need to provide

The reduce and mitigate steps together are usually called the _recursive step_.

Let's take a look at another example problem: generating a list of a particular size
with a specific value in place of every element.

In Haskell, this function would have the following signature:

```haskell
replicate :: Int -> a -> [a]
```

Here are a few usage examples of `replicate`:

```haskell
ghci> replicate 4 True
[True,True,True,True]
ghci> replicate 0 True
[]
ghci> replicate (-13) True
[]
```

How would we implement this function recursively? How would describe it in three steps above?

1. **Base case**: the cases we already know how to generate are the cases where the length
   of the list is zero (or less) - we just return an empty list.
2. **Reduce**: while we might not know how to generate a list of size `N` (where `N` is positive),
   if we knew the solution for `N-1` we could:
3. **Mitigate**: Add another element to the solution for `N-1` using the `:` (cons) operator.

---

Try to write this in Haskell!

<details>
<summary>Solution</summary>

```haskell
replicate :: Int -> a -> [a]
replicate n x =
  if n <= 0    -- recognizing the base case
    then
      []       -- the solution for the base case
    else
        x : replicate (n - 1) x
  --   ---  -------------------
  --    ^           ^
  --    |           |
  --    |           +-------- reduction
  --    |
  --    +--- mitigation
```

</details>

---

### Mutual recursion

When solving functions recursively we usually call the same function again,
but that doesn't have to be the case. It is possible to reduce our problem
to something simpler that requires an answer from a different function.
If, in turn, that function will (or another function in that call chain)
call our function again, we have a **mutual recursive** solution.

For example, let's write two functions, one that checks whether a natural number
is even or not, and one that checks whether a number is odd or not
only by decrementing it.

```haskell
even :: Int -> Bool

odd :: Int -> Bool
```

Let's start with `even`, how should we solve this recursively?

1. **Base case**: We know the answer for `0` - it is `True`.
2. **Reduction**: We might not know the answer for a general `N`, but we could check whether `N - 1` is odd,
3. **Mitigation**: if `N - 1` is odd, then `N` is even! if it isn't odd, then `N` isn't even.

What about `odd`?

1. **Base case**: We know the answer for `0` - it is `False`.
2. **Reduction**: We might not know the answer for a general `N`, but we could check whether `N - 1` is even,
3. **Mitigation**: if `N - 1` is even, then `N` is odd! if it isn't even, then `N` isn't odd.

---

Try writing this in Haskell!

<details>
<summary>Solution</summary>

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

## Partial functions

Because we didn't handle the negative numbers cases in the example above,
our functions will loop forever when a negative value is passed as input.
A function that does not return a result for some value
(either by not terminating or by throwing an error) is called **a partial function**
(because it only returns a result for a part of the possible inputs).

Partial functions are generally considered **bad practice** because they can have
undesired behaviour at runtime (a runtime exception or an infinite loop),
so we want to **avoid using** partial functions
as well as **avoid writing** partial functions.

The best way to avoid writing partial functions is by covering all inputs!
In the situation above, it is definitely possible to handle negative numbers
as well, so we should do that! Or, instead, we could require that our functions
accept a `Natural` instead of an `Int`, and then the type system would've stopped
us from using these functions with values that we did not handle.

There are cases where we can't possibly cover all inputs, in these cases it is important
to re-examine the code and see if we could further restrict the inputs using types to
mitigate these issues.

For example, the `head :: [a] -> a` function from `Prelude` promises
to return the first element (the head) of a list, but we know that lists
could possibly be empty, so how can this function deliver on its promise?

Unfortunately, it can't. But there exists a different function that can:
`head :: NonEmpty a -> a` from the
[`Data.List.NonEmpty`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List-NonEmpty.html)
module! The trick here is that this other `head` does not take a general list
as input, it takes a different type entirely, one that promises to have
at least one element, and therefore can deliver on its promise!

We could also potentially use smart constructors with `newtype` and enforce some sort
of restrictions in the type system, as we saw in earlier chapters,
But this solution can sometimes be less ergonomic to use.

An alternative approach is to use `data` types to encode the absence of a proper result,
for example, using `Maybe`, as we'll see in a future chapter.

Make sure the functions you write return a result for every input,
either by constraining the input using types, or by encoding the absence of a result using
types.

## Parsing markup?

Let's get back to the task at hand.

As stated previously, our strategy for parsing the markup text is:

1. Split the string to a list where each element is a separate line
   (which we can do with [`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)), and
2. Go over the list line by line and process it, remembering
   information from previous lines if necessary

Remember that we want to start by ignoring all of the markup syntax
and just group lines together into paragraphs (paragraphs are separated by an empty line),
and iteratively add new features later in the chapter:

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

Things to note:

1. We pass a list that contains the currently grouped paragraph (paragraphs are separated by an empty line)
2. Because of laziness, `paragraph` is not computed until it's needed, so we don't have to worry about
   the performance implications in the case that we are still grouping lines
3. Why do we reverse `currentParagraph`? (See point (6))
4. We saw case expressions used to deconstruct `newtype`s and `Char`s,
   but we can also pattern match on lists and other ADTs as well!
   In this case we match against two patterns, an empty list (`[]`),
   and a "cons cell" - a list with at least one element (`currentLine : rest`).
   In the body of the "cons" pattern, we bind the first element to the name `currentLine`,
   and the rest of the elements to the name `rest`.

   We will talk about how all of this works really soon!

5. When we run into an empty line we add the accumulated paragraph to the resulting list (A `Document` is a list of structures) and start the function again with the rest of the input.
6. We pass the new lines to be grouped in a paragraph **in reverse order** because of
   performance characteristics - because of the nature of singly-linked lists,
   prepending an element is fast, and appending is slow. Prepending only requires
   us to create a new cons (`:`) cell to hold a pointer to the value and a pointer to the list,
   but appending requires us to traverse the list to its end and rebuild the cons cells -
   the last one will contain the last value of the list and a pointer to the list to append,
   the next will contain the value before the last value of the list and a pointer to the
   list which contains the last element and the appended list, and so on.

This code above will group together paragraphs in a structure, but how do we view our result?
In the next chapter we will take a short detour and talk about type classes, and how
they can help us in this scenario.
