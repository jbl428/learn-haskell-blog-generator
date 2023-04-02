# 파싱 결과 보여주기 (타입 클래스)

우리가 만든 `Document` 타입을 출력하는 방법을 알아봅시다.
출력하기 위한 몇 가지 방법이 있습니다:

1. 우리가 직접 `Document -> String` 타입의 함수를 작성합니다
2. 하스켈이 자동으로 만들도록 합니다

하스켈은 타입을 `String`으로 바꿔주는 `show`라는 _타입 클래스_ 함수의 구현을 자동으로 생성해주는 메커니즘을 제공합니다.

`show` 함수의 타입은 다음과 같습니다:
The type of the function `show` looks like this:

```haskell
show :: Show a => a -> String
```

이 함수는 지금까지 우리가 봤던 것과는 다릅니다.
`::`와 `=>` 사이에 있는 것은 타입 `a`에 대한 **타입 클래스 제약(type class constraint)**이라고 부릅니다.
이 시그니처의 의미는 `show` 함수는 `Show` 타입 클래스의 멤버인 어떤 타입에 대해서도 작동할 수 있다는 것입니다.

타입클래스는 하스켈에서 타입에 공통된 인터페이스를 정의하는 기능을 제공합니다.
하스켈의 표준 라이브러리는 `Show` 타입 클래스를 다음과 같이 정의합니다
(간단하게 생략한 버전이지만 지금은 이정도로 충분합니다):

```haskell
class Show a where
  show :: a -> String
```

타입 클래스 선언은 하스켈 타입에 대한 공통된 인터페이스를 설명합니다.
`show`는 `Show` 타입 클래스의 어떠한 _인스턴스_ 대해서도 작동할 수 있는 오버로드된 함수입니다.
우리는 다음과 같이 타입 클래스의 인스턴스를 수동으로 정의할 수 있습니다:

```haskell
instance Show Bool where
  show x =
    case x of
      True -> "True"
      False -> "False"
```

인스턴스를 정의하는 것은 특정 타입의 인터페이스를 구현하는 것을 의미합니다.
`show` 함수에 데이터 타입을 전달하면 컴파일러는 타입의 `Show` 인스턴스를 찾고, 인스턴스 선언에서 제공된 구현을 사용합니다.

```haskell
ghci> show True
"True"
ghci> show 187
"187"
ghci> show "Hello"
"\"Hello\""
```

위에서 볼 수 있듯이 `show` 함수는 값을 텍스트로 변환합니다.
그래서 `"Hello"`에는 따옴표가 포함되어 있습니다.
`Show` 타입 클래스는 주로 디버깅 목적으로 사용합니다.

## 인스턴스 자동구현

몇 가지 타입 클래스는 인스턴스를 자동으로 생성할 수 있습니다.
다행히 `Show`도 그 중 하나입니다.

만약 우리가 정의한 데이터 타입의 모든 타입이 `Show` 인스턴스를 이미 구현하고 있다면,
데이터 정의 끝에 `deriving Show`를 추가하면 `Show` 인스턴스를 *자동으로 생성*할 수 있습니다.

```haskell
data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show
```

이제 `Show`인스턴스를 구현한 어떠한 타입에 대해서도 `show :: Show a => a -> String` 함수를 사용할 수 있습니다.
예를 들어 `print` 함수를 사용할 수 있습니다:

```haskell
print :: Show a => a -> IO ()
print = putStrLn . show
```

`print` 함수는 `show` 함수를 사용하여 값을 `String`으로 변환하고, 표준 출력으로 출력합니다.

리스트 또한 요소가 `Show` 인스턴스를 가졌다면 `Show` 인스턴스를 구현합니다.
그래서 `Document`는 `[Structure]`의 별칭이기 때문에 `Show` 인스턴스를 가집니다.
한 번 시도해보세요!

하스켈러들이 자주 사용하는 다양한 타입 클래스가 있습니다.
그 중에 동등성을 위한 `Eq`와 정렬을 위한 `Ord`도 있습니다.
이 두 타입 클래스 역시 인스턴스를 자동으로 생성할 수 있습니다.

## 법칙

타입 클래스에는 인스턴스가 준수해야 하는 "법칙"이나 "규칙"이 있습니다.
이러한 법칙의 목적은 인스턴스 간에 *예측 가능한 동작*을 제공하는 것입니다.
따라서 새로운 인스턴스를 접할 때, 이것이 예상대로 동작할 것이라고 확신할 수 있으며,
이러한 법칙을 준수하는 모든 인스턴스에 대해 일반적으로 작동하는 코드를 작성할 수 있습니다.

예들 들어, `Semigroup` 타입 클래스를 살펴보겠습니다:

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

이 타입 클래스는 두 값을 어떠한 방식으로 결합해 하나의 값을 만들 수 있는 연산자 `<>`를 제공하는 타입에 대한 공통된 인터페이스를 제공합니다.

이 타입 클래스는 또한 `<>` 연산자가 결합법칙을 만족해야 한다는 것을 의미합니다.
즉, 아래 등식이 성립해야 합니다.

```
x <> (y <> z) = (x <> y) <> z
```

`Semigroup`을 만족하는 인스턴스의 예로 리스트와 `++` 연산자가 있습니다:

```haskell
instance Semigroup [a] where
  (<>) = (++)
```

아쉽게도 하스켈 타입 시스템은 이러한 법칙을 "증명"할 수 없기에, 법칙을 만족하지 않는 인스턴스 사용하지 않는것이 좋습니다.

많은 데이터 타입(그리고 각 연산자)은 `Semigroup`을 만족하며, 인스턴스들은 비슷해 보이거나 공통적인 유사성/비유를 가져야 할 필요가 없습니다.
(이는 많은 다른 타입 클래스에도 해당됩니다.)

**타입 클래스는 종종 _법칙_(또는 예상 동작)이 있는 *인터페이스*에 불과합니다.**
이러한 관점으로 접근하면 매우 자유로울 수 있습니다!

다르게 말하면, **타입 클래스는 추상화를 만드는 데 사용할 수 있습니다.**
주어진 타입의 구체적인 세부 사항에 대해 신경 쓰지 않고, 단지 _API를 구현하고 특정 방식으로 동작한다는 것을 보장하는_ 인터페이스입니다.

사실 [이전 장](../03-html/04-safer-construction.md#appending-htmlstructure)에서 `Html` EDSL을 위한 `<>` 연산자를 만들었습니다!
`Structure` 타입에 `Semigroup` 인스턴스를 추가하면 더 나은 API를 가질 수 있습니다!

---

연습문제: `append_` 함수를 제거하고 `Semigroup` 인스턴스를 추가하세요.

<details>
  <summary>정답</summary>

다음 코드를:

```haskell
append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)
```

아래처럼 변경합니다:

```haskell
instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)
```

그리고 `Html.hs`에서 `append_`를 제거합니다.
타입 클래스는 자동으로 내보내지기 때문에 따로 내보내지 않아도 됩니다.

`hello.hs`에서 `append_`를 `<>`로 바꾸는 작업도 필요합니다.

</details>

---
