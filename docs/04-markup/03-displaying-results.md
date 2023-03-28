# 파싱 결과 보여주기 (타입 클래스)

우리가 만든 `Document` 타입을 출력하는 방법을 알아봅시다.
출력하기 위한 몇 가지 방법이 있습니다:

1. 우리가 직접 `Document -> String` 타입의 함수를 작성합니다
2. 하스켈이 자동으로 만들도록 합니다

하스켈은 타입을 `String`으로 바꿔주는 `show`라는 *타입 클래스* 함수의 구현을 자동으로 생성해주는 메커니즘을 제공합니다.

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
`show`는 `Show` 타입 클래스의 어떠한 *인스턴스* 대해서도 작동할 수 있는 오버로드된 함수입니다.
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

## Laws

Type classes often come with "rules" or "laws" that instances should satisfy,
the purpose of these laws is to provide _predictable behaviour_ across
instances, so that when we run into a new instance we can be confident
that it will behave in an expected way, and we can write code
that works generically for all instances of a type class while expecting
them to adhere to these rules.

As an example, let's look at the `Semigroup` type class:

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

This type class provides a common interface for types with an operation `<>`
that can combine two values into one in some way.

This type class also mentions that this `<>` operation should be associative,
meaning that these two sides should evaluate to the same result:

```
x <> (y <> z) = (x <> y) <> z
```

An example of a lawful instance of `Semigroup` is lists with the append operation (`++`):

```haskell
instance Semigroup [a] where
  (<>) = (++)
```

Unfortunately the Haskell type system cannot "prove" that instances
satisfy these laws, but as a community we often shun unlawful instances.

Many data types (together with their respective operations) can
form a `Semigroup`, and instances
don't even have to look similar or have a common analogy/metaphor
(and this is true for many other type classes as well).

**Type classes are often just _interfaces_ with _laws_** (or expected behaviours if you will).
Approaching them with this mindset can be very liberating!

To put it differently, **type classes can be used to create abstractions** -
interfaces with laws/expected behaviours where we don't actually care about the
concrete details of the underlying type, just that it _implements a certain
API and behaves in a certain way_.

Regarding `Semigroup`, we have [previously](../03-html/04-safer-construction.md#appending-htmlstructure)
created a function that looks like `<>` for our `Html` EDSL!
We can add a `Semigroup` instance for our `Structure` data type
and have a nicer API!

---

Exercise: Please do this and remove the `append_` function from the API.

<details>
  <summary>Solution</summary>

Replace this:

```haskell
append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)
```

With this:

```haskell
instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)
```

And remove the export of `append_` in `Html.hs`. You won't need to further export anything
as type class instances are exported automatically.

You will also need to replace the usage of `append_` with `<>` in `hello.hs`.

</details>

---
