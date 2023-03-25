# 하스켈 데이터 타입으로 마크업 언어 표현하기

하스켈(그리고 다른 ML-계열의 언어들)과 대부분의 주류 언어들 간의 가장 큰 차이점은 데이터를 정확하고 간결하게 표현할 수 있다는 점입니다.

그렇다면 하스켈을 사용하여 마크업 언어를 어떻게 표현할 수 있을까요?

이전 HTML 생성 라이브러리에서는 HTML 문서, 구조, 제목을 구분하기 위해 `newtype`을 사용했습니다.
하지만 데이터를 분석하지 않는 이상, 문단과 제목과 같은 다른 구조를 구분할 필요는 없었습니다.

이러한 경우, 동일한 구조의 목록을 가지면서 각 구조에는 몇 가지 특정 옵션(문단, 제목, 목록 등)을 가지는 형태를 생각할 수 있습니다.
우리는 각 구조가 어떤 항목인지만 알면 동일한 HTML로 쉽게 변환할 수 있습니다.

이를 위해 `data` 정의를 사용할 수 있습니다.
`data`는 여러 타입을 그룹화하여 대체 구조를 가지도록 하는 사용자 정의 타입을 제공합니다.
`data`는 `struct`와 `enum`의 조합으로 생각할 수 있습니다.

`data` 선언은 다음과 같이 생겼습니다:

```haskell
data <타입 이름> <타입 매개변수>
  = <타입 생성자1> <타입>
  | <타입 생성자2> <타입>
  | ...
```

이는 `newtype`과 매우 유사해 보이지만, 두 가지 중요한 차이점이 있습니다:

1. `<types>` 부분에서는 많은 타입(예: `Int`, `String`, `Bool`)을 작성할 수 있습니다.
   `newtype`에서는 하나만 작성할 수 있습니다.
2. `|`를 사용하여 여러 구조를 가질 수 있습니다.
   `newtype`은 오직 하나의 구조만 가질 수 있습니다.

이는 `newtype`이 **타입 안전한 별칭**을 제공하기 위해 사용되고, `data`는 새로운 **복합** 타입을 만들기 위해 사용되기 때문입니다.

`data`를 사용하여 다음과 같은 몇 가지 예를 살펴보겠습니다:

1. Bool

   ```haskell
   data Bool
     = True
     | False
   ```

   `True`와 `False`만 가질 수 있는 `Bool`이라는 새로운 타입을 만들었습니다.

   이 경우 *생성자* 목록만 가지고 있을뿐 추가적인 값은 없습니다.
   이는 다른 언어의 열거형(enum)과 유사합니다.

2. Person

   ```haskell
   data Person
     = Person String Int -- 첫 번째 인자는 이름, 두 번째 인자는 나이
   ```

   이번에는 `Person`이라는 새로운 타입을 만들었습니다.
   이 타입의 값은 다음과 같이 생겼습니다:

   ```
   Person <문자열> <정수>
   ```

   예를 들면:

   ```haskell
   Person "Gil" 32
   ```

   이 경우에는 다른 생성자는 없고 여러 타입을 *함성*하는 생성자만 있습니다.
   이는 다른 언어에서 구조체(struct)와 유사하지만, 각 필드를 이름으로 구별하는 대신 위치로 구분합니다.

   그 대신, 하스켈은 **records**라고 불리는 필드 이름을 지정하는 **문법적 설탕**를 제공합니다.
   위 정의는 다음과 같이 작성할 수 있습니다:

   ```haskell
   data Person
     = Person
       { name :: String
       , age :: Int
       }
   ```

   위 타입의 값은 이전과 동일하게 작성할 수 있거나,

   ```haskell
   Person "Gil" 32
   ```

   다음과 같이 작성할 수 있습니다:

   ```haskell
   Person { name = "Gil", age = 32 }
   ```

   하스켈은 복합 타입에서 필드를 추출하기 위한 용도의 함수또한 생성합니다:

   ```haskell
   name :: Person -> String
   age :: Person -> Int
   ```

   다음과 같이 사용할 수 있습니다:

   ```haskell
   ghci> age (Person { name = "Gil", age = 32 })
   32
   ```

   record에서 특정 필드를 업데이트하기 위한 특별한 문법도 제공합니다.
   당연하게도, 기존 값을 업데이트하는 대신 새로운 값을 생성합니다.

   ```haskell
   ghci> gil = Person { name = "Gil", age = 32 }
   ghci> age (gil { age = 33 })
   33
   ghci> age gil
   32
   ```

   안타깝게도, 각 필드에 대해 특별한 함수를 생성하는 것은 다른 데이터 타입에 `age`라는 필드를 정의할 경우 GHC가 함수를 생성할 때 이름이 충돌할 수 있다는 것을 의미합니다.

   이러한 문제를 해결하는 가장 쉬운 방법은 필드에 고유한 이름을 부여하는 것입니다.
   예를 들어 다음과 같이 접두사를 추가할 수 있습니다:

   ```haskell
   data Person
     = Person
       { pName :: String
       , pAge :: Int
       }
   ```

   또 다른 방법은 하스켈 언어의 확장을 사용하는 것입니다.
   이는 이후 장에서 다루겠습니다.

3. Tuple

   ```haskell
   data Tuple a b
     = Tuple a b
   ```

   `Person`과 비슷하지만, 이번에는 어떠한 타입도 넣을 수 있는 `Tuple`이라는 새로운 타입을 만들었습니다. 예를 들면:

   ```haskell
   Tuple "Clicked" True :: Tuple String Bool

   Tuple 'a' 'z' :: Tuple Char Char
   ```

   하스켈에서는 다음과 같은 특별한 문법을 제공합니다:

   ```haskell
   ("Clicked", True) :: (String, Bool)

   ('a', 'z') :: (Char, Char)
   ```

   `Tuple`의 정의는 다형적이기에, 구체적인 타입을 만들 때 매번 다른 타입을 넣을 수 있습니다.
   이는 `Tuple`을 이후에 값을 채워지기를 기다리는 **템플릿**이라고 생각할 수 있습니다.
   또는 데이터 타입을 반환하기 위해 타입을 입력으로 받는 **함수**라고 생각할 수도 있습니다.
   `ghci`에서 `:kind` 명령을 사용하여 `Tuple`의 "타입"을 살펴볼 수 있습니다.

   ```haskell
   ghci> data Tuple a b = Tuple a b
   ghci> :kind Tuple
   Tuple :: * -> * -> *
   ```

   :::note Kinds
   타입의 "종류(type)"를 **kind**라고 부르기 때문에 `:kind` 명령어라 이름을 붙였습니다.
   kind에는 두 가지 종류가 있습니다. 하나는 `*`로 `Int`나 `Person`과 같은 **포화된(또는 구체적인)** 타입을 의미하고,
   다른 하나는 `->`로 kind를 받아 kind를 반환하는 **타입 함수**를 의미합니다.

   kind가 `*`인 타입만 값을 가질 수 있다는 것을 기억하세요.
   예를 들어 `Tuple Int`는 *kind*가 `* -> *`이기 때문에 하스켈의 개념으로는 유효하며,
   모든 `* -> *` kind를 가진 타입에 대해 일반적으로 작동하는 코드를 작성할 수 있지만,
   `* -> *` kind를 가진 값을 만들 수는 없습니다.
   모든 값은 타입을 가지며, 값을 가지고 있는 타입의 kind는 `*`입니다.

   kind에 대한 더 자세한 내용은 이후에 살펴볼 예정이며, 지금은 타입에 집중하겠습니다!
   :::

4. Either

   ```haskell
   data Either a b
     = Left a
     | Right b
   ```

   Tuple과 유사하지만 두 개의 생성자를 가지고 있습니다.
   이는 어느 쪽을 선택할지 선택할 수 있다는 것을 의미합니다.
   다음은 몇 가지 `Either String Int` 타입의 값의 예시입니다:

   ```haskell
   Left "Hello"

   Right 17
   ```

   이 타입은 에러를 모델링하는 데 유용합니다.
   성공했다면 원하는 값을 얻고 (`Right` 생성자와 값), 그렇지 않다면 에러를 얻습니다. (`Left` 생성자와 문자열 또는 사용자 정의 에러 타입).

우리 프로그램에서, 마크업 언어에 쓰이는 다양한 본문의 유형을 모델링하기 위해 `data` 타입을 사용합니다.
각 구조체에 태그를 달고, 각 생성자의 `<types>` 섹션에 나머지 정보(문단 텍스트, 리스트 항목 등)를 제공합니다:

```haskell
type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

:::note
`Natural`은 `base` 패키지에 정의되어 있지만 `Prelude`에서는 내보내지 않습니다.
`Natural`을 어떤 모듈에서 가져올 수 있는지 알아내려면 [Hoogle](https://hoogle.haskell.org)을 사용하세요.
:::

---

### 연습문제

다음 마크업 문서를 `Document` 값으로 표현하세요:

1. ```org
   Hello, world!
   ```

2. ```org
   * Welcome

   To this tutorial about Haskell.
   ```

3. ```org
   Remember that multiple lines with no separation
   are grouped together to a single paragraph
   but list items remain separate.

   # Item 1 of a list
   # Item 2 of the same list
   ```

4. ```org
   * Compiling programs with ghc

   Running ghc invokes the Glasgow Haskell Compiler (GHC),
   and can be used to compile Haskell modules and programs into native
   executables and libraries.

   Create a new Haskell source file named hello.hs, and write
   the following code in it:

   > main = putStrLn "Hello, Haskell!"

   Now, we can compile the program by invoking ghc with the file name:

   > ➜ ghc hello.hs
   > [1 of 1] Compiling Main             ( hello.hs, hello.o )
   > Linking hello ...

   GHC created the following files:

   - hello.hi - Haskell interface file
   - hello.o - Object file, the output of the compiler before linking
   - hello (or hello.exe on Microsoft Windows) - A native runnable executable.

   GHC will produce an executable when the source file satisfies both conditions:

   # Defines the main function in the source file
   # Defines the module name to be Main, or does not have a module declaration

   Otherwise, it will only produce the .o and .hi files.
   ```

정답:

<details>
  <summary>정답 1</summary>

```haskell
example1 :: Document
example1 =
  [ Paragraph "Hello, world!"
  ]
```

</details>

<details>
  <summary>정답 2</summary>

```haskell
example2 :: Document
example2 =
  [ Heading 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]
```

</details>

<details>
  <summary>정답 3</summary>

```haskell
example3 :: Document
example3 =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]
```

</details>

<details>
  <summary>정답 4</summary>

```haskell
example4 :: Document
example4 =
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock
    [ "main = putStrLn \"Hello, Haskell!\""
    ]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock
    [ "➜ ghc hello.hs"
    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]
```

</details>

`Markup` 모듈을 만들고 `data` 타입 정의를 추가하세요.
`Structure`의 생성자들을 내보내야 하는 것에 주의하세요.

<details>
  <summary>정답</summary>

```haskell title=Markup.hs
module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

</details>

---

## 바로 변환하면 안되나요?

아마 다음과 같은 의문이 들 수 있습니다.

- 왜 마크업을 타입으로 표현해야 하나요?
- 파싱할 때 바로 HTML로 변환하면 어떨까요?

좋은 질문이고 유효한 전략입니다.
우리가 마크업을 먼저 하스켈 타입으로 표현하는 이유는 유연성과 모듈성 때문입니다.

만약 해석 작업이 HTML 생성과 결합되어 있다면, 마크업 문서를 사전 처리할 수 있는 기회를 잃게 됩니다.
예를 들어 요약을 위해 문서의 일부분만 가져오거나, 제목으로 목차를 만들기 어렵게됩니다.
또는 단순히 HTML이 아닌 마크다운 형식이나 GUI 리더기 같은 다른 형식으로 변환하고 싶을 수도 있습니다.

해석을 "추상 데이터 타입" (ADT) 표현으로(예를 들어 순서 목록을 위한 '#' 같은 언어의 세부 사항을 포함하지 않는)하면 HTML 변환 외에도 많은 일을 할 수 있습니다.
따라서 최적화가 정말로 필요하지 않는 한 ADT로 변환하는 것이 좋다고 생각합니다.
