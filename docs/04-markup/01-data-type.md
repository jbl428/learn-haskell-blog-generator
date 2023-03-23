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

   We even have special syntax for updating specific fields in a record. Of course,
   we do not update records in place - we generate a new value instead.

   ```haskell
   ghci> gil = Person { name = "Gil", age = 32 }
   ghci> age (gil { age = 33 })
   33
   ghci> age gil
   32
   ```

   Unfortunately, having specialized functions for each field also means that if we
   defined a different data type with the field `age`, the functions which GHC needs
   to generate will clash.

   The easiest way to solve this is to give fields unique names, for example
   by adding a prefix:

   ```haskell
   data Person
     = Person
       { pName :: String
       , pAge :: Int
       }
   ```

   Another way is by using extensions to the Haskell language, which we will cover
   in later chapters.

3. Tuple

   ```haskell
   data Tuple a b
     = Tuple a b
   ```

   This is pretty similar to `Person`, but we can plug any type we want
   for this definition. For example:

   ```haskell
   Tuple "Clicked" True :: Tuple String Bool

   Tuple 'a' 'z' :: Tuple Char Char
   ```

   This type has special syntax in Haskell:

   ```haskell
   ("Clicked", True) :: (String, Bool)

   ('a', 'z') :: (Char, Char)
   ```

   This `Tuple` definition is polymorphic, we define the structure but are able to
   plug different types into the structure to get concrete types. You can think of `Tuple`
   as a _template_ for a data type waiting to be filled, or as a **function** waiting
   for types as input in order to return a data type. We can even take a look at the "type"
   signature of `Tuple` in `ghci` using the `:kind` command.

   ```haskell
   ghci> data Tuple a b = Tuple a b
   ghci> :kind Tuple
   Tuple :: * -> * -> *
   ```

   > #### Quick detour: Kinds
   >
   > The `:kind` command is called as such because the "type" of a type is called a **kind**.
   > Kinds can be one of two things, either a `*` which means a saturated (or concrete) type,
   > such as `Int` or `Person`, or an `->` of two kinds, which is, as you might have guessed,
   > a type function, taking kind and returning a kind.
   >
   > Note that only types that have the kind `*` can have values. So for example while `Tuple Int`
   > is a valid Haskell concept that has the _kind_ `* -> *`, and we can write code that will
   > work "generically" for all types that have a certain kind (e.g. `* -> *`), we cannot
   > construct a value that will have the kind `* -> *`. All values have types, and all
   > types that have values have the kind `*`.
   >
   > We will talk more about kinds later, for now let's focus on types!

4. Either

   ```haskell
   data Either a b
     = Left a
     | Right b
   ```

   Similar to Tuple but instead of having only one constructor, we have
   two. This means that we can choose which side we want. Here are a
   couple of values of type `Either String Int`:

   ```haskell
   Left "Hello"

   Right 17
   ```

   This type is useful for modeling errors. Either we succeeded and got
   what we wanted (The `Right` constructor with the value), or we didn't
   and got an error instead (The `Left` constructor with a string or a
   custom error type).

In our program we use `data` types to model the different kinds of content types
in our markup language. We tag each structure using the data constructor
and provide the rest of the information (the paragraph text, the list items, etc.)
in the `<types>` section of the data declaration for each constructor:

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

Note: `Natural` is defined in the `base` package but not exported from `Prelude`.
Find out which module to import `Natural` by using [Hoogle](https://hoogle.haskell.org).

---

### Exercises

Represent the following markup documents as values of `Document`:

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

Solutions:

<details>
  <summary>Solution 1</summary>

```haskell
example1 :: Document
example1 =
  [ Paragraph "Hello, world!"
  ]
```

</details>

<details>
  <summary>Solution 2</summary>

```haskell
example2 :: Document
example2 =
  [ Heading 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]
```

</details>

<details>
  <summary>Solution 3</summary>

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
  <summary>Solution 4</summary>

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

Add a new module named `Markup` and add the data type definition to it.
Note that in this case we _do_ want to export the constructors of `Structure`.

<details>
  <summary>Solution</summary>

```haskell
-- Markup.hs

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

## Translating directly?

You might ask "Why do we even need to represent the markup as a type?
Why don't we convert it into HTML as soon as we parse it
instead?". That's a good question and a valid strategy. The reason we
first represent it as a Haskell type is for flexibility and modularity.

If the parsing code is coupled with HTML generation, we lose the
ability to pre-process the markup document. For example we might want
to take only a small part of the document (for summary) and present
it, or create a table of content from headings. Or maybe we'd like to
add other targets and not just HTML - maybe markdown format or a GUI reader?

Parsing to an "abstract data type" (ADT) representation (one that does
not contain the details of the language, for example '#' for
ordered lists) gives us the freedom to do so much more than just
conversion to HTML that it's usually worth it in my opinion unless you
really need to optimize the process.
