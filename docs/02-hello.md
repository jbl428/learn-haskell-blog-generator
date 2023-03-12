# Hello, world!

이번 장에서는 간단한 HTML "hello world" 프로그램을 만들고 Haskell 툴체인을 사용하여 컴파일하고 실행해 보겠습니다.

> 아직 Haskell 툴체인을 설치하지 않았다면
> [haskell.org/downloads](https://haskell.org/downloads)에 방문하여
> Haskell 툴체인을 다운로드하고 설치하는 과정을 진행하세요.

## 하스켈 소스 파일

하스켈 소스 파일은 여러 정의들로 구성됩니다.

가장 일반적인 형태의 정의는 다음과 같습니다:

```haskell
<이름> = <표현식>
```

다음과 같은 제약사항이 있습니다:

1. 이름은 소문자로 시작해야 합니다
2. 파일 내에서 이름은 한 번만 사용할 수 있습니다

어떤 파일이 `main`이라는 정의를 가지고 있다면, 해당 파일은 실행 가능한 파일로 간주됩니다.
또한 `main`의 표현식은 프로그램의 시작점이 됩니다.

첫 번째 하스켈 소스코드 파일을 작성해 보겠습니다. `hello.hs`라는 이름의 새로운 파일을 만들고 다음과 같이 작성하세요:

```haskell
main = putStrLn "<html><body>Hello, world!</body></html>"
```

위 코드에서 `main`이라는 이름을 정의하고 `putStrLn "<html><body>Hello, world!</body></html>"`라는 표현식을 바인딩했습니다.

`main`의 정의는 `"<html><body>Hello, world!</body></html>"` 값을 입력으로 `putStrLn` 함수를 호출한다는 의미입니다.
`putStrLn` 함수는 하나의 문자열을 입력으로 받고 표준 출력으로 해당 문자열을 출력합니다.

:::caution
하스켈에서 함수에 인자를 전달할 때 괄호를 사용하지 않아도 됩니다.
:::

이 프로그램을 실행하면 다음과 같은 텍스트가 화면에 출력됩니다:

```
<html><body>Hello, world!</body></html>
```

주의할 점은 `main =` 부분을 생략하고 단순히 `putStrLn "<html><body>Hello, world!</body></html>"`만 작성할 수는 없습니다.
왜나하면 이는 정의가 아니기 때문입니다.
파이썬이나 OCaml과 같은 언어에서는 허용되지만 하스켈이나 C와 같은 언어에서는 허용되지 않습니다.

## 프로그램 컴파일하기

프로그램을 실행하기 위해, `ghc`라는 커맨드라인 프로그램을 사용하여 `hello.hs`라는 파일을 컴파일할 수 있습니다:

```sh
> ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

`hello.hs` 파일을 `ghc`에 전달하면 다음과 같은 파일들이 생성됩니다:

1. `hello.o` - 오브젝트 파일
2. `hello.hi` - 하스켈 인터페이스 파일
3. `hello` - 네이티브 실행 파일

컴파일 이후에, `hello`라는 파일을 실행할 수 있습니다:

```sh
> ./hello
<html><body>Hello, world!</body></html>
```

## 인터프리터로 실행하기

다른 방법으로, 컴파일과 아티팩트 파일 생성 단계를 생략하고 `runghc`라는 커맨드라인 프로그램을 사용하여 소스 파일을 직접 실행할 수 있습니다:

```sh
> runghc hello.hs
<html><body>Hello, world!</body></html>
```

프로그램 실행 결과를 파일로 저장하고 Firefox 브라우저를 사용하여 해당 파일을 열 수도 있습니다.

```sh
> runghc hello.hs > hello.html
> firefox hello.html
```

위 명령어는 Firefox를 실행하고 `Hello, world!`라는 텍스트가 있는 웹 페이지를 표시합니다.

이번 튜토리얼에서는 `runghc`를 사용하는 것을 추천합니다. 컴파일 하는것이 더 빠른 프로그램을 생성하지만,
프로그램을 개발하고 자주 변경하는 동안에는 인터프리터를 사용하는 것이 피드백을 더 빠르게 받을 수 있기 때문입니다.

> 하스켈 툴체인에 대해 더 자세히 알고 싶다면 [이 글](https://gilmi.me/blog/post/2021/08/14/hs-core-tools)을 참고하세요.
> 하지만 지금 당장은 위 내용만 알아도 충분합니다.

## 더 많은 바인딩

`putStrLn` 함수에 직접 문자열을 전달하는 대신, HTML 문자열을 정의하는 새로운 이름을 만들어서 전달할 수도 있습니다.
이전에 만든 `hello.hs` 파일을 다음과 같이 수정하세요:

```haskell
main = putStrLn myhtml

myhtml = "<html><body>Hello, world!</body></html>"
```

**참고**: 바인딩의 정의 순서는 중요하지 않습니다.
