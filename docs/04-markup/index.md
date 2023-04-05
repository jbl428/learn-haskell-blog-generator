# 사용자 정의 마크업 언어

이번 장에서는 간단한 마크업 언어를 정의하고, 이 언어로 작성된 문서를 하스켈 데이터 구조로 만드는 방법을 알아보겠습니다.

이번에 만들 마크업 언어는 다음과 같은 기능을 가질 것입니다:

- 제목: `*` 문자를 앞에 붙입니다.
- 단락: 빈 줄이 없는 그룹의 라인들
- 순서 없는 목록: 각 줄이 `- `로 시작합니다.
- 순서 있는 목록: 각 줄이 `# `로 시작합니다.
- 코드 블록: 각 줄이 `> `로 시작합니다.

다음 예제 문서를 보겠습니다:

```org
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

이 문서는 다음 HTML로 변환됩니다:

```html
<h1>Compiling programs with ghc</h1>

<p>
  Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to
  compile Haskell modules and programs into native executables and libraries.
</p>

<p>
  Create a new Haskell source file named hello.hs, and write the following code
  in it:
</p>

<pre>
main = putStrLn "Hello, Haskell!"
</pre>

<p>Now, we can compile the program by invoking ghc with the file name:</p>

<pre>
➜ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
</pre>

<p>GHC created the following files:</p>

<ul>
  <li>hello.hi - Haskell interface file</li>
  <li>hello.o - Object file, the output of the compiler before linking</li>
  <li>
    hello (or hello.exe on Microsoft Windows) - A native runnable executable.
  </li>
</ul>

<p>
  GHC will produce an executable when the source file satisfies both conditions:
</p>

<ol>
  <li>Defines the main function in the source file</li>
  <li>
    Defines the module name to be Main, or does not have a module declaration
  </li>
</ol>

<p>Otherwise, it will only produce the .o and .hi files.</p>
```
