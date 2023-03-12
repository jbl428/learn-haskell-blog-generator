# 유연한 HTML 콘텐츠 (함수)

매번 모든 HTML과 body 태그를 작성하지 않고 다른 HTML 페이지를 작성할 수 있다면 유용할 것입니다.
함수를 사용하면 이를 가능하게 할 수 있습니다.

함수를 정의하기 위해서는 이전 장에서 본 것과 비슷한 정의를 만들고 `=` 뒤에 인자 이름을 추가하면 됩니다.
따라서 함수 정의는 다음과 같은 형태를 가집니다:

```haskell
<name> <arg1> <arg2> ... <argN> = <expression>
```

인자 이름은 `=` 오른쪽에 있는 `<expression>`에서 사용할 수 있으며 함수 이름은 `<name>`이 됩니다.

페이지 본문을 의미하는 문자열을 인자로 받는 함수를 정의하겠습니다.
함수는 주어진 문자열의 앞뒤에 `html`과 `body` 태그를 감싸서 반환합니다.
문자열을 연결하기 위해 `<>` 연산자를 사용합니다.

```haskell
wrapHtml content = "<html><body>" <> content <> "</body></html>"
```

`wrapHtml` 함수는 하나의 인자 `content`를 받고, `content` 앞뒤에 `<html><body>`와 `</body></html>`를 붙여서 반환합니다.
하스켈에서는 보통 이름을 `camelCase`로 작성합니다.

이제 이전 장에서 정의한 `myhtml`을 다음과 같이 변경할 수 있습니다:

```haskell
myhtml = wrapHtml "Hello, world!"
```

함수를 호출할 때에는 괄호가 필요하지 않는점을 주목하세요. 함수 호출은 다음과 같은 형태를 가집니다:

```haskell
<name> <arg1> <arg2> ... <argN>
```

하지만, `main = putStrLn myhtml`에서 `myhtml`을 그에 해당하는 표현식으로 교체하고 싶다면 괄호를 사용해야 합니다:

```haskell
main = putStrLn (wrapHtml "Hello, world!")
```

만약 다음과 같이 작성한다면:

```haskell
main = putStrLn wrapHtml "Hello, world!"
```

GHC는 `putStrLn`는 하나의 인자를 필요로 하지만 두 개가 주어졌다는 오류를 발생시킬 것입니다.
왜냐하면 이는 `<name> <arg1> <arg2>`의 형태를 가지기 때문입니다.
즉 `<arg1>`과 `<arg2>`는 `<name>`의 인자로 해석됩니다.

괄호를 사용하면 표현식을 올바른 순서로 묶을 수 있습니다.

> #### 연산자 우선순위와 결합성
>
> `<>`과 같은 연산자는 두 개의 인자를 받는 중위 함수입니다.
> operators (like `<>`) are infix functions which take two arguments - one from each side.
>
> 괄호 없이 동일한 식에서 여러 연산자가 있는 경우, 연산자의 *결합성* (왼쪽 또는 오른쪽)과 *우선순위* (0에서 10 사이의 숫자)에 따라 더 강하게 바인딩하는 연산자가 결정됩니다.
>
> `<>`는 오른쪽 결합성을 갖기 때문에 Haskell은 `<>` 오른쪽에 보이지 않는 괄호를 추가합니다. 예를 들어:
>
> ```haskell
> "<html><body>" <> content <> "</body></html>"
> ```
>
> 는 다음과 같이 해석됩니다:
>
> ```haskell
> "<html><body>" <> (content <> "</body></html>")
> ```
>
> 표현식 `1 + 2 * 3`에서 `+` 연산자는 우선순위 6을 갖고, `*` 연산자는 우선순위 7을 갖기 때문에 `*` 연산자가 더 강하게 바인딩됩니다.
> 따라서 Haskell은 이 표현식을 다음과 같이 해석합니다:
>
> ```haskell
> 1 + (2 * 3)
> ```
>
> 만약 같은 우선순위를 가지지만 다른 결합성을 가진 연산자를 혼용할 때 오류가 발생할 수 있습니다.
> 왜냐하면 Haskell은 이러한 표현식을 어떻게 묶어야 할지 알 수 없기 때문입니다.
> 이 경우 명시적으로 괄호를 추가하여 문제를 해결할 수 있습니다.

---

연습문제:

1. `wrapHtml` 함수의 기능을 두 개의 함수로 분리합니다:
   1. `html` 태그로 감싸는 기능
   2. `body` 태그로 감싸는 기능

   이를 `html_`와 `body_`라고 이름을 붙입니다.
2. `myhtml`을 `html_`과 `body_`를 사용하도록 변경합니다.
3. `<head>` 태그와 `<title>` 태그를 감싸는 두 개의 함수를 추가하고 `head_`와 `title_`라고 이름을 붙입니다.
4. `makeHtml` 함수를 추가합니다. 이 함수는 두 개의 문자열을 인자로 받습니다:
   1. 제목을 의미하는 문자열
   2. 본문을 의미하는 문자열
   
   이 함수는 `html_`, `head_`, `title_`, `body_` 함수를 사용하여 HTML 문자열을 생성합니다.
   
   다음과 같은 입력에 대해:
   
   ```haskell
   makeHtml "My page title" "My page content"
   ```
   
   다음과 같은 결과를 반환해야 합니다:
   
   ```html
   <html><head><title>My page title</title></head><body>My page content</body></html>
   ```
5. `myhtml`을 `html_`과 `body_` 대신 `makeHtml`을 사용하도록 변경합니다.

---

정답:

<details>
  <summary>연습문제 #1 정답</summary>

```haskell
html_ content = "<html>" <> content <> "</html>"

body_ content = "<body>" <> content <> "</body>"
```

</details>

<details>
  <summary>연습문제 #2 정답</summary>

```haskell
myhtml = html_ (body_ "Hello, world!")
```

</details>

<details>
  <summary>연습문제 #3 정답</summary>

```haskell
head_ content = "<head>" <> content <> "</head>"

title_ content = "<title>" <> content <> "</title>"
```

</details>

<details>
  <summary>연습문제 #4 정답</summary>

```haskell
makeHtml title content = html_ (head_ (title_ title) <> body_ content)
```

</details>


<details>
  <summary>연습문제 #5 정답</summary>

```haskell
myhtml = makeHtml "Hello title" "Hello, world!"
```

</details>


<details>
  <summary>최종 프로그램</summary>

```haskell
-- hello.hs

main = putStrLn myhtml

myhtml = makeHtml "Hello title" "Hello, world!"

makeHtml title content = html_ (head_ (title_ title) <> body_ content)

html_ content = "<html>" <> content <> "</html>"
  
body_ content = "<body>" <> content <> "</body>"

head_ content = "<head>" <> content <> "</head>"

title_ content = "<title>" <> content <> "</title>"
```

이제 `hello.hs` 프로그램을 실행하고 출력을 파일로 파이프라인으로 전달하고 브라우저에서 열 수 있습니다:

```sh
runghc hello.hs > hello.html
firefox hello.html
```

이제 `Hello, world!`가 페이지에 표시되고 페이지의 제목이 `Hello title`로 표시됩니다.

</details>


---

## 들여쓰기

하스켈은 어떻게 정의가 완료되었는지 알 수 있을까요?
정답은 하스켈은 들여쓰기를 사용하여 표현식이 어떻게 그룹화되어야 하는지 알 수 있다는 것입니다.

하스켈의 들여쓰기 법칙은 약간 까다로울 수 있지만, 일반적으로 다음과 같습니다:
표현식의 일부가 되어야 하는 코드는 해당 표현식의 시작보다 더 들여쓰기되어야 합니다.

만약 두 표현식이 동일한 들여쓰기를 가지고 있다면, 이들은 서로 독립적이라고 할 수 있습니다.


### 들여쓰기 팁

1. 들여쓰기할 공백의 개수를 선택하고 (2개, 4개 등) 이를 유지하세요.
   탭보다는 공백을 사용하세요.
2. 한 번에 두 번 이상 들여쓰기를 하지 마십시오.
3. 확실하지 않은 경우 필요에 따라 줄을 삭제하고 한 번 들여쓰기합니다.

다음과 같은 예제가 있습니다:

```haskell
main =
    putStrLn "Hello, world!"
```

또는:

```haskell
main =
    putStrLn
        (wrapHtml "Hello, world!")
```

__다음 스타일은 피하세요__. 들여쓰기 단계를 두 개 이상 사용하거나 완전히 무시하고 있습니다:

```haskell
main = putStrLn
        (wrapHtml "Hello, world!")
```

```haskell
main = putStrLn
                (wrapHtml "Hello, world!")
```

