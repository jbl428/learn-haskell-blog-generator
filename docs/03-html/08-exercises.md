# 연습문제

블로그 소프트웨어를 위해 HTML 라이브러리에 몇 가지 기능이 더 필요합니다.
`Html.Internal` 모듈에 다음 기능을 추가하고 `Html` 모듈에서 내보내세요.

## 1. 순서 없는 목록

HTML에서 순서 없는 목록은 다음 형태를 가집니다:

```html
<ul>
  <li>item 1</li>
  <li>item 2</li>
  <li>...</li>
</ul>
```

라이브러리에 다음 함수를 추가하고자 합니다:

```haskell
ul_ :: [Structure] -> Structure
```

이제 사용자는 다음과 같이 작성할 수 있습니다:

```haskell
ul_
  [ p_ "item 1"
  , p_ "item 2"
  , p_ "item 3"
  ]
```

그리고 다음 결과를 얻습니다:

```html
<ul>
  <li><p>item 1</p></li>
  <li><p>item 2</p></li>
  <li><p>item 3</p></li>
</ul>
```

## 2. 순서 있는 목록

순서없는 목록과 유사하지만, `<ul>` 대신 `<ol>`을 사용합니다.

## 3. 코드 블록

`<p>`와 유사하지만, `<pre>` 태그를 사용합니다. `code_`라는 함수를 정의하세요.

## 정답

<details>
  <summary>순서없는 목록</summary>

```haskell
ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)
```

</details>

<details>
  <summary>순서있는 목록</summary>

```haskell
ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)
```

:::note
위 두 함수를 하나의 함수로 합칠 수 있습니다.
:::

</details>

<details>
  <summary>코드 블록</summary>

```haskell
code_ :: String -> Structure
code_ = Structure . el "pre" . escape
```

</details>
