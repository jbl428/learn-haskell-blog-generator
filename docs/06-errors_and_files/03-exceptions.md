# 예외

[Control.Exception](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html)
모듈은 `IO` 코드에서 예외를
[throw](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:throwIO)
하고,
`IO` 코드에서 하스켈 예외를
[`catch`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#g:5)
하고, 심지어 `IO (Either ...)`로 변환하는 함수
[`try`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#g:7)
를 제공합니다.

```haskell
throwIO :: Exception e => e -> IO a

catch
  :: Exception e
  => IO a         -- 실행하려는 계산
  -> (e -> IO a)  -- 예외가 발생하면 호출할 핸들러
  -> IO a

try :: Exception e => IO a -> IO (Either e a)
```

위 타입 시그니처에서 중요한 부분은
[`Exception`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#t:Exception)
타입 클래스입니다. 
타입을 `Exception` 타입 클래스의 인스턴스로 만들면, `IO` 코드에서 예외를 던지고 잡을 수 있습니다.

```haskell
{-# language LambdaCase #-}

import Control.Exception
import System.IO

data MyException
  = ErrZero
  | ErrOdd Int
  deriving Show

instance Exception MyException

sayDiv2 :: Int -> IO ()
sayDiv2 n
  | n == 0 = throwIO ErrZero
  | n `mod` 2 /= 0 = throwIO (ErrOdd n)
  | otherwise = print (n `div` 2)

main :: IO ()
main =
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
    )
```

> 참고: 여기서 두 가지 새로운 것을 사용했습니다: guard와 `LambdaCase` 언어 확장입니다.
>
> 1. `sayDiv2`에서 본 guard는 `if-then-else` 표현식의 더 나은 문법입니다.
>     guard를 사용하면 여러 `if` 분기를 가질 수 있고, 마지막으로 `otherwise`를 사용하여 `else` 분기를 사용할 수 있습니다.
>     각 guard(`|`) 뒤에는 조건이 있고, 조건 뒤에는 `=`가 있고, 그 다음에는 표현식이 있습니다. (`if` 표현식의 `then` 뒤에 있는 부분)
> 2. `catch`에서 본 `LambdaCase`는 몇 개의 문자를 줄이기 위한 문법적 설탕일 뿐입니다.
>     `\e -> case e of` 대신에 `\case`를 사용할 수 있습니다.
>     이를 위해서는 `LambdaCase` 확장을 활성화해야 합니다.
>
>    #### 언어 확장
>
>    하스켈은 표준화된 언어입니다. 하지만 GHC는 언어에 *확장*을 제공합니다. - 하스켈 98 또는 2010 표준에서 다루지 않는 추가 기능을 말합니다. 
>    LambdaCase와 같은 문법 확장, 타입 체커에 대한 확장 등이 있습니다.
>
>    이러한 확장은 `{-# language <확장 이름> #-}` (`language` 부분은 대소문자 구분 없음)을 소스 파일의 맨 위에 추가하거나,
>    `.cabal 파일`의 [default-extensions](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-default-extensions)
>    섹션에 지정하여 프로젝트 전체에 전역으로 설정할 수 있습니다.
>
>    언어 확장 목록은 [GHC 매뉴얼](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html)에서 찾을 수 있습니다.
>    찾아보는 것은 자유지만, 모든 확장을 기억하실 필요는 없습니다.

물론 이 예제는 `Either`를 사용하고 '기능적 코어, 명령형 쉘'처럼 나누는 것이 훨씬 더 잘 작동합니다.
어쨌든, 예제로서는 잘 작동합니다.
우리는 커스텀 예외를 만들고 `IO` 블록 밖에서 특별하게 처리했습니다.
하지만 `putStrLn`이 발생시킬 수 있는 예외는 처리하지 않았습니다.
예를 들어, 어떤 이유로 `stdout` 핸들을 이 블록 이전에 닫는다면:

```haskell
main :: IO ()
main = do
  hClose stdout
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
    )
```

프로그램은 다음과 같은 에러가 발생합니다:

```
ghc: <stdout>: hFlush: illegal operation (handle is closed)
```

우선 어떤 예외를 처리해야 하는지 어떻게 알 수 있을까요? 
몇 가지 함수의 문서에는 이것이 포함되어 있지만, 불행히도 `putStrLn`의 문서에는 포함되어 있지 않습니다.
`Exception` 타입 클래스가 가지고 있는
[인스턴스 목록](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#i:Exception)
을 참조해 추측해볼 수 있습니다.
아마
[`IOException`](https://hackage.haskell.org/package/base-4.16.4.0/docs/GHC-IO-Exception.html#t:IOException) fits.
를 처리하면 될거 같습니다.
이제 이 에러를 어떻게 처리할 수 있을까요? catch를 연속해서 사용할 수 있습니다:

```haskell
-- 상단에 아래 내용을 추가해야 합니다.

{-# language ScopedTypeVariables #-}

import GHC.IO.Exception (IOException(..))

main :: IO ()
main = do
  hClose stdout
  catch
    ( catch
      ( do
        putStrLn "Going to print a number now."
        sayDiv2 7
        putStrLn "Did you like it?"
      )
      ( \case
        ErrZero ->
          hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
        ErrOdd n ->
          hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
      )
    )
    ( \(e :: IOException) ->
      -- stderr 핸들에 대한 잘못된 작업인지 확인할 수 있습니다.
      if ioe_handle e /= Just stderr && ioe_type e /= IllegalOperation
        then pure () -- stderr 핸들이 닫혔기 때문에 stderr에 쓸 수 없습니다.
        else hPutStrLn stderr (displayException e)
    )
```

> let 표현식, 람다, 패턴 매칭 등에서 타입을 지정할 수 있도록 하기 위해 `ScopedTypeVariables`를 사용했습니다.

또는 유용한 함수
[`catches`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:catches)
를 사용해 예외
[handlers](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#t:Handler)
목록을 전달할 수 있습니다.

```haskell
main :: IO ()
main = do
  hClose stdout
  catches
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    [ Handler $ \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")

    , Handler $ \(e :: IOException) ->
      -- stderr 핸들에 대한 잘못된 작업인지 확인할 수 있습니다.
      if ioe_handle e /= Just stderr && ioe_type e /= IllegalOperation
        then pure () -- stderr 핸들이 닫혔기 때문에 stderr에 쓸 수 없습니다.
        else hPutStrLn stderr (displayException e)
    ]
```

> 추가로 `Handler`는 `Exception`을 구현하는 임의의 타입을 가지는 함수를 숨기기 위해
> [existentially quantified types](https://en.m.wikibooks.org/wiki/Haskell/Existentially_quantified_types)
> 라는 개념을 사용합니다.
> 이를 통해 `catches`가 입력으로 받는, 예외를 처리하는 함수들의 혼합된 리스트를 처리할 수 있습니다.
> 이러한 패턴은 자주 사용되지는 않지만, 혼란을 피하기 위해 여기에 포함시켰습니다.

만약 모든 예외를 처리하고 싶다면, `SomeException`을 사용하면 됩니다:

```haskell
main :: IO ()
main = do
  hClose stdout
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \(SomeException e) ->
      hPutStrLn stderr (show e)
    )
```

`SomeException`은 다른 상황에 대한 특별한 처리를 원한다면, `catches` 리스트의 마지막 요소로 넣을 수도 있습니다.

다른 알아두면 좋은 함수로
[`bracket`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:bracket)
과
[`finally`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:finally)
가 있습니다.
이러한 함수들은 에러가 발생할 수 있는 리소스를 안전하게 획득하는데 도움을 줍니다.

---

`app/Main.hs` 파일의 `main`에서는 보통 핸들을 열고 닫는 작업을 합니다.
우리가 열었던 핸들을 정리해야 하는 상황이 있을까요? 
어떤 부분에서 예외가 발생할 수 있을까요?
어떤 핸들은 닫히지 않을까요?

- `bracket`을 사용해 예외가 발생할지라도, 작업 이후에 핸들이 닫히도록 해보세요.
  하지만 `stdin`과 `stdout`에 대해서는 닫히지 않도록 해야 합니다.
  <details><summary>힌트</summary>매개 변수를 받는 함수를 다른 함수로 전달하고, 해당 함수가 매개 변수를 만들어 호출하는 방식인 continuation-passing style을 사용할 수 있습니다.
  </details>
- 어떻게 하면 `Stdin`과 `InputFile`을 위한 `outputHandle` 코드의 중복을 제거할 수 있을까요?
  <details><summary>힌트</summary>let을 사용하세요.</details>

<details><summary>정답</summary>

```haskell
import Control.Exception (bracket)

main :: IO ()
main = do
...

    ConvertSingle input output ->
      let
        -- 여기서 action은 우리가 하고 싶은 다음 단계입니다.
        -- 우리가 만든 값을 입력으로 받아서 사용하고,
        -- 이후 정리할 수 있도록 control을 반환합니다.
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              bracket
                (openFile file ReadMode)
                hClose
                (action file)

        -- 두 함수 모두 action은 원하는 임의의 타입 `a`를 반환할 수 있습니다.
        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout ->
              action stdout
            OutputFile file -> do
              exists <- doesFileExist file
              shouldOpenFile <-
                if exists
                  then confirm
                  else pure True
              if shouldOpenFile
                then
                  bracket (openFile file WriteMode) hClose action
                else
                  exitFailure
      in
        withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)
```

</details>

`bracket (openFile file <mode>) hClose`과 같은 동작을 하는 커스텀 함수가 있습니다.
바로
[withFile](https://hackage.haskell.org/package/base-4.17.0.0/docs/System-IO.html#v:withFile)
입니다.
보통 `with` 접두사를 가진 함수들은, continuation-passing style 패턴을 사용합니다.

---

## 요약

예외는 `IO`를 사용할 때 유용하고 때로는 필수적입니다.
이는 프로그램이 오류를 우아하게 처리할 수 있도록 합니다.
`Either`와 달리, 서로 다른 타입의 오류를 던질 수 있는 함수를 쉽게 조합할 수 있지만, 
반환값을 통해 타입을 전달하지 않기 때문에 핸들링을 강제하지 않는 단점이 있습니다.

하스켈에서는 언어 설계자들이 `IO`를 `Either` 대신 예외를 사용할 수 있는 선택지를 제공해주었습니다.
대부분의 경우 효과를 가진 계산을 다룰 때 추천하는 방법입니다.
하지만 효과가 없는 코드의 경우에는 `Either`가 더 적절하다고 생각합니다.
왜냐하면 우리가 오류를 인지하고 처리해야 한다는 것을 강제해, 프로그램을 더 견고하게 만들기 때문입니다.
또한 `IO` 코드에서만 예외를 잡을 수 있기 때문이기도 합니다.
