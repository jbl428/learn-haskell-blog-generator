# Either와 IO

I/O를 요구하는 `IO` 액션을 만들 때는 다양한 에러가 발생할 수 있습니다.
예를 들면, `writeFile`을 사용할 때 파일을 쓰는 중에 디스크 공간이 부족해질 수 있고, 파일이 쓰기 보호되어 있을 수도 있습니다.
이러한 상황은 흔하지는 않지만 발생할 가능성은 언제나 있습니다.

`readFile`과 `writeFile`과 같은 하스켈 함수를 `Either`를 반환하는 `IO` 연산으로 바꿔볼 수 있습니다.
예를 들면 다음과 같습니다:

```haskell
readFile :: FilePath -> IO (Either ReadFileError String)
writeFile :: FilePath -> String -> IO (Either WriteFileError ())
```

하지만 여기에는 몇 가지 문제가 있습니다.
첫 번째는 `IO` 액션을 합성하는 것이 더 어려워진다는 것입니다.
이전에 작성한 다음 코드는

```haskell
readFile "input.txt" >>= writeFile "output.html"
```

이제는 타입이 일치하지 않습니다 - `readFile`은 실행될 때 `Either ReadFileError String`을 반환하지만,
`writeFile`은 `String`을 입력으로 받습니다.
따라서 `writeFile`를 호출하기 전에 에러를 처리해야 합니다.

## ExceptT를 사용한 IO + Either 합성

이를 해결하는 한 가지 방법은 **monad transformer**를 사용하는 것입니다.
Monad transformer는 **monad 기능을 하나씩 쌓아 올릴 수 있는 방법**을 제공합니다.
이름이 transformer인 이유는 **monad 인스턴스를 입력으로 받아 새로운 기능을 쌓아 올린 새로운 monad 타입을 반환하기 때문**입니다.

예를 들어, `IO (Either Error a)`와 같은 타입의 값을 monadic 인터페이스를 사용하여 합성하고 싶다고 가정해봅시다.
이를 위해 [`ExceptT`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html#g:2)라는 monad transformer를 사용하여 `IO` 위에 쌓을 수 있습니다.
`ExceptT`의 정의를 살펴보겠습니다:

```haskell
newtype ExceptT e m a = ExceptT (m (Either e a))
```

`newtype`은 기존 타입의 새로운 이름을 만드는 데 사용됩니다.
`e`를 `Error`로, `m`을 `IO`로 바꾸면 우리가 원하는 것과 정확히 일치하는 `IO (Either Error a)`가 됩니다.
그리고 `ExceptT Error IO a`를 `IO (Either Error a)`로 변환하는 함수 `runExceptT`를 사용할 수 있습니다:

```haskell
runExceptT :: ExceptT e m a -> m (Either e a)
```

`ExceptT`는 `Either`의 기능과 임의의 `m`의 기능을 결합하는 방식으로 모나드 인터페이스를 구현합니다.
`ExceptT e m`은 `Monad` 인스턴스이므로, 특수화된 `>>=`의 구현은 다음과 같습니다:

```haskell
-- 일반적인 버전
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- 특수화된 버전, 위의 `m`을 `ExceptT e m`으로 바꾸면 됩니다.
(>>=) :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
```

특수화된 버전에서의 `m`은 여전히 `Monad` 인스턴스여야 합니다.

---

어떻게 동작하는지 확실하지 않다면, `IO (Either Error a)`의 `>>=`를 구현해보세요:

```haskell
bindExceptT :: IO (Either Error a) -> (a -> IO (Either Error b)) -> IO (Either Error b)
```

<details><summary>정답</summary>

```haskell
bindExceptT :: IO (Either Error a) -> (a -> IO (Either Error b)) -> IO (Either Error b)
bindExceptT mx f = do
  x <- mx -- `x`의 타입은 `Either Error a`입니다.
  case x of
    Left err -> pure (Left err)
    Right y -> f y
```

여기서 `Error` 또는 `IO`의 구현 세부 사항을 실제로 사용하지 않았습니다.
`Error`는 전혀 언급되지 않았고, `IO`에 대해서는 do 표기법과 함께 모나드 인터페이스만 사용했습니다.
따라서 더 일반화된 타입 시그니처로 동일한 함수를 작성할 수 있습니다:

```haskell
bindExceptT :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
bindExceptT mx f = do
  x <- mx -- `x` has the type `Either e a`
  case x of
    Left err -> pure (Left err)
    Right y -> f y
```

그리고 `newtype ExceptT e m a = ExceptT (m (Either e a))`이기 때문에
`ExceptT` 생성자를 사용해 감쌀 수 있습니다.

```haskell
bindExceptT :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
bindExceptT mx f = ExceptT $ do
  -- `runExceptT mx`의 타입은 `m (Either e a)`입니다.
  -- `x`의 타입은 `Either e a`입니다.
  x <- runExceptT mx
  case x of
    Left err -> pure (Left err)
    Right y -> runExceptT (f y)
```

</details>

---

> monad transformer를 쌓을 때, 쌓는 순서가 중요하다는 점을 기억하세요.
> `ExceptT Error IO a`을 사용하면, `Either`를 반환하는 `IO` 연산이 생성됩니다.

`ExceptT`는 두 가지 경우를 모두 처리할 수 있습니다. - `throwError` 함수를 사용하여 오류 값을 반환할 수 있습니다:

```haskell
throwError :: e -> ExceptT e m a
```

그리고 monadic 타입 `m`의 값을 반환하는 함수를 끌어올려(`lift`), `ExceptT e m a`를 반환하게 할 수 있습니다:

```haskell
lift :: m a -> ExceptT e m a
```

예를 들면:

```haskell
getLine :: IO String

lift getLine :: ExceptT e IO String
```

> `lift`는 사실 `MonadTrans`의 타입 클래스 함수이기도 합니다.
> `MonadTrans`는 monad transformer의 타입 클래스입니다.
> 따라서 정확한 타입은 `lift getLine :: MonadTrans t => t IO String`이지만, > 여기서는 이해를 돕기 위해 구체화했습니다.

이제 다음의 경우:

```haskell
readFile :: FilePath -> ExceptT IOError IO String

writeFile :: FilePath -> String -> ExceptT IOError IO ()
```

문제 없이 합성할 수 있습니다:

```haskell
readFile "input.txt" >>= writeFile "ouptut.html"
```

여기서 주의해야 할 점은 에러 타입 `e`가 (`Either`와 `Except` 모두) 각 함수 내에서 동일해야 한다는 것입니다!
이는 `readFile`과 `writeFile` 모두 에러를 나타내는 타입이 동일해야 한다는 것을 의미합니다.
따라서 이러한 함수의 사용자가 동일한 에러를 처리해야 함을 의미합니다.
`writeFile`을 호출한 사용자는 "파일을 찾을 수 없음" 에러를 처리해야 할까요?
`readFile`을 호출한 사용자는 "디스크 공간 부족" 에러를 처리해야 할까요?
"네트워크 연결 불가", "메모리 부족", "취소된 스레드" 등 이 외에 수많은 IO 에러가 있습니다!
사용자가 이러한 모든 에러를 처리하도록 요구할 수는 없으며, 데이터 타입에서 이러한 모든 에러를 다룰 수도 없습니다.

그렇다면 어떻게 해야 할까요?

**IO 코드에 대해서는** 이러한 접근 방식을 포기하고, Exceptions 라는 다른 방식을 사용합니다.
다음 장에서 살펴보겠습니다.

> 만약 `ExceptT`를
> [`Identity`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Functor-Identity.html)
> 라 불리는 다른 타입 위에 쌓는다면,
> [`Except`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Except.html#t:Except)
> 라는 이름(끝에 `T`가 없습니다)의 `Either`와 동일한 타입을 얻을 수 있습니다.
> `Except`는 `Either`보다 더 적절한 이름과 에러 처리를 위한 더 나은 API를 가지고 있기 때문에
> `Either` 대신 `Except`를 사용하는 것이 더 좋을 수 있습니다.
