# 환경 변수 전달하기

이번 장에서는 블로그 이름, 스타일시트 위치와 같은 블로그에 대한 일반적인 정보를 유지할 수 있는 환경을 추가하고자 합니다.

## 환경

환경은 레코드 데이터 타입으로 표현할 수 있으며 사용자 입력을 통해 구성할 수 있습니다.
여기서 사용자 입력은 명령행 인자, 구성 파일 또는 그 외 다른 것들이 될 수 있습니다.

```haskell
module HsBlog.Env where

data Env
  = Env
    { eBlogName :: String
    , eStylesheetPath :: FilePath
    }
  deriving Show

defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css"
```

이러한 레코드를 요청받은 정보로 채운이후, 필요로 하는 함수의 입력으로 전달할 수 있습니다.
이는 작은 프로젝트에서는 잘 동작하지만, 프로젝트가 커지고 많은 중첩된 함수들이 같은 정보를 필요로 할 때는 환경을 전달하는 것이 번거로울 수 있습니다.

함수의 입력으로 환경을 전달하는 대신, `mtl` (또는 `transformers`) 패키지에서 제공하는
[`ReaderT`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:2)
타입을 사용할 수 있습니다.

### ReaderT

```haskell
newtype ReaderT r m a = ReaderT (r -> m a)
```

`ReaderT`는 `ExceptT`와 비슷한 *monad transformer*이며
`Functor`, `Applicative`, `Monad` 그리고 `MonadTrans`의 인스턴스 또한 제공합니다.

정의에서 볼 수 있듯이, `ReaderT`는 `r` 타입의 값을 받아 `m a` 타입의 값을 반환하는 함수를 감싼 _newtype_ 입니다.
`r`은 보통 우리가 합성하고자 하는 함수들 사이에서 공유하고자 하는 환경을 나타냅니다.
그리고 `m a`는 우리가 반환하고자 하는 결과를 나타냅니다.
`m`은 우리가 익숙한 `Monad`를 구현하는 어떤 타입이든 될 수 있습니다.
보통 `IO` 또는 `Identity`와 잘 어울리는데, 환경을 효과가 있는 또는 효과가 없는 계산 사이에서 공유하고자 할 때에 따라 다릅니다.

`ReaderT`는 `r` 타입의 값을 _가지고 있고_ `Applicative`과 `Monad` 인터페이스를 사용할 때 다른 함수들에게 `r` 타입의 값을 전달합니다.
따라서 직접 `r` 타입의 값을 전달하지 않아도 됩니다.
필요한 경우 단순히 `ask`를 사용하면 됩니다.

블로그의 경우 `Env`를 전달하는 대신 우리의 함수들을 `ReaderT`를 사용하게 변경할 수 있습니다.
효과가 없고 `IO`를 사용하지 않으면 `a` 대신 `Reader Env a`를 반환하고 (또는 더 간단한 버전인 `Reader Env a`),
효과가 있는 경우 `IO a` 대신 `ReaderT Env IO a`를 반환합니다.

이전에 언급했듯이, `Functor`, `Applicative` 그리고 `Monad`는 이들의 인터페이스를 구현한 타입이 `* -> *` kind를 가져야합니다.
이는 `ReaderT r m`이 이들의 인터페이스를 구현하고, 함수들을 `<*>` 또는 `>>=`로 합성할 때 `f` 또는 `m`을 타입 시그니처에서 `ReaderT r m`으로 대체한다는 것을 의미합니다.

이는 `Either e`의 경우 같은 에러타입을 가진 함수끼리만 합성할 수 있었던 것과 비슷하게,
`ReaderT r m`의 경우 같은 `r` 타입과 같은 `m` 타입을 가진 함수끼리만 합성할 수 있다는 것을 의미합니다.

우리는 `m`을 `Identity`로 하는 특수화된 `ReaderT`인
[`Reader`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:2)
를 사용할 예정인디ㅏ.
`Control.Monad.Reader`는 다음 alias를 제공합니다: `Reader r a = ReaderT r Identity a`.

> 만약 `ReaderT`에 대한 개념이 아직 혼란스럽고 `ReaderT`가 어떻게 동작하는지 더 잘 이해하고 싶다면, 다음 연습문제를 풀어보세요:
>
> 1. `Applicative` 또는 `Monad` 인터페이스 함수를 선택하세요. 개인적으로 `liftA2`를 추천합니다.
>    그리고 `f` (또는 `m`)를 `ReaderT` 타입인 `ReaderT Int IO`와 같은 구체적인 타입으로 대체하여 타입 시그니처를 특수화하세요.
> 2. `ReaderT` newtype을 풀어 `ReaderT Int IO t`를 `Int -> IO t`로 대체하세요.
> 3. 선택한 함수를 특수화된 타입에 대해 구현하세요.
>
> <details><summary>liftA2에 대한 정답</summary>
>
> ```haskell
> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> ```
>
> <details><summary>(1)번 정답</summary>
>
> ```haskell
> -- 특수화: `f`를 `ReaderT Env IO`로 대체
> liftA2 :: (a -> b -> c) -> ReaderT Env IO a -> ReaderT Env IO b -> ReaderT Env IO c
> ```
>
> </details>
>
> <details><summary>(2)번 정답</summary>
>
> ```haskell
> -- newtype 해제, `ReaderT Env IO a`를 `Env -> IO a`로 대체
> liftA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
> ```
>
> </details>
>
> <details><summary>(3)번 정답</summary>
>
> ```haskell
> specialLiftA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
> specialLiftA2 combine funcA funcB env =
>   liftA2 combine (funcA env) (funcB env)
> ```
>
> 이러한 `ReaderT`에 대한 `liftA2`의 역할은 두 함수에 `env`를 제공하고, 나머지 일은 기반 타입 `m` (우리의 경우 `IO`)의 `liftA2` 구현에 위임하는 것을 알 수 있습니다.
> 이러한 형태는 다양한 `m`에 대해 기능을 추가하는 것처럼 보이지 않나요?
> 이것이 바로 monad transformer의 아이디어입니다.
>
> </details>
> </details>

### Reader 사용법

#### 함수 정의하기

다음과 같은 함수를 정의하는 대신:

```haskell
txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
```

다음과 같이 정의합니다:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
```

이제 이 함수는 `Reader`를 사용하므로, 함수 구현부도 수정이 필요합니다.

변경 전:

```haskell
txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml env txtFiles =
 let
   txtOutputFiles = map toOutputMarkupFile txtFiles
   index = ("index.html", buildIndex env txtOutputFiles)
   htmlPages = map (convertFile env) txtOutputFiles
 in
   map (fmap Html.render) (index : htmlPages)
```

`env`를 다른 함수에게 어떻게 전달하는지 주의깊게 살펴보세요.

변경 후:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
txtsToRenderedHtml txtFiles = do
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
  index <- (,) "index.html" <$> buildIndex txtOutputFiles
  htmlPages <- traverse convertFile txtOutputFiles
  pure $ map (fmap Html.render) (index : htmlPages)
```

이제 *do 표기법*을 사용하고 있으며, `env`를 전달하지 않고도 `buildIndex`와 `convertFile`을 *합성*할 수 있습니다.
함수를 합성하기 위해 타입 클래스 인터페이스를 사용하고 있습니다.
`buildIndex`에 `fmap`을 사용해 출력 파일을 추가하고, `map` 대신 `traverse`를 사용해 `convertFile`이 생성할 수 있는 여러 `Reader` 값을 합성합니다.

#### `Env` 추출하기

`Env`를 사용하려면 `Reader`에서 *추출*해야 합니다.
다음 함수를 사용합니다:

```haskell
ask :: ReaderT r m r
```

`ask`는 `Reader`에서 `r`을 꺼내오며 `>>=` 또는 `do` 표기법 안에서 `<-`을 사용해 추출할 수 있습니다.
다음 코드를 비교해보세요:

변경 전:

```haskell
convertFile :: Env -> (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile env (file, doc) =
  (file, convert env (takeBaseName file) doc)
```

변경 후:

```haskell
convertFile :: (FilePath, Markup.Document) -> Reader Env (FilePath, Html.Html)
convertFile (file, doc) = do
  env <- ask
  pure (file, convert env (takeBaseName file) doc)
```

:::note
`Reader`를 사용하기 위해 `convert`를 수정하지 않았습니다.
이는 `convert`가 라이브러리의 사용자에게 노출되는 API이기 때문입니다.
더 간단한 인터페이스를 제공함으로써, monad transformer에 대해 아직 익숙하지 않은 사용자도 라이브러리를 사용할 수 있습니다.

함수 인자 전달 인터페이스는 간단하게 유지하는 것이 좋습니다.
:::

### `Reader` 실행하기

이전에 `Either`를 사용해 에러를 처리하는 것과 비슷하게, `Reader`를 사용하는 계산에 환경을 전달하고, 계산에서 결과를 추출해야 합니다.
이를 위해 `runReader`와 `runReaderT` 함수를 사용합니다:

```haskell
runReader :: Reader r a -> (r -> a)

runReaderT :: ReaderT r m a -> (r -> m a)
```

이러한 함수는 `Reader` 또는 `ReaderT`를 `r`을 받는 함수로 변환합니다.
그러면 이 함수에 초기 환경을 전달할 수 있습니다:

```haskell
convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = runReader (txtsToRenderedHtml filesToProcess) env
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."
```

`let outputHtmls` 부분을 살펴보세요.

### 추가: 특정 호출을 위해 `Env` 변환하기

때로는 특정 함수 호출에 전달하는 `Env`를 수정해야 할 수도 있습니다.
예를 들어, 많은 정보를 포함하는 일반적인 `Env` 타입이 있고, 그 중 일부 정보만 필요로 하는 함수 호출이 있을 수 있습니다.

만약 호출하는 함수가 `conver`와 비슷하고 `Reader`대신 환경을 인자로 받는다면,
`ask`를 활용해 환경을 추출하고, 변환한 후 그 결과를 함수에 전달할 수 있습니다:

```haskell
outer :: Reader BigEnv MyResult
outer = do
  env <- ask
  pure (inner (extractSmallEnv env))

inner :: SmallEnv -> MyResult
inner = ...

extractSmallEnv :: BigEnv -> SmallEnv
extractSmallEnv = ...
```

하지만 만약 `inner`가 인자 전달 대신 `Reader SmallEnv`를 사용한다면,
`runReader`를 사용해 *`inner`를 일반 함수로 변환*할 수 있으며 위와 같은 방식을 적용할 수 있습니다!

```haskell
outer :: Reader BigEnv MyResult
outer = do
  env <- ask
  -- Here the type of `runReader inner` is `SmallEnv -> MyResult`
  pure (runReader inner (extractSmallEnv env))

inner :: Reader SmallEnv MyResult
inner = ...

extractSmallEnv :: BigEnv -> SmallEnv
extractSmallEnv = ...
```

이러한 패턴은 일반적이라서
[withReaderT](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Reader.html#v:withReaderT)
라는 함수가 존재하며 이를 사용해 더 간단하게 표현할 수 있습니다:

```haskell
withReaderT :: (env2 -> env1) -> ReaderT env1 m a -> ReaderT env2 m a
```

이 함수는 환경을 변환하는 함수를 받아 `ReaderT env1 m a` 계산을 `ReaderT env2 m a` 계산으로 변환합니다.

이번 예제에 적용해보겠습니다:

```haskell
outer :: Reader BigEnv MyResult
outer = withReaderT extractSmallEnv inner
```

---

문제: 이번 예제로 구체화하면 `withReaderT`의 타입은 어떻게 될까요?

<details><summary>정답</summary>

```haskell
withReaderT
  :: (BigEnv -> SmallEnv)     -- `extractSmallEnv` 타입과 동일합니다.
  -> Reader SmallEnv MyResult -- `inner` 타입과 동일합니다.
  -> Reader BigEnv   MyResult -- `outer` 타입과 동일합니다.
```

</details>

---

각 환경의 순서에 주의하세요!
`SmallEnv`의 `Reader`를 `BigEnv`의 `Reader`로 변환하기 위해서는, `BigEnv`를 `SmallEnv`로 변환하는 함수가 필요합니다!

이러한 순서를 가지는 이유는 함수의 *출력*대신 *입력*에 대해 매핑을 수행하기 때문입니다.
이는 공변(variance)과 반공변(covariance)에 대한 주제와 관련이 있지만, 지금 당장은 중요하지 않습니다.

### 로직에서 `Env` 사용하기

아직 다루지 못한 주제가 하나 있습니다.
바로 `convert` 함수를 통해 원하는 페이지를 생성하는 방법입니다.
사실 우리는 아직 스타일시트를 HTML EDSL에 추가하는 기능조차 존재하지 않습니다.
이제 이 기능을 추가해보겠습니다:

---

스타일시트는 `head` 요소에 들어가기 때문에, `head` 정보를 위한 `Structure`와 같은 추가적인 `newtype`을 만드는 것이 좋을 것 같습니다.
제목, 스타일시트, 그리고 메타 요소와 같은 것들은 `Structure`를 만들면서 했던것과 같은 방식으로 조합할 수 있습니다!

1. 지금 당장 해보기: `head`를 위한 3 개의 함수를 구현해 HTML 라이브러리를 확장해보세요.
   제목을 위한 `title_`, 스타일시트를 위한 `stylesheet_`, 그리고
   [twitter cards](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards)
   와 같은 메타 요소를 위한 `meta_`입니다.

   <details><summary>정답</summary>

     <details><summary>src/HsBlog/Html.hs</summary>

   ```haskell
   -- Html.hs

   module HsBlog.Html
     ( Html
     , Head
     , title_
     , stylesheet_
     , meta_
     , Structure
     , html_
     , p_
     , h_
     , ul_
     , ol_
     , code_
    , Content
     , txt_
     , img_
     , link_
     , b_
     , i_
     , render
     )
     where

   import Prelude hiding (head)
   import HsBlog.Html.Internal
   ```

     </details>

     <details><summary>src/HsBlog/Html/Internal.hs</summary>

   ```haskell
   newtype Head
     = Head String

   -- * EDSL

   html_ :: Head -> Structure -> Html
   html_ (Head head) content =
     Html
       ( el "html"
         ( el "head" head
           <> el "body" (getStructureString content)
         )
       )

   -- * Head

   title_ :: String -> Head
   title_ = Head . el "title" . escape

   stylesheet_ :: FilePath -> Head
   stylesheet_ path =
     Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

   meta_ :: String -> String -> Head
   meta_ name content =
     Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

   instance Semigroup Head where
     (<>) (Head h1) (Head h2) =
       Head (h1 <> h2)

   instance Monoid Head where
     mempty = Head ""
   ```

     </details>

   </details>

2. `conver`와 `buildIndex`가 새로운 API를 사용하게 수정하세요.
   `buildIndex`는 `Reader`를 반환해야 합니다!

   <details><summary>정답</summary>

     <details><summary>src/HsBlog/Convert.hs</summary>

   ```haskell
   import Prelude hiding (head)
   import HsBlog.Env (Env(..))

   convert :: Env -> String -> Markup.Document -> Html.Html
   convert env title doc =
     let
       head =
         Html.title_ (eBlogName env <> " - " <> title)
           <> Html.stylesheet_ (eStylesheetPath env)
       article =
         foldMap convertStructure doc
       websiteTitle =
         Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
       body =
         websiteTitle <> article
     in
       Html.html_ head body
   ```

     </details>

     <details><summary>src/HsBlog/Directory.hs</summary>

   ```haskell
   buildIndex :: [(FilePath, Markup.Document)] -> Reader Env Html.Html
   buildIndex files = do
     env <- ask
     let
       previews =
         map
           ( \(file, doc) ->
             case doc of
               Markup.Head 1 head : article ->
                 Html.h_ 3 (Html.link_ file (Html.txt_ head))
                   <> foldMap convertStructure (take 2 article)
                   <> Html.p_ (Html.link_ file (Html.txt_ "..."))
               _ ->
                 Html.h_ 3 (Html.link_ file (Html.txt_ file))
           )
           files
     pure $ Html.html_
         ( Html.title_ (eBlogName env)
           <> Html.stylesheet_ (eStylesheetPath env)
         )
         ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
           <> Html.h_ 2 (Html.txt_ "Posts")
           <> mconcat previews
         )
   ```

     </details>

   </details>

3. `Env`를 위한 명령줄 파서를 만들어, `convert-dir`명령어에 연결하세요.
   그리고 결과를 `convertDirectory` 함수에 전달하세요.

<details><summary>정답</summary>

<details><summary>src/HsBlog.hs</summary>

```haskell
import HsBlog.Env (defaultEnv)

convertSingle :: String -> Handle -> Handle -> IO ()

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
```

</details>

<details><summary>app/OptParse.hs</summary>

```haskell
import HsBlog.Env

------------------------------------------------
-- * Our command-line options model

-- | Model
data Options
 = ConvertSingle SingleInput SingleOutput
 | ConvertDir FilePath FilePath Env
 deriving Show

------------------------------------------------
-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir =
 ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

-- | Parser for blog environment
pEnv :: Parser Env
pEnv =
 Env <$> pBlogName <*> pStylesheet

-- | Blog name parser
pBlogName :: Parser String
pBlogName =
 strOption
   ( long "name"
     <> short 'N'
     <> metavar "STRING"
     <> help "Blog name"
     <> value (eBlogName defaultEnv)
     <> showDefault
   )

-- | Stylesheet parser
pStylesheet :: Parser String
pStylesheet =
 strOption
   ( long "style"
     <> short 'S'
     <> metavar "FILE"
     <> help "Stylesheet filename"
     <> value (eStylesheetPath defaultEnv)
     <> showDefault
   )

```

</details>

<details><summary>app/Main.hs</summary>

```haskell
main :: IO ()
main = do
 options <- parse
 case options of
   ConvertDir input output env ->
     HsBlog.convertDirectory env input output

   ...
```

</details>

</details>

---

### 요약

직접 인자를 전달하는 방법과 `Reader`를 사용하는 방법 중 어떤것을 선호하시나요?

저는 상황에 따라 `Reader`를 사용하는 두 번째 방식이 직접 인자 전달하는 첫 번째 방식보다 더 좋다고 말할 수는 없다고 생각합니다.

`Reader`와 `ReaderT`를 사용하면 이러한 개념과 기술에 익숙하지 않은 사람에게는 불친절한 코드가 될 수 있습니다.
이러한 상황에서는 큰 이점을 얻지 못할 것입니다.

프로그램이 커질수록 `Reader`를 사용하는 방법이 더 유용해집니다.
우리의 비교적 작은 예제에서는 `Reader`를 사용하는 것이 적합하지 않을 수도 있습니다.
하지만 `Reader`는 익혀야 할 중요한 기술이라고 생각하기에 이 책에 포함했습니다.

고급 기술을 사용했을 때의 이점과 비용을 고려하는 것은 중요합니다.
그리고 때로는 할 수 있다면, 더 간단한 방법을 사용하는 것이 좋습니다.

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/f9fe7179fcf0e6c818f6caa860b52e991432dab2)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/f9fe7179fcf0e6c818f6caa860b52e991432dab2) 를 확인할 수 있습니다.
