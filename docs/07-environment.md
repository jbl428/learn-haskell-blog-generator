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

`ReaderT`는 `ExceptT`와 비슷한 _monad transformer_이며
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
>     그리고 `f` (또는 `m`)를 `ReaderT` 타입인 `ReaderT Int IO`와 같은 구체적인 타입으로 대체하여 타입 시그니처를 특수화하세요.
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

### How to use Reader

#### Defining a function

Instead of defining a function like this:

```haskell
txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
```

We define it like this:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
```

Now that our code uses `Reader`, we have to accommodate that in the way we write our functions.

Before:

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

Note how we needed to thread the `env` to the other functions that use it.

After:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
txtsToRenderedHtml txtFiles = do
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
  index <- (,) "index.html" <$> buildIndex txtOutputFiles
  htmlPages <- traverse convertFile txtOutputFiles
  pure $ map (fmap Html.render) (index : htmlPages)
```

Note how we use _do notation_ now, and _instead of threading_ `env` around we _compose_
the relevant functions, `buildIndex` and `convertFile`, we use the type classes
interfaces to compose the functions. Note how we needed to `fmap` over `buildIndex`
to add the output file we needed with the tuple, and how we needed to use `traverse` instead
of `map` to compose the various `Reader` values `convertFile` will produce.

### Extracting `Env`

When we want to use our `Env`, we need to _extract_ it from the `Reader`.
We can do it with:

```haskell
ask :: ReaderT r m r
```

Which yanks the `r` from the `Reader` - we can extract with `>>=` or `<-` in do notation.
See the comparison:

Before:

```haskell
convertFile :: Env -> (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile env (file, doc) =
  (file, convert env (takeBaseName file) doc)
```

After:

```haskell
convertFile :: (FilePath, Markup.Document) -> Reader Env (FilePath, Html.Html)
convertFile (file, doc) = do
  env <- ask
  pure (file, convert env (takeBaseName file) doc)
```

> Note: we didn't change `convert` to use `Reader` because it is a user facing API for our
> library. By providing a simpler interface we allow more users to use our library -
> even those that aren't yet familiar with monad transformers.
>
> Providing a simple function argument passing interface is preferred in this case.

### Run a `Reader`

Similar to handling the errors with `Either`, at some point we need to supply the environment to
a computation that uses `Reader`, and extract the result from the computation.
We can do that with the functions `runReader` and `runReaderT`:

```haskell
runReader :: Reader r a -> (r -> a)

runReaderT :: ReaderT r m a -> (r -> m a)
```

These functions convert a `Reader` or `ReaderT` to a function that takes `r`.
Then we can pass the initial environment to that function:

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

See the `let outputHtmls`part.

### Extra: Transforming `Env` for a particular call

Sometimes we may want to modify the `Env` we pass to a particular function call.
For example, we may have a general `Env` type that contains a lot of information, and
functions that only need a part of that information.

If the functions we are calling are like `convert` and take the environment as an
argument instead of a `Reader`, we can just extract the environment
with `ask`, apply a function to the extracted environment,
and pass the result to the function, like this:

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

But if `inner` uses a `Reader SmallEnv` instead of argument passing,
we can use `runReader` to _convert `inner` to a normal function_,
and use the same idea as above!

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

This pattern is generalized and captured by a function called
[withReaderT](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Reader.html#v:withReaderT),
and works even for `ReaderT`:

```haskell
withReaderT :: (env2 -> env1) -> ReaderT env1 m a -> ReaderT env2 m a
```

`withReaderT` takes a function that modifies the environment,
and converts a `ReaderT env1 m a` computation to a `ReaderT env2 m a` computation
using this function.

Let's see it concretely with our example:

```haskell
outer :: Reader BigEnv MyResult
outer = withReaderT extractSmallEnv inner
```

---

Question: what is the type of `withReaderT` when specialized in our case?

<details><summary>Answer</summary>

```haskell
withReaderT
  :: (BigEnv -> SmallEnv)     -- This is the type of `extractSmallEnv`
  -> Reader SmallEnv MyResult -- This is the type of `inner`
  -> Reader BigEnv   MyResult -- This is the type of `outer`
```

</details>

---

Note the order of the environments! We use a function from a `BigEnv` to a `SmallEnv`,
to convert a `Reader` of `SmallEnv` to a `Reader` of `BigEnv`!

This is because we are mapping over the _input_ of a function rather than the _output_,
and is related to topics like variance and covariance, but isn't terribly important
for us at the moment.

### Using `Env` in our logic code

One thing we haven't talked about yet is using our environment in the `convert`
function to generate the pages we want. And actually, we don't even have the ability to add
stylesheets to our HTML EDSL at the moment! We need to go back and extend it. Let's do all
that now:

---

Since stylesheets go in the `head` element, perhaps it's a good idea to create an additional
`newtype` like `Structure` for `head` information? Things like title, stylesheet,
and even meta elements can be composed together just like we did for `Structure`
to build the `head`!

1. Do it now: extend our HTML library to include headings and add 3 functions:
   `title_` for titles, `stylesheet_` for stylesheets, and `meta_` for meta elements
   like [twitter cards](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards).

   <details><summary>Solution</summary>

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

2. Fix `convert` and `buildIndex` to use the new API. Note: `buildIndex` should return
   `Reader`!

   <details><summary>Solution</summary>

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

3. Create a command-line parser for `Env`, attach it to the `convert-dir` command,
   and pass the result it to the `convertDirectory` function.

<details><summary>Solution</summary>

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

### Summary

Which version do you like better? Manually passing arguments, or using `Reader`?

To me, it is not clear that the second version with `Reader` is better than the first
with explicit argument passing in our particular case.

Using `Reader` and `ReaderT` makes our code a little less friendly toward beginners
that are not yet familiar with these concepts and techniques, and we don't see
(in this case) much benefit.

As programs grow larger, techniques like using `Reader` become more attractive to use.
For our relatively small example, using `Reader` might not be appropriate.
I've included it in this book because it is an important technique to have in our
arsenal and I wanted to demonstrate it.

It is important to weigh the benefits and costs of using advanced techniques,
and it's often better to try and get away with simpler techniques if we can.

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/f9fe7179fcf0e6c818f6caa860b52e991432dab2)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/f9fe7179fcf0e6c818f6caa860b52e991432dab2).
