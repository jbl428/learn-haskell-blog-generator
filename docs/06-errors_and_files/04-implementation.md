# 코드를 작성해봅시다!

지금까지 많은 설명을 했습니다. 이제는 배운것을 활용해봅시다. 다음 작업을 할 것입니다.

- 출력 디렉토리를 생성합니다.
- 디렉토리의 모든 파일 이름을 가져옵니다.
- 확장자에 따라 필터링합니다.
- .txt 파일을 처리합니다.
- 다른 파일은 수정하지 않고 복사합니다.
- 각 텍스트 파일을 구문 분석하고 결과의 색인을 작성하고, 파일을 HTML로 변환하고, 모든 것을 대상 디렉토리에 생성합니다.

:::note
최종 형태로 제시된 코드는 한 번에 작성한게 아닙니다.
코드 작성, 리팩터링, 함수 분할, 타입 시그니처 변경 등의 반복 과정이었습니다.
코딩 문제를 해결할 때는 작고 간단한 것부터 시작하고, 작동하게 만들고, 코드가 더 명확하고 모듈화되도록 리팩터링하는 것이 좋습니다.
하스켈에서는 우리가 코드를 리팩터링하고 시간이 지남에 따라 개선할 수 있는 능력에 자부심을 가지고 있으며, 새로운 소프트웨어를 작성할 때도 그 원칙이 유지됩니다!
:::

## 새로운 모듈

먼저 `HsBlog.Directory`라는 새로운 모듈을 만들겠습니다.
이 모듈은 디렉터리와 여러 파일을 처리할 것입니다.
이 모듈에서는 이전에 정의한 `convertDirectory`와 `buildIndex` 함수를 내보낼 것입니다.

```haskell
-- | 여러 파일을 처리하고 디렉토리를 변환합니다

module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where
```

이 모듈에서는 디렉터리, 파일 및 파일 경로를 조작하는 데 사용할
[directory](https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html)
와 [filepath](https://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath.html)
라이브러리를 사용합니다.
우리가 배운 새로운 추상화인 `Traversable`와 `Monad`, 그리고 이전에 배운 개념과 타입인 `Either`, `IO` 및 예외를 사용할 것입니다.

이를 위해, 꽤 많은 모듈이 필요합니다:

```haskell
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )
```

이번에 사용할 각 함수들이 어떤 역할을 하는지 확실하지 않다면 [Hoogle](https://hoogle.haskell.org/)을 참고하세요.
타입 시그니처와 문서를 읽고 `ghci`에서 실험해보세요.

## 디렉터리 변환하기

먼저 다른 작은 함수들을 캡술화한 고차 함수인 `convertDirectory`의 설명으로 시작하겠습니다.
`convertDirectory`는 꽤 명령형적인 모습을 하고 있으며, 우리가 해야할 작업을 다른 방식으로 설명하는 것처럼 보입니다.

```haskell
-- | 특정 디렉터리의 파일을 다른 디렉터리로 복사하고, '.txt' 파일을 '.html' 파일로 변환합니다.
-- 읽기나 쓰기에 실패한 경우 stderr에 기록합니다.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."
```

여기서 우리는 각 `IO` 함수가 에러를 적절하게 처리하고, 필요할 때 프로젝트를 종료한다고 가정했습니다.

이제 단계별로 살펴보겠습니다.

### `getDirFilesAndContent`

```haskell
-- | 애플리케이션에 필요한 디렉터리 내용
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ 파일 경로와 그 내용
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
      -- ^ 다른 파일 경로, 직접 복사될 것
    }

-- | 디렉터리 내용을 반환합니다
getDirFilesAndContent :: FilePath -> IO DirContents

```

`getDirFilesAndContent`는 처리를 위한 연관도니 파일들을 제공하는 역할을 합니다. --
마크업으로 변환해야 할 파일(그리고 그들의 텍스트 내용)과 이미지나 스타일시트와 같이 그대로 복사할 다른 파일들입니다.

```haskell
-- | 디렉터리 내용을 반환합니다
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }
```

이 함수는 네 가지 중요한 일을 합니다:

1. 디렉터리 내의 모든 파일을 나열합니다
2. 파일들을 확장자에 따라 두 그룹으로 나눕니다
3. `.txt` 파일의 내용을 읽고, 파일을 읽는 데 실패한 경우 보고합니다
4. 결과를 반환합니다. 결과를 더 명확하게 하기 위해 데이터 타입을 정의했습니다.

(3)번은 나머지보다 조금 더 복잡합니다. 이를 살펴보겠습니다.

#### `applyIoOnList`

---

`applyIoOnList`는 다음과 같은 타입 시그니처를 가집니다:

```haskell
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
```

이 함수는 `IO` 함수를 특정 값들의 리스트에 적용하고, 성공과 실패를 기록합니다.

한 번 구현해보세요! 어떤 함수를 사용해야 할지 힌트가 필요하다면, 이전에 작성한 가져오기 목록을 참고하세요.

<details><summary>정답</summary>

```haskell
-- | IO 함수를 값들의 리스트에 적용하고, 성공과 실패를 기록합니다
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)
```

</details>

---

`applyIoOnList`는 특정 `IO` 함수를(이번 경우 `readFile`) 특정 값들의 리스트(이번 경우 `FilePath`)에 적용하는 고차 함수입니다.
각 요소에 대해, 요소 그 자체와 `IO` 함수를 적용한 결과를 `Either`로 변환한 값을 함께 반환합니다.
여기서 `Left` 타입은 `String`으로, 에러를 나타냅니다.

이 함수의 타입 만으로도 함수가 어떤 일을 할지에 대해 많은 것을 알 수 있습니다.
타입이 다형적이기 때문에, `a`에 대해 할 수 있는 일은 함수에 적용하는 것 뿐이고, `b`를 생성할 수 있는 곳은 함수의 결과뿐입니다.

:::note
이 함수를 처음 작성할 때는 `readFile`에만 특화되어 있었고, `[FilePath]`를 받아 `IO [(FilePath, Either String String)]`를 반환했습니다.
하지만 추후에 다른 사용 사례를 만나게 되었고(`writeFiles`와 `copyFiles`), `action`, 입력 타입, 반환 타입을 분리했습니다.
:::

이 함수는 에러를 처리하기 위해 예외를 사용하고, `Either`를 사용해 성공과 실패를 모두 타입 시스템에 표현합니다.
이를 통해 예외 처리를 함수 호출자에게 미룸과 동시에 호출자가 예외 처리를 잊지 않도록 하였습니다!

다음에는, 에러를 보고하고 실패하는 모든 경우를 필터링하는 함수를 살펴보겠습니다.

#### `filterAndReportFailures`

---

`filterAndReportFailures` has the following type signature:

```haskell
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
```

It filters out unsuccessful operations on files and reports errors to the stderr.

Try to implement it!

<details><summary>Answer</summary>

```haskell
-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]
```

This code may seem a bit surprising - how come we can use `foldMap` here? Reminder,
the type of `foldMap` is:

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

If we specialize this function for our use case, substituting the general type
with the types we are using, we learn that `IO [(a, b)]` is a monoid.
And indeed - `[a]` is a monoid for any `a` with `[]` (the empty list) as `mempty`
and `++` as `<>`, but also `IO a` is a monoid for any `a` that is itself
a monoid with `pure mempty` as `mempty` and `liftA2 (<>)` as `<>`!

Using these instances, we can `map` over the content, handle errors, and return
an empty list to filter out a failed case, or a singleton list to keep the result.
And the `fold` in `foldMap` will concatenate the resulting list where we return
all of the successful cases!

If you've written this in a different way that does the same thing, that's fine too!
It's just nice to see how sometimes abstractions can be used to write concise code.

</details>

---

These functions are responsible for fetching the right information. Next,
let's look at the code for creating a new directory.

### `createOutputDirectoryOrExit`

```haskell
-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create
```

`createOutputDirectoryOrExit` itself is not terribly exciting, it does
what it is named -- it tries to create the output directory, and exits the
program in case it didn't succeed.

`createOutputDirectory` is the function that actually does the heavy lifting.
It checks if the directory already exists, and checks if the user would like to
override it. If they do, we remove it and create the new directory; if they don't,
we do nothing and report their decision.

### `txtsToRenderedHtml`

```haskell
let
  outputHtmls = txtsToRenderedHtml filesToProcess
```

---

In this part of the code we convert files to markup and change the
input file paths to their respective output file paths (`.txt` -> `.html`).
We then build the index page, and convert everything to HTML.

Implement `txtsToRenderedHtml`, which has the following type signature:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
```

<details><summary>Hint</summary>

I implemented this by defining three functions:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
```

</details>

.

<details><summary>Answer</summary>

```haskell
-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)
```

One possibly surprising thing about this code could be the `map (fmap Html.render)`
part. We can use `fmap` on the tuple because it is a `Functor` on the second
argument, just like `Either`!

</details>

---

### `copyFiles` and `writeFiles`

The only thing left to do is to write the directory
content, after the processing is completed, to the newly created directory:

```haskell
-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures
```

Here we use `applyIoOnList` again to do something a bit more complicated,
instead of reading from a file, it copies from the input path to a newly generated
output path. Then we pass the result (which has the type `[(FilePath, Either String ())]`)
to `filterAndReportFailures` to print the errors and filter out the unsuccessful copies.
Because we are not really interested in the output of `filterAndReportFailures`,
we discard it with `void`, returning `()` as a result instead:

```haskell
-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures
```

Once again, this code looks almost exactly like `copyFiles`, but the types are different.
Haskell's combination of parametric polymorphism + type class for abstractions is really
powerful, and has helped us reduce quite a bit of code.

---

This pattern of using `applyIoOnList` and then `filterAndReportFailures`
happens more than once. It might be a good candidate for refactoring. Try it!
What do you think about the resulting code? Is it easier or more difficult to
understand? Is it more modular or less? What are the pros and cons?

---

## Summary

With that, we have completed our `HsBlog.Directory` module that is responsible for converting
a directory safely. Note that the code could probably be simplified quite a bit if we
were fine with errors crashing the entire program altogether, but sometimes this is
the price we pay for robustness. It is up to you to choose what you can live with
and what not, but I hope this saga has taught you how to approach error handling
in Haskell in case you need to.

View the full module:

<details><summary>HsBlog.Directory</summary>

```haskell
-- | Process multiple files and convert directories

module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

-- | Copy files from one directory to another, converting '.txt' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

------------------------------------
-- * Read directory content

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

-- | The relevant directory content for our application
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

------------------------------------
-- * Build index page

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 2 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Blog"
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )

------------------------------------
-- * Conversion

-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

------------------------------------
-- * Output to directory

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

------------------------------------
-- * IO work and handling errors

-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

------------------------------------
-- * Utilities

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
```

</details>
