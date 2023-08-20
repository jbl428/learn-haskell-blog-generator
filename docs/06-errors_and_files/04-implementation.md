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
-- 출력 디렉터리 생성중에 예외가 발생할 수 있습니다.
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

`filterAndReportFailures`는 다음과 같은 타입 시그니처를 가집니다:

```haskell
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
```

파일에 대한 작업의 실패를 필터링하고, stderr에 에러를 보고합니다.

한 번 구현해보세요!

<details><summary>정답</summary>

```haskell
-- | 파일에 대한 작업의 실패를 필터링하고, stderr에 에러를 보고합니다.
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

이 코드는 조금 놀라울 수 있습니다 - 어떻게 `foldMap`을 사용할 수 있을까요? 
기억을 떠올려보면, `foldMap`의 타입은 다음과 같습니다:

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

이 함수의 일반적인 타입을 이번 예제에서 사용한 타입으로 치환해서 생각해보면 `IO [(a, b)]`는 monoid라는 것을 알 수 있습니다. - `[a]`는 임의의 `a`에 대해 `[]`(빈 리스트)를 `mempty`로, `++`를 `<>`로 사용하는 monoid이며,
`IO a`는 임의의 `a`가 monoid일 때 `pure mempty`를 `mempty`로, `liftA2 (<>)`를 `<>`로 사용하는 monoid입니다!

만약 당신이 다른 방식으로 같은 동작을 하는 코드를 작성했다면, 그것도 괜찮습니다!
때때로 추상화를 사용해 더 간결한 코드를 작성할 수 있다는 점만 기억하시면 됩니다.

</details>

---

이러한 함수들은 유효한 정보를 가져오는 데 사용됩니다.
다음으로, 새 디렉토리를 만드는 코드를 살펴보겠습니다.

### `createOutputDirectoryOrExit`

```haskell
-- | 출력 디렉터리를 생성하고 실패하면 프로그램을 종료합니다.
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | 출력 디렉터리를 생성합니다.
--   디렉터리가 생성되었는지 여부를 반환합니다.
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

`createOutputDirectoryOrExit`는 이름 그대로의 일을 합니다 - 출력 디렉터리를 생성하고, 실패하면 프로그램을 종료합니다.

`createOutputDirectory`는 사실 꽤 많은 작업을 수행하는 함수입니다.
디렉터리가 이미 존재하는지 확인하고, 사용자가 덮어쓰기를 원하는지 확인합니다.
만약 덮어쓰기를 원한다면, 디렉터리를 삭제하고 새 디렉터리를 생성합니다.
만약 덮어쓰기를 원하지 않는다면, 아무것도 하지 않고 사용자의 결정을 반환합니다.

### `txtsToRenderedHtml`

```haskell
let
  outputHtmls = txtsToRenderedHtml filesToProcess
```

---

이 코드에서는 파일을 마크업으로 변환하고, 입력 파일 경로를 해당 출력 파일 경로로 변경합니다(`.txt` -> `.html`).
그리고 인덱스 페이지를 빌드하고, 모든 것을 HTML로 변환합니다.

다음 타입 시그니처를 가지는 `txtsToRenderedHtml` 함수를 구현해보세요:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
```

<details><summary>힌트</summary>

다음 세 가지 함수를 정의해서 구현할 수 있습니다:

```haskell
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
```

</details>

<details><summary>정답</summary>

```haskell
-- | 텍스트 파일을 마크업으로 변환하고, 인덱스를 빌드하고, html로 렌더링합니다.
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

이 코드에서 흥미로울 점은 `map (fmap Html.render)` 부분입니다.
튜플에 `fmap`을 사용할 수 있는 이유는, `Either`처럼 두 번째 인자에 대해 `Functor`이기 때문입니다.

</details>

---

### `copyFiles` 과 `writeFiles`

이제 남은 작업은 처리가 완료된 후 디렉터리 내용을 새로 생성된 디렉터리에 작성하는 것입니다:

```haskell
-- | 디렉터리에 파일을 복사하고, 오류를 stderr에 기록합니다.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures
```

여기서 다시 한 번 `applyIoOnList`를 사용해서 더 복잡한 작업을 수행합니다.
파일을 읽는 대신, 입력 경로에서 새로 생성된 출력 경로로 복사합니다.
그리고 결과(`[(FilePath, Either String ())]` 타입)를 `filterAndReportFailures`에 전달해서 오류를 출력하고, 복사에 실패한 것들을 필터링합니다.
`filterAndReportFailures`의 반환값에는 관심이 없기 때문에, `void`를 사용해서 버립니다.
그리고 `()`를 반환합니다:

```haskell
-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures
```

이 코드는 타입이 다르다는 점을 제외하면 `copyFiles`와 거의 동일합니다.
하스켈의 매개변수 다형성과 추상화를 위한 타입 클래스의 조합은 정말 강력하고, 많은 코드를 줄일 수 있습니다.

---

`applyIoOnList`를 호출하고 이후에 `filterAndReportFailures`를 호출하는 패턴이 다시 한 번 나왔습니다.
이는 리팩토링의 후보가 될 수 있습니다. 한 번 시도해보세요!
작성한 코드에 대해 어떻게 생각하시나요? 이전보다 이해하기 쉽나요? 더 모듈화되었나요? 장단점은 무엇인가요?

---

## 요약

이제 우리는 디렉터리를 안전하게 변환하는 역할을 하는 `HsBlog.Directory` 모듈을 완성했습니다.
코드는 아마도 오류가 전체 프로그램을 충돌시키는 것을 허용한다면 훨씬 더 단순화될 수 있었을 것입니다.
하지만 때때로 이는 견고함을 위해 지불해야 하는 대가입니다.
어떤 것을 허용할 수 있는지, 그리고 어떤 것을 허용할 수 없는지 선택하는 것은 여러분에게 달렸습니다.
지금까지의 여정이 하스켈에서 에러 처리를 어떻게 접근해야 하는지에 대해 배울 수 있었기를 바랍니다.

전체 모듈 코드:

<details><summary>HsBlog.Directory</summary>

```haskell
-- | 여러 파일을 처리하고 디렉토리를 변환합니다

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

-- | 특정 디렉터리의 파일을 다른 디렉터리로 복사하고, '.txt' 파일을 '.html' 파일로 변환합니다.
-- 읽기나 쓰기에 실패한 경우 stderr에 기록합니다.
--
-- 출력 디렉터리 생성중에 예외가 발생할 수 있습니다.
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
-- * 디렉터리 내용 읽기

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

-- | 애플리케이션에 필요한 디렉터리 내용
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

------------------------------------
-- * 인덱스 페이지 생성

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
-- * 변환

-- | 텍스트 파일을 마크업으로 변환하고, 인덱스를 빌드하고, html로 렌더링합니다.
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
-- * 디렉터리에 출력

-- | 출력 디렉터리를 생성하고 실패하면 프로그램을 종료합니다.
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | 출력 디렉터리를 생성합니다.
--   디렉터리가 생성되었는지 여부를 반환합니다.
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

-- | 디렉터리에 파일을 복사하고, 오류를 stderr에 기록합니다.
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
-- * IO 작업과 오류 처리

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

-- | 파일에 대한 작업의 실패를 필터링하고, stderr에 에러를 보고합니다.
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
-- * 유틸리티

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "잘못된 응답입니다. y 또는 n을 사용하세요."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
```

</details>
