# 프로젝트의 명세를 정의하기

지금까지 우리는 `base`와 GHC와 함께 제공되는 [라이브러리](https://downloads.haskell.org/ghc/9.2.5/docs/html/users_guide/9.2.5-notes.html#included-libraries)만 사용했습니다.
그래서 프로그램을 실행하기 위해 `runghc`보다 더 복잡한 것을 할 필요가 없었습니다.
하지만 이번에는 GHC에 포함되지 않은 외부 라이브러리를 사용할 것입니다.

외부 패키지는 [Hackage](https://hackage.haskell.org/) - 하스켈의 중앙 패키지 저장소, [Stackage](https://www.stackage.org/) - 함께 작동하는 것으로 알려진 Hackage 패키지의 하위 집합, 또는 원격 git 저장소에서 다운로드할 수 있습니다.
보통 하스켈러들은 여러 프로젝트를 위해 패키지를 다운로드하고 관리하기 위해 **패키지 관리자**를 사용합니다. 
하스켈에서 가장 인기있는 패키지 관리자는 [cabal](https://cabal.readthedocs.io)과 [stack](https://haskellstack.org)입니다.

두 패키지 관리자 간의 주요 차이점은 철학입니다.
`cabal`은 하스켈 프로젝트를 빌드하고, Hackage 전체를 사용하여 패키지 관리를 수행하며, 패키지가 함께 작동하도록 복잡한 알고리즘을 사용하려고 노력하는 최소한의 도구를 만들려고 노력합니다.
`stack`은 각 프로젝트에 맞는 올바른 GHC를 설치하고, hoogle과 같은 외부 도구와 통합을 제공하며, 사용자가 사용할 패키지의 '집합'을(버전을 포함해서) 선택할 수 있도록 하는 등 최대한의 도구를 만들려고 노력합니다.

만약 GHCup을 사용해 하스켈을 설치했다면, `cabal`이 이미 설치되어 있을 것입니다.
만약 stack을 사용해 하스켈을 설치했다면, `stack`이 이미 설치되어 있을 것입니다.
만약 그렇지 않다면, [haskell.org downloads page](https://www.haskell.org/downloads/)를 확인해 보세요.

## 프로젝트 만들기

외부 패키지를 사용하는 방법은 여러 가지가 있습니다.
빠른 실험을 위해, 외부 패키지를 사용하여 프로그램을 빌드하거나 실행할 수 있도록 [stack이나 cabal에게 요청](https://gilmi.me/blog/post/2021/08/14/hs-core-tools#using-external-packages-in-ghci)할 수 있습니다.
하지만 프로그램이 커지고, 더 많은 의존성을 사용하고, 더 많은 기능이 필요할수록, 프로그램과 라이브러리를 위한 **프로젝트 명세**를 만드는 것이 좋습니다.

프로젝트 명세는 **cabal 파일**에 작성됩니다.
`cabal init --libandexe`나 `stack new`를 사용하여 cabal이나 stack에게 명세를 생성하도록 요청할 수 있지만, 나중에 파일을 직접 편집해야 할 수도 있습니다.
지금은 단순히 `hs-blog.cabal`에 다음 예제를 붙여넣고 편집합시다.

```cabal
cabal-version:       2.4

name:                이름은 <name>.cabal과 일치해야 합니다.
version:             버전은 PvP를 사용해야 합니다.
synopsis:            시놉시스는 hackage 패키지 목록과 검색에 표시됩니다.
description:         설명은 라이브러리의 상단에 표시됩니다.
homepage:            홈페이지 url
bug-reports:         이슈 추적기 url
license:             라이선스 이름
license-file:        라이선스 파일
author:              작성자 이름
maintainer:          메인테이너 이메일
category:            쉼표로 구분된 Hackage 카테고리
extra-doc-files:
  README.md

common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall

library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:

executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , <package-name>
  ghc-options:
    -O
```

각 항목에 대한 자세한 내용을 하나씩 살펴보겠습니다.

- [package metadata](#package-metadata)
- [common settings](#common-settings)
- [library](#library)
- [executable](#executable).

### Package metadata

패키지 메타데이터 항목들은 주석 내용만 보고도 어떤 내용인지 직관적으로 알 수 있습니다.
다만 다음 항목들에 대해서는 좀 더 살펴볼 필요가 있습니다.

- `cabal-version`: 어떤 cabal 버전으로 이 프로젝트를 빌드할 수 있는지 정의합니다. 예제에서는 2.4 이상을 지정했습니다.
  각 버전에 대한 정보는 [문서](https://cabal.readthedocs.io/en/stable/file-format-changelog.html)를 참고하세요.
- `name`: 라이브러리와 패키지의 이름을 정의합니다. `.cabal` 파일의 이름과 일치해야 합니다. 보통 소문자로 시작합니다.
  [Hackage에서 패키지 이름이 이미 사용중인지 확인하세요](https://hackage.haskell.org/packages/search?terms=name).
- `version`: 일부 하스켈 패키지는 [semver](https://semver.org/)을 사용하지만, 대부분 [PvP](https://pvp.haskell.org/)를 사용합니다.
- `license`: Most Haskell packages use [BSD-3-Clause](https://choosealicense.com/licenses/bsd-3-clause/). [Neil Mitchell blogged about this](https://neilmitchell.blogspot.com/2018/08/licensing-my-haskell-packages.html). You can find more licenses if you'd like at [choosealicense.com](https://choosealicense.com).
- `license`: 대부분의 하스켈 패키지는 [BSD-3-Clause](https://choosealicense.com/licenses/bsd-3-clause/)를 사용합니다.
  이에 대한 [Neil Mitchell의 글](https://neilmitchell.blogspot.com/2018/08/licensing-my-haskell-packages.html)을 참고하세요.
  더 많은 라이선스를 찾으려면 [choosealicense.com](https://choosealicense.com)을 참고하세요.
- `extra-doc-files`: `README`나 `CHANGELOG`와 같은 추가 문서 파일을 포함합니다.

우리 프로젝트에 맞게 각 항목을 채워넣어봅시다.

```cabal
cabal-version:       2.4

name:                hs-blog
version:             0.1.0.0
synopsis:            마크업 파일을 통한 커스텀 블로그 생성기
description:         이 패키지는 커스텀 마크업 형식의 파일을 HTML로 변환하는
                     정적 블로그 생성기를 제공합니다.
                     이 커스텀 마크업 형식의 파서와 HTML 프리티 프린터 EDSL을 정의합니다.

                     이 패키지는 온라인 책 'Learn Haskell Blog Generator'의 예제 프로젝트로 사용됩니다.
                     자세한 내용은 README를 참고하세요.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             BSD-3-Clause
license-file:        LICENSE.txt
author:              Gil Mizrahi
maintainer:          gilmi@posteo.net
category:            Learning, Web
extra-doc-files:
  README.md
```

### Common settings

Cabal package descriptions can include multiple "targets": libraries, executables,
and test suites. Since Cabal 2.2, we can use
[common stanzas](https://cabal.readthedocs.io/en/stable/cabal-package.html#common-stanzas)
to group settings to be shared between different targets, so we don't have to repeat them for each target.

In our case we've created a new common stanza (or block) called `common-settings` and
defined the default language (Haskell has two standards, 98 and 2010),
and instructed GHC to compile with `-Wall`.

```cabal
common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall
```

Later, in our targets' descriptions, we can add `import: common-settings` ,
and all of these settings will be automatically added.

### Library

In a `library` target, we define:

- The settings with which to build the library (in this case we just import `common-settings`)
- The directory in which the source files can be found
- The packages we require to build the library
- The modules exposed from the library and can be used by others
- The modules _not_ exposed from the library and which _cannot_ be used by others;
  these could be any module you don't wish to export, such as an internal utility
  functions module.
  In our case we don't have anything like this, so we commented out the `other-modules`
  label.

Note that it is common to specify **version bounds** for packages.
Version bounds specify _which package versions this library works with_.
These can also be generated using cabal with the `cabal gen-bounds` command.

```cabal
library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:
```

Also note that we've added an additional _hierarchy_ for our modules and defined
a different source directory. This means we will need to move the files around
a bit and change the `module` name in each file and the `import` statements. This is to avoid
conflict with other packages that a user might import.

---

Do this now.

<details><summary>Solution</summary>

1. `Main.hs` -> `src/HsBlog.hs`

   ```haskell
   module HsBlog
     ( main
    , process
    )
     where

   import qualified HsBlog.Markup as Markup
   import qualified HsBlog.Html as Html
   import HsBlog.Convert (convert)
   ```

2. `Convert.hs` -> `src/HsBlog/Convert.hs`

   ```haskell
   module HsBlog.Convert where

   import qualified HsBlog.Markup as Markup
   import qualified HsBlog.Html as Html
   ```

3. `Html.hs` -> `src/HsBlog/Html.hs`

   ```haskell
   module HsBlog.Html
   ...

   import HsBlog.Html.Internal
   ```

4. `Html/Internal.hs` -> `src/HsBlog/Html/Internal.hs`

   ```haskell
   module HsBlog.Html.Internal where
   ```

5. `Markup.hs` -> `src/HsBlog/Markup.hs`

   ```haskell
   module HsBlog.Markup
   ```

</details>

---

### Executable

We have separated our code into two sections: a library and an executable, why?

First, libraries can be used by others. If we publish our code and someone wants to
use it and build upon it, they can. Executables can't be imported by other projects.
Second, we can write unit tests for libraries. It is usually
beneficial to write most, if not all, of our logic as a library, and provide
a thin executable over it.

Executables' descriptions are very similar to libraries, here we define:

- The name of the executable
- Where the source directory for this application is
- Which file is the 'Main' file
- Import our library, which is named `hs-blog`
- Additional flags for GHC, e.g., `-O` to compile with optimizations

```cabal
executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , hs-blog
  ghc-options:
    -O
```

We can write many executables descriptions. In this case we only have one.

---

**Exercise**: Add a new file: `app/Main.hs` which imports `HsBlog` and runs `main`.

<details><summary>Solution</summary>

```haskell
-- app/Main.hs

module Main where

import qualified HsBlog

main :: IO ()
main = HsBlog.main
```

</details>

---

### Test-suites

`test-suite` defines a target for running package tests. We will get back to it
in a later chapter.

## Our complete .cabal file

```cabal
cabal-version:       2.4

name:                hs-blog
version:             0.1.0.0
synopsis:            A custom blog generator from markup files
description:         This package provides a static blog generator
                     from a custom markup format to HTML.
                     It defines a parser for this custom markup format
                     as well as an html pretty printer EDSL.

                     It is used as the example project in the online book
                     'Learn Haskell Blog Generator'. See the README for
                     more details.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             BSD-3-Clause
license-file:        LICENSE.txt
author:              Gil Mizrahi
maintainer:          gilmi@posteo.net
category:            Learning, Web
extra-doc-files:
  README.md

common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall

library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:

executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , hs-blog
  ghc-options:
    -O
```

We'll also add a `README.md` file and a `LICENSE.txt` file:

<details><summary>README.md</summary>

Just write whatever you want here:

```md
# hs-blog

One day it will be a static blog generator.

[Read the book](https://lhbg-book.link).
```

</details>

<details><summary>LICENSE.txt</summary>

This is BSD-3-Clause with me as the author. Please write your own name for your projects :)

```
BSD 3-Clause License

Copyright (c) 2021-2022, Gil Mizrahi
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

</details>

## `cabal.project` and `stack.yaml`

The [cabal.project](https://cabal.readthedocs.io/en/stable/cabal-project.html) and
[stack.yaml](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config)
files are used by `cabal` and `stack` respectively to add additional information on _how
to build the package_. While `cabal.project` isn't necessary to use `cabal`, `stack.yaml`
is necessary in order to use `stack`, so we will cover it briefly.

There are two important fields a `stack.yaml` file must have:

- `resolver`: Describes which snapshot to use for packages and ghc version.
  We will choose the latest (at time of writing) on the `lts` branch: `lts-18.22`.
  Visit [this link](https://www.stackage.org/lts-18.22) to find out which packages this
  snapshot includes, what their versions are, and which GHC version is used
  with this snapshot
- `packages`: Describes the location of packages we plan to build. In our case
  we have only one and it can be found in the current directory

We'll add `stack.yaml` to our project directory:

```yaml
resolver: lts-18.22

packages:
  - .
```

For additional options and configurations, please consult the relevant user guides.

## Usage

Now, instead of manually running `runghc Main.hs`, we will use either `stack`
or `cabal` to build and run our program and package (I mostly use stack, but it's up to you).

### For cabal:

Building the project - on the first run, cabal will download the package dependencies
and use the GHC on PATH to build the project.

Cabal caches packages between projects, so if a new project uses the same packages
with the same versions (and the same flag settings) they will not need to be reinstalled.

> In older versions of cabal, packages could be installed either globally, or in sandboxes.
> In each sandbox (and globally) there could only be one version of a package installed,
> and users would usually create different sandboxes for different projects, without caching
> packages between projects.
>
> With the new build system implementation, multiple versions of the same package can be
> installed globally, and for each project cabal will (try to) choose a specific version for each
> package dependency such that they all work together, without needing sandboxing.
> This change helps us increase sharing of built packages while avoiding conflicts and manual
> handling of sandboxes.

> Note: The new build system implementation is now the default and Cabal commands do not need
> to be prefixed with `v2-`, but the Cabal documentation will still mention the prefix to
> refer to the new commands.

A few important commands we should be familiar with:

```sh
cabal update
```

[`update`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-update)
fetches information from remote package repositories (specifically Hackage unless specified otherwise)
and updates the local package index which includes various information about available packages such as
their names, versions and dependencies.

`cabal update` is usually the first command to run before fetching package dependencies.

```sh
cabal build
```

[`build`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-build)
compiles the various targets (such as `library` and `executable`s).
It will also fetch and install the package dependencies when they're not already installed.

When building executables, `cabal build` will report where the executable has been created,
and it is also possible to find the path to the executable using `cabal exec -- which hs-blog-gen`.

```sh
cabal run hs-blog-gen -- <program arguments>
```

[`run`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-run)
Can be used to compile and then run a target (in our case our `executable` which we named `hs-blog-gen`).
We separate arguments passed to `cabal` and arguments passed to our target program with `--`.

```sh
cabal repl hs-blog
```

[`repl`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-repl)
runs `ghci` in the context of the target (in our case our `library` which we named `hs-blog`) -
it will load the target's package dependencies and modules to be available in `ghci`.

```sh
cabal clean
```

[`clean`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-clean)
Deletes the build artifacts that we built.

There are more interesting commands we could use, such as `cabal freeze` to generate
a file which records the packages versions and flags we used to build this project,
and `cabal sdist` to bundle the project source to a package tarball which can be
uploaded to Hackage. If you'd like to learn more visit the
[Cabal user guide](https://cabal.readthedocs.io/en/stable/cabal-commands.html).

### For stack:

Building the project - on the first run, stack will install the right GHC for this project
which is specified by the `resolver` field in the `stack.yaml` file,
download the package dependencies, and compile the project.

Stack caches these installations between projects that use the same resolver,
so future projects with the same resolver and future runs of this project won't
require reinstallation. This approach is kind of a middle ground between full packages
sharing and sandboxes.

Let's look at the (somewhat) equivalent commands for Stack:

```sh
stack build
```

[`build`](https://docs.haskellstack.org/en/stable/build_command/#build-command)
will compile the project as described above - installing GHC and package dependencies if they are not
installed.

When building executables, `stack build` will report where the executable has been created,
and it is also possible to find the path to the executable using `stack exec -- which hs-blog-gen`.

```sh
stack exec hs-blog-gen -- <program arguments>
```

[`exec`](https://docs.haskellstack.org/en/stable/GUIDE/#stack-exec)
will run the executable passing the program arguments to our executable.

```sh
stack ghci hs-blog
```

[`ghci`](https://docs.haskellstack.org/en/stable/ghci/#ghci)
runs `ghci` in the context of our library `hs-blog` - loading the library modules
and packages.

```sh
stack clean
```

[`clean`](https://docs.haskellstack.org/en/stable/GUIDE/#cleaning-your-project)
cleans up build artifacts.

The [Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/) contains more
information about how stack works and how to use it effectively.

### Build artifacts

Both stack and cabal create build artifacts that we will not want to track using
our version control. These build artifacts are found in the `dist`, `dist-newstyle`
and `.stack-work` directories. We can add these to a `.gitignore` file
(or similar for other version control programs) to ignore them:

```txt
dist
dist-newstyle
.stack-work
```

## Finding packages

Finding packages isn't a very straightforward process at the moment.
People have written on
[how they choose packages](https://www.haskellforall.com/2018/05/how-i-evaluate-haskell-packages.html),
[recommendation lists](https://github.com/soupi/haskell-study-plan#useful-packages), [books](https://leanpub.com/haskell-stdlibs), and more.

My suggestion is:

- Search for a tutorial on something you'd like to do, and see which packages come up
- Use the download amount on Hackage as an indication of package popularity
- Use [Stackage](https://www.stackage.org/lts) package synopses to locate a relevant package
- Check social network channels for recommendations, but know that sometimes people tend
  to recommend inappropriate solutions and packages that might be too complicated or
  still experimental

It's also important to note the amount of dependencies a package has. Adding many dependencies
will affect compilation time and code size. And it can sometimes be a good thing to consider
when comparing packages, or considering whether a package is needed at all.

## Summary

We've created a package description for our library and used `stack` and/or `cabal`
to build our program. In future chapters we'll start adding external packages,
we'll only have to add them to the `build-depends` section in the cabal file and
our package manager will download and install the required package for us!

We've made some change to our project directory, and it should now look like this:

```
.
├── app
│   └── Main.hs
├── hs-blog.cabal
├── LICENSE.txt
├── README.md
├── src
│   ├── HsBlog
│   │   ├── Convert.hs
│   │   ├── Html
│   │   │   └── Internal.hs
│   │   ├── Html.hs
│   │   └── Markup.hs
│   └── HsBlog.hs
└── stack.yaml

4 directories, 10 files
```

Note that this package format could be released on [Hackage](https://hackage.haskell.org/)
for other Haskell developers to use!

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/8ca58aef80930db82cd20e85f44f5e34e1d74214)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/8ca58aef80930db82cd20e85f44f5e34e1d74214).
