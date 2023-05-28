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


Cabal 패키지 설명에는 라이브러리, 실행 파일, 테스트 스위트 등 여러 "타겟"을 포함할 수 있습니다.
Cabal 2.2 이후로는 [공통 stanza](https://cabal.readthedocs.io/en/stable/cabal-package.html#common-stanzas)를 사용하여
다른 타겟들 사이에서 공유할 설정을 그룹화할 수 있습니다. 이렇게 하면 각 타겟마다 설정을 반복할 필요가 없습니다.

예제에서는 `common-settings`라는 새로운 공통 stanza를 만들었습니다.
여기에는 기본 언어(Haskell은 98과 2010 두 가지 표준이 있습니다)설정과 GHC에 `-Wall` 옵션을 주어 컴파일하도록 지시합니다.

```cabal
common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall
```

이후 타켓 설정에서 `import: common-settings`를 추가하면 이 설정들이 자동으로 추가됩니다.

### Library

`library` 명세에는 다음 항목들을 정의합니다:

- 라이브러리를 빌드하는 데 필요한 설정(우리는 `common-settings`를 import합니다)
- 소스 파일이 위치한 디렉토리
- 라이브러리를 빌드하는 데 필요한 패키지
- 라이브러리에서 외부에 노출할 모듈
- 라이브러리에서 외부에 노출하지 않을 모듈, 이 모듈들은 외부에서 사용할 수 없습니다.
  이는 내부 유틸리티 함수 모듈과 같은 모듈을 노출하지 않을 때 사용합니다.
  예제에서는 내부 유틸리티 함수 모듈이 없으므로 `other-modules` 라벨을 주석 처리했습니다.

보통 패키지의 **버전 범위(version bounds)**를 지정하는 것이 일반적입니다.
버전 범위는 _이 라이브러리가 동작하는 패키지 버전_을 지정합니다.
이는 `cabal gen-bounds` 명령으로 생성할 수도 있습니다.

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

모듈을 위한 추가적인 _계층_을 정의했으며, 기존과 다른 소스 디렉토리를 지정했음을 주목하세요.
따라서 파일들을 약간 옮겨야 하고 각 파일의 `module` 이름과 `import` 문을 변경해야 합니다.
이는 사용자가 import할 다른 패키지와 충돌을 피하기 위함입니다.

---

위 작업을 진행해보세요.

<details><summary>정답</summary>

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

우리는 코드를 라이브러리와 실행 파일, 두 부분으로 나누었습니다. 그 이유는 무엇일까요?

첫째, 라이브러리는 다른 사람들이 사용할 수 있습니다. 만약 우리가 코드를 공개하면 누군가는 이를 사용하고 더 발전시킬 수 있습니다.
하지만 실행 파일은 다른 프로젝트에서 import할 수 없습니다.
둘째, 라이브러리에 대해 유닛 테스트를 작성할 수 있습니다. 보통 대부분의 로직을 라이브러리로 작성하고, 그 위에 간단한 실행 파일을 제공하는 것이 유용합니다.

Executable 명세는 libraries 항목과 매우 유사합니다. 예제에서는 다음 항목들을 정의합니다:

- 실행 파일의 이름
- 소스 디렉토리
- 'Main'을 정의한 파일
- `hs-blog`로 이름지은 우리 라이브러리를 import
- GHC에 전달할 추가 옵션, 예를 들어 `-O`를 주어 최적화를 하도록 합니다.

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

`executable` 명세에는 여러 실행 파일을 정의할 수 있습니다. 예제에서는 하나만 정의했습니다.

---

**연습문제**: 새로운 파일을 추가하세요: `app/Main.hs`. 이 파일은 `HsBlog`를 import하고 `main`을 실행합니다.

<details><summary>정답</summary>

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

패키지의 테스트를 실행하기 위한 명세를 정의합니다. 이후 장에서 다룰 예정입니다.

## 최종 .cabal 파일

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

`README.md` 파일과 `LICENSE.txt` 파일도 추가할 수 있습니다:

<details><summary>README.md</summary>

원하는 내용을 넣을 수 있습니다.

```md
# hs-blog

이 프로그램은 언젠가 정적 블로그 생성기가 될 것입니다.

[이 책을 읽어보세요](https://lhbg-book.link).
```

</details>

<details><summary>LICENSE.txt</summary>

예제에서는 저자의 이름으로 BSD-3-Clause를 적용했습니다. 여러분의 프로젝트에는 여러분의 이름을 적어주세요 :)

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

## `cabal.project`와 `stack.yaml`

[cabal.project](https://cabal.readthedocs.io/en/stable/cabal-project.html)와
[stack.yaml](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config)
파일은 각각 `cabal`과 `stack`에서 사용합니다. 이는 _패키지를 빌드하는 방법_에 대한 추가적인 정보를 제공합니다.
`cabal.project`파일은 `cabal`을 사용하기 위해 반드시 필요하지는 않지만, `stack.yaml`파일은 `stack`을 사용하기 위해 반드시 필요합니다.

`stack.yaml`파일에는 다음 두 중요한 필드가 있습니다:

- `resolver`: 패키지와 GHC 버전에 대한 스냅샷을 정의합니다.
  이번에는 (글 작성시점 기준) `lts` 브랜치의 가장 최신 버전(`lts-18.22`)을 사용할 것입니다.
  [이 링크](https://www.stackage.org/lts-18.22)에서 이 스냅샷에 포함된 패키지들과 그 버전, 그리고 GHC 버전을 확인할 수 있습니다.
- `packages`: 빌드할 패키지들의 위치를 정의합니다. 이번에는 현재 디렉토리에 있는 하나의 패키지만 사용할 것입니다.

프로젝트 디렉토리에 `stack.yaml`파일을 추가합니다:

```yaml
resolver: lts-18.22

packages:
  - .
```

추가 옵션과 설정에 대한 자세한 내용은 각각의 사용자 가이드를 참고하세요.

## 사용법

이제 직접 `runghc Main.hs`을 수행하지 않고, `stack` 또는 `cabal`을 사용하여 프로그램을 빌드하고 실행할 것입니다.
(저는 주로 `stack`을 사용하지만, 어떤 것을 사용하든 상관없습니다.)

### cabal을 사용하는 경우

프로젝트 빌드하기 - 처음 실행시, cabal은 패키지 의존성을 다운로드하고 PATH에 있는 GHC를 사용하여 프로젝트를 빌드합니다.

cabal은 각 프로젝트 패키지를 캐시하기에, 새로운 프로젝트가 같은 패키지와 같은 버전을 사용한다면
(그리고 같은 플래그 설정을 사용한다면) 패키지를 다시 설치할 필요가 없습니다.

> 이전 버전의 cabal에서는 패키지를 전역 또는 샌드박스에 설치할 수 있었습니다.
> 각 샌드박스(그리고 전역)에는 하나의 패키지 버전만 설치할 수 있었고,
> 사용자들은 보통 프로젝트마다 다른 샌드박스를 만들었고, 프로젝트 간에 패키지를 공유하지 않았습니다.
>
> 새로운 빌드 시스템으로 인해, 같은 패키지의 여러 버전을 전역으로 설치할 수 있게 되었고,
> 각 프로젝트는 샌드박스를 적용할 필요없이 패키지 의존성들이 모두 함께 동작하도록 특정 버전을 선택할 수 있습니다.
> 이 변경으로 인해 패키지 공유가 증가하고, 충돌을 피하고 샌드박스를 수동으로 관리할 필요가 없어졌습니다.

> 참고: 새로운 빌드 시스템 구현은 이제 기본값이며, cabal 명령어는 `v2-`로 접두사를 붙이지 않아도 됩니다.
> 하지만 cabal 문서에서는 여전히 새 명령어를 참조하기 위해 접두사를 언급할 것입니다.

익혀두면 좋은 몇 가지 중요한 명령어가 있습니다:

```sh
cabal update
```

[`update`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-update)
명령어는 원격 패키지 저장소(기본값은 Hackage)에서 정보를 가져와 로컬 패키지 인덱스를 업데이트합니다.
이 인덱스에는 패키지 이름, 버전, 의존성 등 패키지에 대한 다양한 정보가 포함됩니다.

`cabal update`는 보통 패키지 의존성을 가져오기 전에 실행하는 명령어입니다.

```sh
cabal build
```

[`build`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-build)
명령어는 (`library`와 `executable` 등의) 다양한 타겟들을 컴파일합니다.
또한 기존에 설치되지 않은 패키지 의존성들을 가져와 설치합니다.

실행파일을 빌드하면, `cabal build`는 실행파일이 생성된 위치를 알려줍니다.
또한 `cabal exec -- which hs-blog-gen`과 같은 명령어를 사용하여 실행파일의 경로를 찾을 수 있습니다.

```sh
cabal run hs-blog-gen -- <프로그램 인자>
```

[`run`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-run)
명령어는 타겟(예제에서는 `hs-blog-gen`이라는 이름을 붙인 `executable`)을 컴파일한 다음 실행합니다.
`--` 을 사용하면 `cabal`에게 전달한 인자와 타겟 프로그램에 전달한 인자를 구분할 수 있습니다.

```sh
cabal repl hs-blog
```

[`repl`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-repl)
명령어는 타겟의(예제에서는 `hs-blog`라는 이름을 붙인 `library`) `ghci`를 실행합니다.
이는 타겟의 패키지의 의존성과 모듈을 `ghci`에서 사용할 수 있도록 로드합니다.

```sh
cabal clean
```

[`clean`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-clean)
명령어는 빌드한 빌드 아티팩트를 삭제합니다.

cabal에는 이 외에도 흥미로운 명령어들이 있습니다. 예를 들어, `cabal freeze`는
프로젝트를 빌드할 때 사용한 패키지 버전과 플래그를 기록하는 파일을 생성하고,
`cabal sdist`는 프로젝트 소스를 패키지 tarball로 묶어 Hackage에 업로드할 수 있습니다.
더 많은 내용을 알고싶다면 [Cabal 사용자 가이드](https://cabal.readthedocs.io/en/stable/cabal-commands.html)를 참고하세요.

### stack을 사용하는 경우

프로젝트 빌드하기 - 처음 실행할 때, stack은 이 프로젝트를 위한(`stack.yaml` 파일의 `resolver`에 명시한) GHC를 설치하고 의존성 패키지를 다운로드하고 프로젝트를 컴파일합니다.

stack은 이러한 설치를 캐싱하여 같은 resolver를 사용하는 이후 프로젝트에서는 재설치없이 캐시를 사용합니다.
이는 완전한 패키지 공유와 샌드박스 사이의 중간 정도의 접근법입니다.

이제 (cabal 명령어와 비슷한) stack 명령어를 살펴보겠습니다:

```sh
stack build
```

[`build`](https://docs.haskellstack.org/en/stable/build_command/#build-command)
명령어는 cabal에서 언급한 것과 동일한 타겟을 컴파일합니다. - 만약 설치되어 있지 않다면 GHC와 패키지 의존성을 설치합니다.

실행파일을 빌드하면, `stack build`는 실행파일이 생성된 위치를 알려줍니다.
또한 `stack exec -- which hs-blog-gen`과 같은 명령어를 사용하여 실행파일의 경로를 찾을 수 있습니다.

```sh
stack exec hs-blog-gen -- <프로그램 인자>
```

[`exec`](https://docs.haskellstack.org/en/stable/GUIDE/#stack-exec)
명령어는 프로그램 인자를 전달해 실행파일을 실행합니다.

```sh
stack ghci hs-blog
```

[`ghci`](https://docs.haskellstack.org/en/stable/ghci/#ghci)
명령어는 라이브러리 모듈과 패키지를 `ghci`에서 사용할 수 있도록 로드합니다.

```sh
stack clean
```

[`clean`](https://docs.haskellstack.org/en/stable/GUIDE/#cleaning-your-project)
명령어는 빌드 아티팩트를 삭제합니다.

[Stack 사용자 가이드](https://docs.haskellstack.org/en/stable/GUIDE/)에는 stack이 어떻게 동작하는지와 효과적으로 사용하는 방법에 대한 더 많은 정보가 있습니다.

### 빌드 아티팩트

stack과 cabal은 빌드 아티팩트를 생성합니다. 이 빌드 아티팩트는 보통 버전 관리에 포함하지 않습니다.
빌드 아티팩트는 `dist`, `dist-newstyle` 그리고 `.stack-work` 디렉토리에 있습니다.
우리는 이들을 `.gitignore` 파일(다른 버전관리 프로그램을 사용한다면 이와 비슷한 파일)에 추가하여 무시할 수 있습니다.

```txt
dist
dist-newstyle
.stack-work
```

## 패키지 찾기

사용할 패키지를 찾는것은 지금 당장은 쉽지 않은 과정입니다.
여러 사람들이 작성한 [패키지를 어떻게 선택하는가](https://www.haskellforall.com/2018/05/how-i-evaluate-haskell-packages.html),
[추천 리스트](https://github.com/soupi/haskell-study-plan#useful-packages), [책](https://leanpub.com/haskell-stdlibs) 등을 참고할 수 있습니다.

저의 제안은 다음과 같습니다:

- 흥미로운 튜토리얼을 찾아보고, 어떤 패키지가 사용되었는지 확인합니다.
- Hackage의 다운로드 수를 확인합니다.
- 관련 패키지를 찾기위해 [Stackage](https://www.stackage.org/lts)의 패키지 시놉시스를 확인합니다.
- 소셜 네트워크 채널에서 사람들의 추천을 확인합니다. 하지만 때때로 사람들은 적절하지 않은 솔루션과, 너무 복잡하거나 실험적인 패키지를 추천하기도 합니다.

패키지의 의존성 개수를 확인하는 것도 중요합니다. 많은 의존성을 추가하면 컴파일 시간과 코드 크기에 영향을 미칩니다.
때때로 패키지를 비교하거나 패키지가 필요한지 고려할 때 사용할 수 있는 좋은 방법입니다.

## 요약

지믁까지 라이브러리를 위한 패키지 명세를 만들었고, `stack` 또는 `cabal`을 사용하여 프로그램을 빌드했습니다.
이후 장에서는 본격적으로 외부 패키지를 추가할 것인데, `cabal` 파일의 `build-depends` 섹션에만 추가하면 됩니다.
그러면 패키지 관리자가 필요한 패키지를 다운로드하고 설치할 것입니다!

우리는 프로젝트 디렉토리 구조를 변경하였는데 다음과 같습니다:

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

이 패키지 포맷은 다른 하스켈 개발자들이 사용할 수 있도록 [Hackage](https://hackage.haskell.org/)에 배포될 수 있습니다!


> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/8ca58aef80930db82cd20e85f44f5e34e1d74214)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/8ca58aef80930db82cd20e85f44f5e34e1d74214) 를 확인할 수 있습니다.
