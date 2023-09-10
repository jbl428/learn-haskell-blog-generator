# 자주 묻는 질문

> 질문이 있으신가요? [토론 게시판](https://github.com/soupi/learn-haskell-blog-generator/discussions) 이나 [이슈 트래커](https://github.com/soupi/learn-haskell-blog-generator/issues)를 방문해주세요!

## 일반적인 질문

### 하스켈을 배워야 하는 이유

저는 이 주제에 대한 몇가지 글을 썼습니다:

- [하스켈 고려하기](https://gilmi.me/blog/post/2020/04/28/consider-haskell) (대체 제목, '하스켈로 무엇을 할 수 있나요?')
- [하스켈을 통해 배운 7가지](https://gilmi.me/blog/post/2022/12/13/learned-from-haskell)

### 에디터 도구를 설치하는 방법

제가 알기로는, 현재 하스켈 개발을 위해 가장 권장되는 설정은
VSCode 또는 [VSCodium](https://vscodium.com/)을 사용하고
[하스켈 확장](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)을 설치하는 것입니다.

이 확장은 [haskell-language-server](https://github.com/haskell/haskell-language-server)
를 사용하며, [GHCup](https://www.haskell.org/ghcup/) 또는 하스켈 확장 자체를 통해 설치할 수 있습니다.

만약 선호하는 에디터가 이미 있다면,
[HLS를 지원하는지 확인](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor)하거나,
대안으로 에디터에서 독립적으로 빠른 피드백을 제공하는 [GHCid](https://github.com/ndmitchell/ghcid#readme)를 사용할 수 있습니다.

### 새로운 것을 배우는 방법

하스켈 커뮤니티는 매우 활발하며, 새로운 라이브러리, 도구 및 기술을 개발하고 있습니다.
[하스켈 planetarium](https://haskell.pl-a.net)은 여러 커뮤니티의 피드를 한 페이지에 모아두었으며,
[하스켈 주간 뉴스레터](https://haskellweekly.news/)도 있습니다.
또한 [Fediverse](https://fosstodon.org/tags/haskell)에서도 많은 하스켈 관련 내용을 찾을 수 있습니다!

## 디버깅

### 하스켈 코드를 디버깅하는 방법

명령형 언어의 대부분은 단계별 디버거를 제공합니다.
반면에 하스켈은
[GHCi 디버거](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger)
가 있긴 하지만, 하스켈의 지연 평가 때문에 예상한 순서대로 평가되지 않을 수 있어서 사용하기가 그리 쉽지 않습니다.
그래서 하스켈러들은
[추적 디버깅](https://hackage.haskell.org/package/base-4.16.4.0/docs/Debug-Trace.html#g:1)과 등식 추론을 활용합니다.
추적 디버깅을 사용하면 코드에 대한 *우리의 가정을 검증*해볼 수 있습니다.
코드의 어디서든 변수, 함수 입력, 함수 출력을 출력하거나 단순히 "이곳에 도달했다"는 정보를 확인할 수 있습니다.

예상하지 못한 함수의 입력과 출력과 같은 *우리의 가정을 위반하는 것*을 찾으면,
우리는 코드의 어느 부분에 문제가 있는지 파악하거나 추적 디버깅을 다시 사용하여 정확한 위치를 찾을 수 있습니다.
그리고 우리의 추론과 벗어난 코드를 평가하기 위해 "등식 추론"을 사용할 수 있습니다.
만약 쉽게 할 수 있다면, `ghci`에서 함수에 다른 입력을 전달해 우리의 가정을 확인할 수도 있습니다.

하스켈은 불변성, 합성 가능성, 타입을 사용하여 많은 오류를 제거합니다.
이는 "지역 추론"을 가능하게 하고, 추적 디버깅이 하스켈 프로그램을 디버깅하는 데 유용하도록 만듭니다.

### 타입 에러를 이해하는 방법

GHC 타입 에러는 종종 친절하지 못한 메시지를 출력하지만 최대한 도움을 주려고 노력합니다!
그들은 우리가 코드에서 일관성을 유지하도록 도와줍니다 - 보통 타입 사용과 관련된 것들로 인해, 우리가 실수를 하지 않도록 도와줍니다.

만약 에러 메시지를 만난다면, 익숙해질 때 까지 자세히 읽어보고, 에러 메시지가 암시하는 문제 코드도 읽어보세요.
경험이 쌓이면, 에러의 가장 중요한 부분은 문제가 발생한 코드의 위치일 것이고, 코드를 읽어보면 에러 메시지 없이도 문제를 찾을 수 있을 것입니다.

타입에 대한 이해를 시험하기 위해 타입 시그니처과 주석을 추가하는 것도 큰 도움이 됩니다.
또는 [typed holes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html)
를 사용하여 특정 위치에서 예상되는 타입을 GHC를 통해 확인할 수도 있습니다.

### 프로젝트가 느린 이유

[//]: # "이것은 다양한 이유로 발생할 수 있습니다. 일반 작업의 시간 복잡도에 대한 비효율적 알고리즘 또는 작업에 부적합한 데이터 구조([unsuited data structures](https://github.com/soupi/haskell-study-plan#data-structures))부터, 덜 효율적인 메모리 표현(이것은 대부분의 경우 `Text`를 `String` 대신 사용하는 다른 알림입니다)까지, 그리고 게으름 문제(또한 평가 전략!)까지입니다."

프로젝트가 느린 이유는 다양할 수 있습니다.
일반 연산의 시간 복잡도에 관해 비효율적인 알고리즘을 사용하거나,
[부적절한 자료구조](https://github.com/soupi/haskell-study-plan#data-structures)를 사용했을 수 있습니다.
또는 덜 효율적인 메모리 표현(`Text` 대신 `String` 사용하기)을 사용했거나, 게으름 문제(이번에도 평가 전략!)도 있을 수 있습니다.

제 Haskell study plan에 있는
[performance 섹션](https://github.com/soupi/haskell-study-plan#performance)
에는 하스켈 평가, 프로파일링 및 사례 연구에 대한 다양한 자료가 있습니다.

## 디자인

### 프로그램 구조를 설계하는 방법

먼저 기능적 코어 명령형 쉘 접근법을 사용하여 시작하세요.
이후 로직을 위한 조합자 패턴으로 EDSL을 정의하고,
필요한 경우 `State`와 같은 기능을 지역적으로 사용하고,
`ReaderT`를 통해 환경 설정을 추가하면서 어떻게 변화하는지 살펴보세요.

만약 이러한 접근법이 실패한다면, 왜 실패했는지 살펴보고, 필요한 경우 다른 접근법을 살펴보세요.

### 데이터를 모델링하는 방법

ADT를 사용하여 데이터를 모델링하는 것이 일반적으로 가장 좋은 방법입니다.
객체 지향 프로그래머들은 종종 타입 클래스를 사용하여 상속과 유사한 메서드를 정의하려고 합니다.
하지만 이는 종종 올바른 접근법이 아니며, 다양한 상황을 위해 다른 생성자를 가진 ADT가 더 좋을 수 있습니다.
OOP 사람들도 종종 상속보다는 합성을 추구하는 걸 명심하세요.

데이터와 행위를 묶기보다는, 데이터에 대한 행위를 정의하기 위해 함수를 사용하세요.
