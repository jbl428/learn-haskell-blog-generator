---
slug: /
---

# 이 책에 대하여

> 이 책은 활발하게 유지되고 있습니다. 오류를 발견하셨다면 [여기](https://github.com/soupi/learn-haskell-blog-generator/issues) 로 알려주세요.

<!--
<div style="text-align: center">
  <img src="book-logo-transparent.png" alt="book logo" style="max-width: 40%">
</div>
-->

이 책에서는 하스켈을 사용하여 간단한 정적 블로그 생성기를 구현합니다.
블로그 생성기는 우리의 커스텀 마크업 언어로 작성된 문서를 HTML로 변환합니다.

다음 작업을 수행할 것입니다:

1. HTML을 출력하는 간소한 라이브러리 구현
2. 우리만의 커스텀 마크업 언어 정의 및 파싱
3. 파일을 읽고 내용을 결합
4. 명령 줄 인수 파싱
5. 테스트 및 문서 작성

각 장에서는 우리가 달성하려는 특정 작업에 집중합니다,
그리고 각 장을 통해 해당 작업을 완료하기 위해 충분한 양의 하스켈을 배울것입니다.

## 이 책을 읽는 다른 방법

영상으로 보는 것을 선호하신다면, [Impure Pics](https://impurepics.com)가 이 책을 기반으로 한
[동영상 시리즈](https://www.youtube.com/watch?v=ZL0qExCnO8g&list=PLxn_Aq3QlOQcXoHWdzxnnuGlGWNXJg43R&index=1)
를 제공합니다.

## 왜 이 책을 읽어야 하나요?

[[다른 곳에서 이 책에 대한 언급](https://github.com/soupi/learn-haskell-blog-generator/discussions/67)]

이미 하스켈에 대한 많은 튜토리얼, 가이드 및 책이 있습니다. 왜 이 책을 읽어야 하나요?

### 장점

아마 더 있겠지만, 여기 몇 가지 장점이 있습니다:

- **상대적으로 짧습니다** - 대부분의 하스켈 책은 수백 페이지입니다.
  이 책을 PDF로 내보내면 약 150 페이지 정도입니다.
- **프로젝트 지향적입니다**. 많은 하스켈 책은 하스켈을 배우기 위해 기본 개념과 기능을 진행하는 방법을 가르칩니다.
  하지만 이 책에서는 **하스켈 프로그램을 구축**하고, 그 과정에서 하스켈을 배웁니다.
  이것은 몇몇 사람에게는 장점이 될 수 있지만, 다른 사람에게는 단점이 될 수 있습니다.<br/>
  이 책과 비슷한 다른 튜토리얼들이 있습니다. 가장 유명한 것들은
  [Beginning Haskell](https://www.apress.com/gp/book/9781430262510#otherversion=9781430262503)
  과 [Haskell via Sokoban](https://haskell-via-sokoban.nomeata.de/) 입니다.
- 디자인 패턴, 테스트, 문서화같은 **중요한 주제**에 대해 다룹니다.
- **많은 연습문제**가 있으며, 그 연습문제들의 **해답**도 있습니다.
- **온라인**이기 때문에, 수정이 쉽습니다.
- **무료**입니다.

### 단점

아마 더 있겠지만, 여기 몇 가지 단점이 있습니다:

- **깊이가 부족할 수 있습니다** - 많은 하스켈 튜토리얼은 각 기능에 대해 더 깊이 들어가기 때문에 길어집니다.
  그래서 이 책은 상대적으로 짧게 유지하려고 했습니다.
- 다른 튜토리얼에 비해 **더 적은 기능과 기술을**을 다룹니다.
  우리는 구현에 필요한 기능을 다루려고 하지만, 우리의 작업에 중요하지 않은 기능은 놓칠 수 있습니다.
  그러나 다른 자료는 다양한 사용 사례를 다루기에 더 많은 기능을 다룹니다.
- 꽤 많은 코드작성이 필요하지만, **에디터를 제공하지 않습니다**.

### 다른 학습 자료

[하스켈 문서 페이지](https://www.haskell.org/documentation/) 에서는 다양한 튜토리얼, 책, 가이드, 강의를 찾을 수 있습니다.
아니면 [이곳](https://github.com/soupi/haskell-study-plan#about-this-guide)에서 다른 대안을 찾을 수 있습니다.

### 저자

저는
[<img src="https://avatars.githubusercontent.com/u/8547573" alt="🐨" style={{ 'border-radius': '100px', 'max-height': '1.5em', 'vertical-align': 'top'}} /> gilmi](https://gilmi.me)
입니다.

## 토론

이 책에 대해 논의하고 싶거나 질문이 있다면 [discussion board](https://github.com/soupi/learn-haskell-blog-generator/discussions)에 방문하세요!
