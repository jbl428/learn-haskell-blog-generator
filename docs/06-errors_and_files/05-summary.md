# 요약

이번 장은 꽤 길었습니다. 배운 것들을 요약해보겠습니다.

우리는 하스켈에서 에러를 다루는 몇 가지 방법을 다루었습니다.

1. 데이터 타입으로 에러를 표현하고 `Either` 타입을 사용해 "값 또는 에러"를 표현.
   부수 효과가 없는 코드에 유용한 접근법입니다.
2. `ExceptT`를 사용해 monadic 기능을 가진 타입 위에 (1)의 접근법을 조합.
3. IO 코드에 대해 예외를 사용.

우리는 몇 가지 새로운 추상화와 기법도 배웠습니다.

1. `Traversable` 타입 클래스는 왼쪽에서 오른쪽으로 순회할 수 있는 자료 구조를 위한 것입니다.
   링크드 리스트, 이진 트리, `Map`과 같은 것들이 있습니다.
   `Either`나 `IO`와 같은 다른 applicative functor 타입과 조합하면 유용합니다.
2. `Monad` 타입 클래스는 `Applicative` 타입 클래스를 `join :: m (m a) -> m a` 함수로 확장합니다.
   `Either`가 이 타입 클래스 인터페이스를 구현한다는 것을 배웠고 `IO`도 마찬가지입니다.
3. `MonadTrans` 타입 클래스는 *monad transformer*로 다른 모나드를 입력으로 받고 monadic 인터페이스(`>>=`, do 표기법 등)를 제공하면서 두 기능을 결합합니다.
   `IO` 위에 `Either`와 비슷한 monad transformer인 `ExceptT`를 쌓는 방법을 배웠습니다.

거의 다 왔습니다. 이 프로젝트를 마무리 하기위해 몇 가지 남은 것들만 처리하면 됩니다.

> Git 커밋을 통해
> [이번에 수정한 내역](https://github.com/soupi/learn-haskell-blog-generator/commit/a08d148d981fa00cb7025f1b651d7b75084dd1ae)
> 과 [현재까지 코드](https://github.com/soupi/learn-haskell-blog-generator/tree/a08d148d981fa00cb7025f1b651d7b75084dd1ae) 를 확인할 수 있습니다.
