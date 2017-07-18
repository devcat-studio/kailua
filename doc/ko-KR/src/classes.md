# 클래스 시스템

루아는 일반적으로 클래스를 포함한 객체지향 기능을 직접 지원하지는 않습니다. 대신 루아는 클래스 시스템을 설계하는 어려운 일을 사용자(또는 라이브러리 저자)에게 맡깁니다. 덕분에 다양한 **클래스 시스템**이 널리 쓰이고 있으며, 카일루아가 뭘 지원하든 이 모든 클래스 시스템의 기능을 제공해야만 합니다.

이는 불가능에 가깝기 때문에, 카일루아는 사용자 지정 가능한 클래스 시스템을 지원합니다. 현재 이 기능은 초기 개발 단계이며 모든 가능한 기능이 구현되어 있지 않은데, 보통 어떤 기능이 사용되는지 분석을 하지 못 하기 때문에 그렇습니다. 다른 클래스 시스템의 분석 및 제안은 언제나 환영합니다.

## 클래스 시스템의 선언

현재 세션에서 사용 가능한 클래스 시스템은 고유한 전역 이름으로 구분되며, 명시적으로 선언되어야 합니다.

```lua
--# class system gideros
```

현재 각 클래스 시스템의 이름은 고정이며 다음 목록에서 골라야 합니다.

| 이름 | 설명 | 상속 지원 | `[make_class]` |
| ---- | ---- | --------- | -------------- |
| `gideros` | [기데로스 클래스 시스템](classes-gideros.html) | 단일 | 지원 |

## 클래스의 선언

클래스는 두 가지 방법으로 선언할 수 있습니다.

1. `--# assume class` 명령은 이미 존재하는 클래스를 선언 및 가정(assume)할 수 있습니다.
2. 클래스 시스템이 지원할 경우, `[make_class(<class system>)]` 속성을 가진 함수가 호출될 때마다 새 클래스가 생성됩니다.

### `--# assume class` 명령

`--# assume class` 명령은 클래스 시스템에 소속되지 않은 클래스를 만들 수 있습니다. 이는 상속을 쓰지 않고 특별한 의미론이 붙지 않은 간단한 클래스를 선언하는 데 유용합니다.

```lua
-- 전역 변수 `Hello`를 새로 선언된 전역 클래스 `Hello`의
-- 프로토타입으로 선언함

--# assume global class Hello
```

이는 단순한 명령이기 때문에(즉 루아는 무시할 것이기 때문에), `global`이 없이 선언된 지역 클래스는 해당 이름이 이미 지역 변수여야만 합니다.

```lua
local Hello = {}
--# assume class Hello
```

클래스 시스템이 있다면 괄호로 묶여야 합니다. 클래스 시스템이 상속을 지원할 경우 부모 클래스도 지정할 수 있습니다.

```lua
--# assume global class(gideros) Object
--# assume global class(gideros) Sprite: Object
```

일반적으로 서로 다른 클래스 시스템, 그리고 클래스 시스템에 소속된 클래스와 그렇지 않은 클래스는 서로 상호작용할 수 없습니다.

### `[make_class]` 속성

`[make_class]` 속성이 붙은 함수는 일반적인 루아 코드가 새 클래스를 만드는 데 쓰는 방법과 비슷합니다. 이 속성은 클래스 시스템에 소속된 클래스에만 쓸 수 있으며, 다음과 같이 `--# assume` 되거나...

```lua
--# assume global `class`: [make_class(gideros)] function() --> table
```

또는 함수 명세와 함께 명시적으로 선언될 수 있습니다:

```lua
--v [NO_CHECK] -- 클래스의 내부 구현은 체크하기 어려우므로
--v [make_class(gideros)]
--v function() --> table
function make_class()
    -- ...
end
```

많은 경우 이 함수를 인자 없이 호출하면 명시적인 부모 클래스가 없는 새 클래스를 선언하게 되고, 부모 클래스 프로토타입을 인자로 넘겨 주면 상속으로 처리됩니다. 정확한 동작과 인자들의 해석은 물론 클래스 시스템마다 다를 수 있습니다.

`[make_class]` 함수가 반환한 새 클래스 프로토타입은 최대한 빨리 변수에 대입되어야 합니다. 이 때가 바로 클래스에 이름이 붙는 때입니다:

```lua
local Hello = class() -- 지역 타입 `Hello`를 함께 정의

Sprite = class() -- 전역 타입 `Sprite`를 함께 정의
```

몇 가지 꼼수로 이름이 없는 클래스를 사용할 수는 있지만 권장하지는 않습니다. 이러한 클래스 또한 오류 메시지 등에서는 유일하게 지칭됩니다.

### 필드와 메소드의 선언

일단 클래스가 선언되면, 클래스 시스템의 자체적인 제한에 걸리지 않는 한 필드와 메소드를 자유롭게 추가할 수 있습니다:

```lua
--# assume global class Person

--v function(name: string) --> Person
function Person.new(name)
    local person = {}
    set_metaclass_for_person(person) -- 이 부분은 여러분에게 맡깁니다
    --# assume person: Person

    person.name = name -- 새 필드를 정의
    return person
end

-- 새 클래스 필드를 선언
Person.HELLO = 'Hello'

-- 새 메소드를 선언 (메소드는 사실 `self`를 받는 함수가 들어간 클래스 필드이므로)
--v method()
function Person:greet()
    print(self.HELLO .. ', ' .. self.name)
end

local person = Person.new('J. Random Hacker')
person:greet()
```

여기서 볼 수 있듯,

* 클래스 시스템이 없을 경우 새 인스턴스 타입을 만들려면 무조건 `--# assume`이 필요합니다. 한편 클래스 시스템은 보통 생성자로부터 `new` 메소드 같은 걸 자동으로 만드는 기능이 있을 겁니다.

* 인스턴스 필드는 대입문으로 만들 수 있습니다. 하지만 없는 필드를 읽는 건 여전히 오류이므로, 만약 위에서 `name` 필드를 설정한 *뒤에* `person` 변수의 타입을 바꿔 치웠다면 타입 체커는 `name` 필드의 존재를 알 수 없을 것입니다.

* 메소드는 함수와 같은 문법으로 선언할 수 있지만 `function` 대신 `method` 예약어를 쓰고 `self` 인자는 생략(되며 자동으로 추론)됩니다.

`--# assume`은 클래스에도 쓸 수 있습니다. 만약 이미 존재하는 클래스에 타입만 붙이는 거라면 위의 코드는 다음과 같이 다시 쓸 수 있습니다:

```lua
--# assume global class Person
--# assume static Person.new: function(name: string) --> Person
--# assume static Person.HELLO: string
--# assume Person.greet: method()

local person = Person.new('J. Random Hacker')
person:greet()
```

이는 보통의 `--# assume`과 같으나 몇 가지 차이점에 주의하세요:

* `Person.greet`는 기술적으로는 `Person`을 첫 인자로 가지는 `static` 함수여야 할 겁니다. `method` 예약어는 편의를 위해서 제공됩니다. (`method`로 시작하는 타입은 없습니다.)

* 전역에 선언된 클래스의 필드는 최상위 블록에서만 `--# assume` 할 수 있습니다. 이는 `--# assume`은 일반적으로 현재의 지역 스코프에 해당 이름의 복사본을 생성하는데, 새 필드를 만드는 건 클래스에 전역적으로 영향을 주기 때문입니다. 

* 클래스 시스템에 따라서는 어떤 필드는 아예 정의를 할 수 없을 수도 있습니다.

<!-- TODO: mention that the classes prototypes are automatically subject to delayed type checking (needs to explain this first) -->

