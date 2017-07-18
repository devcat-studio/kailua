# 기데로스 지원

`gideros` 클래스 시스템은 [기데로스 클래스](http://docs.giderosmobile.com/classes_in_gideros.html)의 동작을 모방합니다. 이 클래스 시스템은, 물론 이미 필요했다는 사실은 차치하고, 너무 단순하게 설계되어 타입 체킹에서 문제가 될 수 있는 흔한 예제라 선택되었습니다.

다음은 `gideros` 클래스 시스템에서 흔히 쓰이게 될 초기 선언입니다.

```lua
--# class system gideros
--# assume global class(gideros) Object
--# assume global Core: {}
--# assume Core.class: [make_class(gideros)] function(parent: table?) --> table
```

이 명령들은 `Object` 최상위 클래스와, 클래스를 생성해 내는 `Core.class` 함수를 선언합니다.

## 생성자

`init` 메소드가 선언되면 자동으로 `new` 메소드도 함께 생성됩니다. 

```lua
--# assume global class(gideros) Foo: Object

--v method(text: string)
function Foo:init(text)
    self.text = text
end

local x = Foo.new('string')
```

참고로, 현재 실제로 `new` 메소드가 생성되는 시점은 `new`가 처음으로 불렸을 때입니다. 따라서 오류가 `init`의 선언보다 늦어져서 사용하는 위치에서 날 수 있습니다.

## 상속

기데로스 클래스 시스템은 단일 상속을 지원하며, [이전 장](classes.html)에서 설명한 일반적인 문법과 동작을 따릅니다.

자식 클래스에서 선언된 필드는 부모 클래스에서 이미 선언된 필드를 단순히 감추게 되는데, 이를 제약하지 않으면 서브타이핑(부모 클래스를 예상하는 곳에 자식 클래스를 쓸 수 있는 기능)이 깨지게 됩니다. 따라서 **카일루아에서 필드들은 일반적으로 오버라이딩할 수 없습니다.** 다만 생성자(`init`)에 한해서 오버라이딩이 가능한데, 대신 생성자는 인스턴스를 통해서는 접근할 수 없습니다.

기데로스에서 모든 클래스는 `Object` 최상위 클래스의 자식으로 가정됩니다. **카일루아는 부모 클래스 없이 선언된 첫번째 (그리고 마지막) 클래스를 인식하며 그러한 클래스가 여럿 생기는 걸 금지합니다.** `Core.class` 함수는 부모 클래스가 없을 경우 `Object`를 대신 쓸 것입니다. 하지만 이런 경우가 아니라면 암묵적인 동작이 혼란스럽기 때문에, `--# assume class`의 경우 부모 클래스가 `Object`더라도 무조건 명시적으로 제시해야 합니다.

