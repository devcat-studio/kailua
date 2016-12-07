use ty::{self, Key, Display, Displayed};
use kailua_syntax::Name;

pub type Ty<'a> = Displayed<'a, 'a, ty::Ty>;
pub type Slot<'a> = Displayed<'a, 'a, ty::Slot>;

define_msg! { pub NoVar<'a> { name: &'a Name }:
    "ko" => "전역 또는 지역 변수 {name}가(이) 선언되지 않았습니다",
    _    => "Global or local variable {name} is not defined",
}

define_msg! { pub NoVarargs:
    "ko" => "맨 안쪽 함수에 가변 인자가 존재하지 않습니다",
    _    => "Variadic arguments do not exist in the innermost function",
}

define_msg! { pub NoType<'a> { name: &'a Name }:
    "ko" => "{name} 타입이 선언되지 않았습니다",
    _    => "Type {name} is not defined",
}

define_msg! { pub NotSubtype<'a, Sub: 'a + Display,
                                 Sup: 'a + Display> { sub: Displayed<'a, 'a, Sub>,
                                                      sup: Displayed<'a, 'a, Sup> }:
    "ko" => "`{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "`{sub}` is not a subtype of `{sup}`",
}

define_msg! { pub NotEqual<'a, Lhs: 'a + Display,
                               Rhs: 'a + Display> { lhs: Displayed<'a, 'a, Lhs>,
                                                    rhs: Displayed<'a, 'a, Rhs> }:
    "ko" => "`{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "`{lhs}` does not equal to `{rhs}`",
}

define_msg! { pub InvalidUnionType<'a, Lhs: 'a + Display,
                                       Rhs: 'a + Display> { lhs: Displayed<'a, 'a, Lhs>,
                                                            rhs: Displayed<'a, 'a, Rhs> }:
    "ko" => "`{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}`",
}

define_msg! { pub OtherTypeOrigin:
    "ko" => "다른 타입은 여기에서 만들어졌습니다",
    _    => "The other type originates here",
}

define_msg! { pub CannotRedefineVar<'a> { name: &'a Name }:
    "ko" => "{name} 변수의 타입을 재지정할 수 없습니다",
    _    => "Cannot redefine the type of a variable {name}",
}

define_msg! { pub TypeSpecToIndex:
    "ko" => "인덱싱 수식에 타입을 지정할 수 없습니다",
    _    => "Cannot specify the type of indexing expression",
}

define_msg! { pub UseOfUnassignedVar:
    "ko" => "초기화되지 않은 변수를 사용하려고 했습니다",
    _    => "The variable is not yet initialized",
}

define_msg! { pub UnassignedVarOrigin<'a> { var: Slot<'a> }:
    "ko" => "변수가 `{var}` 타입이기 때문에 `nil`로 자동으로 초기화되지 않습니다",
    _    => "The variable was not implicitly initialized to `nil` as its type is `{var}`",
}

define_msg! { pub CannotRedefineType<'a> { name: &'a Name }:
    "ko" => "{name} 타입은 이미 선언되어 있습니다",
    _    => "A type named {name} is already defined",
}

define_msg! { pub AlreadyDefinedType:
    "ko" => "이전 타입 선언은 여기에 있습니다",
    _    => "The type was originally defined here",
}

define_msg! { pub WrongOperand<'a> { op: &'static str, lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "{op} 연산자를 `{lhs}`와(과) `{rhs}`에 적용할 수 없습니다",
    _    => "Cannot apply {op} operator to `{lhs}` and `{rhs}`",
}

define_msg! { pub OperandIsBothNumOrStr<'a> { op: &'static str, operand: Slot<'a> }:
    "ko" => "{op}의 피연산자 `{operand}`가(이) 문자열인지 숫자인지가 불분명합니다",
    _    => "Operand `{operand}` to {op} operator should be \
             either numbers or strings but not both",
}

define_msg! { pub OperandsAreNotBothNumOrStr<'a> { op: &'static str, lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "{op}의 피연산자 `{lhs}`와(과) `{rhs}`가(이) \
             둘 다 문자열이거나 둘 다 숫자여야 하는데 아닙니다",
    _    => "Operands `{lhs}` and `{rhs}` to {op} operator \
             should be both numbers or both strings",
}

define_msg! { pub CannotDeduceBothNumOrStr<'a> { op: &'static str, lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "{op}의 피연산자 `{lhs}`와(과) `{rhs}`가(이) \
             둘 다 문자열이거나 숫자인지 알 수 없습니다",
    _    => "Cannot deduce if operands `{lhs}` and `{rhs}` \
             to {op} operator are either numbers or strings",
}

define_msg! { pub CallToNonFunc<'a> { func: Ty<'a> }:
    "ko" => "함수가 아닌 타입 `{func}`을(를) 호출하려고 했습니다",
    _    => "Tried to call a non-function `{func}`",
}

define_msg! { pub CallToInexactType<'a> { func: Ty<'a> }:
    "ko" => "`{func}` 타입은 호출 가능하지만 아직 덜 추론되었습니다",
    _    => "The type `{func}` is callable but not known enough to call",
}

define_msg! { pub CallToAnyFunc<'a> { func: Ty<'a> }:
    "ko" => "`{func}` 타입은 다운캐스팅하지 않으면 호출할 수 없습니다",
    _    => "Cannot call `{func}` without downcasting",
}

define_msg! { pub TableLitWithUnknownKey<'a> { key: Ty<'a> }:
    "ko" => "테이블 생성자는 항상 레코드여야 하므로 덜 추론되거나, 문자열이나 숫자가 아니거나, \
             미리 알 수 없는 `{key}` 타입을 키로 쓸 수 없습니다",
    _    => "The key type `{key}` is not known enough, not a string or integer, \
             or not known ahead of time as the table constructor should always be a record",
}

define_msg! { pub TableLitWithUnboundSeq:
    "ko" => "테이블 생성자는 항상 레코드여야 하므로 \
             반환값 갯수가 정해지지 않은 수식을 마지막 수식으로 쓸 수 없습니다",
    _    => "This expression has an unknown number of return values, \
             so cannot be used as the last value in the table constructor \
             which should always be a record",
}

define_msg! { pub TableLitWithDuplicateKey<'a> { key: &'a Key }:
    "ko" => "테이블 생성자에서 `{key}` 키가 중복되었습니다",
    _    => "The key `{key}` is duplicated in the table constructor",
}

define_msg! { pub PreviousKeyInTableLit:
    "ko" => "같은 키가 여기에서 이미 할당되었습니다",
    _    => "The key was previously assigned here",
}

define_msg! { pub IndexToNonTable<'a> { tab: Slot<'a> }:
    "ko" => "테이블이 아닌 타입 `{tab}`을(를) 인덱싱하려고 했습니다",
    _    => "Tried to index a non-table type `{tab}`",
}

define_msg! { pub IndexToInexactType<'a> { tab: Slot<'a> }:
    "ko" => "`{tab}` 타입은 테이블이긴 하지만 아직 덜 추론되었습니다",
    _    => "The type `{tab}` is tabular but not known enough to index",
}

define_msg! { pub IndexToUnknownClass<'a> { cls: Slot<'a> }:
    "ko" => "`{cls}` 타입이 정확히 하나의 클래스로 추론되지 않아 인덱싱할 수 없습니다",
    _    => "Cannot index `{cls}` that cannot be inferred to a single class",
}

define_msg! { pub IndexToRecWithUnknownStr<'a> { tab: Slot<'a>, key: Ty<'a> }:
    "ko" => "실행하기 전에 알 수 없는 `{key}` 타입으로 `{tab}`을(를) 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` with index `{key}` that cannot be resolved ahead of time",
}

define_msg! { pub IndexToClassWithUnknown<'a> { cls: Slot<'a>, key: Ty<'a> }:
    "ko" => "실행하기 전에 알 수 없는 `{key}` 타입으로 `{cls}`을(를) 인덱싱할 수 없습니다",
    _    => "Cannot index `{cls}` with index `{key}` that cannot be resolved ahead of time",
}

define_msg! { pub IndexToArrayWithNonInt<'a> { tab: Slot<'a>, key: Ty<'a> }:
    "ko" => "정수가 아닌 `{key}` 타입으로 `{tab}`을(를) 인덱싱할 수 없습니다",
    _    => "Cannot index an array `{tab}` with a non-integral index `{key}`",
}

define_msg! { pub IndexToAnyTable<'a> { tab: Slot<'a> }:
    "ko" => "`{tab}` 타입은 다운캐스팅하지 않으면 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` without downcasting",
}

define_msg! { pub CannotIndex<'a> { tab: Slot<'a>, key: Slot<'a> }:
    "ko" => "`{key}` 타입으로 `{tab}`을(를) 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` with `{key}`",
}

define_msg! { pub CannotAdaptTable<'a> { tab: Slot<'a>, adapted: Ty<'a> }:
    "ko" => "`{tab}` 테이블 타입이 `{adapted}`(으)로 확장되어야 하는데 그럴 수 없습니다",
    _    => "Cannot adapt the table type `{tab}` into `{adapted}`",
}

define_msg! { pub CannotAdaptClass<'a> { cls: Slot<'a> }:
    "ko" => "`{cls}` 타입을 제자리에서 확장할 수 없습니다",
    _    => "Cannot adapt the type `{cls}` in place",
}

define_msg! { pub AdaptTriggeredByIndex<'a> { key: Slot<'a> }:
    "ko" => "테이블을 `{key}` 타입으로 인덱싱하려면 테이블이 확장되어야 합니다",
    _    => "The table had to be adapted in order to index it with `{key}`",
}

define_msg! { pub CannotAssign<'a> { lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "`{lhs}` 타입에 `{rhs}` 타입을 대입할 수 없습니다",
    _    => "Cannot assign `{rhs}` into `{lhs}`",
}

define_msg! { pub NonFuncIterator<'a> { iter: Ty<'a> }:
    "ko" => "`for`-`in` 문에 주어진 반복자가 함수가 아닌 `{iter}` 타입을 반환했습니다",
    _    => "The iterator given to `for`-`in` statement returned a non-function `{iter}`",
}

define_msg! { pub BuiltinGivenLessArgs<'a> { name: &'a str, nargs: usize }:
    "ko" => "`{name}` 내장 함수는 인자가 적어도 {nargs}개 필요합니다",
    _    => "`{name}` needs at least {nargs} argument(s)",
}

define_msg! { pub CannotResolveModName:
    "ko" => "`require`에 주어진 모듈 이름을 찾을 수 없습니다",
    _    => "Cannot resolve the module name given to `require`",
}

define_msg! { pub RecursiveRequire:
    "ko" => "모듈을 재귀적으로 `require`하려고 했습니다",
    _    => "Recursive `require` was requested",
}

define_msg! { pub PreviousRequire:
    "ko" => "이전에 이미 여기에서 이 모듈을 `require` 했습니다",
    _    => "The module was previously `require`d here",
}

define_msg! { pub ModCannotReturnFalse:
    "ko" => "모듈에서 `false`를 반환하면 루아가 `require`를 재귀적으로 \
             요청하는 것을 막을 수 없으므로 사용하면 안 됩니다",
    _    => "Returning `false` from the module disables Lua's protection \
             against recursive `require` calls and is heavily discouraged",
}

define_msg! { pub ModCannotReturnInexactType<'a> { returns: Ty<'a> }:
    "ko" => "모듈이 아직 덜 추론된 타입 `{returns}`을(를) 반환하려고 합니다",
    _    => "The module has returned a type `{returns}` that is not yet fully resolved",
}

define_msg! { pub UnknownLiteralTypeName:
    "ko" => "리터럴이 `type`의 반환값으로 나올 수 있는 타입이 아닙니다",
    _    => "The literal cannot appear as a return type name for `type`",
}

define_msg! { pub UnknownAttrName<'a> { name: &'a Name }:
    "ko" => "{name} 타입 속성을 알 수 없어서 무시합니다",
    _    => "{name} is an unknown type attribute and ignored",
}

define_msg! { pub UnknownAssignToPackagePath<'a> { name: &'a str }:
    "ko" => "`{name}` 내장 변수에 저장되는 값을 알 수 없어서 \
             `require` 경로를 찾는데 문제가 있을 수 있습니다",
    _    => "Cannot infer the values assigned to the `{name}` built-in variable; \
             subsequent `require` may be unable to find the module path",
}

define_msg! { pub IndexedTypeIsBothTableOrStr<'a> { indexed: Slot<'a> }:
    "ko" => "인덱싱이 되는 `{indexed}` 타입이 테이블인지 문자열인지가 불분명합니다",
    _    => "`{indexed}` type that is being indexed should be \
             either a table or a string but not both",
}

define_msg! { pub UndefinedStringMeta:
    "ko" => "`string` 타입의 메타테이블이 아직 설정되지 않아서 문자열 메소드들을 쓸 수 없습니다",
    _    => "Cannot use string methods as a metatable for `string` type is not yet defined",
}

define_msg! { pub CannotRedefineStringMeta:
    "ko" => "`string` 타입의 메타테이블은 한 번 이상 설정될 수 없으며 \
             기본적으로 `--# open` 명령을 통해서만 설정되어야 합니다",
    _    => "A metatable for `string` type cannot be defined more than once \
             and in general should only be set via `--# open` directive",
}

define_msg! { pub NonTableStringMeta:
    "ko" => "`string` 타입의 메타테이블이 설정되긴 했지만 테이블이 아닙니다",
    _    => "A metatable for `string` type has been defined but is not a table",
}

define_msg! { pub PreviousStringMeta:
    "ko" => "`string` 타입의 메타테이블이 이전에 여기서 설정되었습니다",
    _    => "A metatable for `string` type has been previously defined here",
}

define_msg! { pub IgnoredIfCase:
    "ko" => "`if` 문의 이 조건(들)은 실행되지 않습니다",
    _    => "These `if` case(s) are never executed",
}

define_msg! { pub IfCaseWithTruthyCond:
    "ko" => "이 조건이 항상 참인 값으로 평가됩니다",
    _    => "This condition always evaluates to a truthy value",
}

define_msg! { pub IfCaseWithFalsyCond:
    "ko" => "이 조건이 항상 거짓인 값으로 평가됩니다",
    _    => "This condition always evaluates to a falsy value",
}

define_msg! { pub RedefinedClassName:
    "ko" => "클래스 이름이 이미 설정되어 있어서 이 이름은 무시됩니다",
    _    => "A new name for the previously named class is ignored",
}

define_msg! { pub PreviousClassName:
    "ko" => "클래스 이름이 여기서 설정되었습니다",
    _    => "The class was previously named here",
}

define_msg! { pub CannotNameUnknownClass<'a> { cls: Slot<'a> }:
    "ko" => "`{cls}` 타입이 하나의 클래스로 정해지지 않았기 때문에 이름을 설정할 수 없습니다",
    _    => "The type `{cls}` cannot be resolved to a single class so cannot be named",
}

define_msg! { pub CannotDefineMethodsWithoutCtor:
    "ko" => "생성자(`init` 메소드)가 없는 상태에서 메소드를 선언할 수 없습니다",
    _    => "Cannot define methods without a constructor (`init` method) defined",
}

define_msg! { pub CannotRedefineCtor:
    "ko" => "이미 선언된 생성자(`init` 메소드)를 바꿀 수 없습니다",
    _    => "Cannot replace an already defined constructor (`init` method)",
}

define_msg! { pub SelfCannotBeAssignedInCtor:
    "ko" => "생성자(`init` 메소드)에서는 `self` 변수에 대입을 할 수 없습니다",
    _    => "`self` variable cannot be assigned in a constructor (`init` method)",
}

define_msg! { pub InexactInitMethod<'a> { init: Slot<'a> }:
    "ko" => "생성자(`init` 메소드)의 타입 `{init}`이(가) 덜 추론되었습니다",
    _    => "The type `{init}` of the constructor (`init` method) is not known enough to call",
}

define_msg! { pub NonFuncInitMethod<'a> { init: Slot<'a> }:
    "ko" => "생성자(`init` 메소드)의 타입 `{init}`이(가) 함수가 아닙니다",
    _    => "The type `{init}` of the constructor (`init` method) is not a function",
}

define_msg! { pub OverloadedFuncInitMethod<'a> { init: Slot<'a> }:
    "ko" => "생성자(`init` 메소드)의 타입 `{init}`이(가) 오버로딩되어 있습니다",
    _    => "The type `{init}` of the constructor (`init` method) is overloaded",
}

define_msg! { pub BadSelfInInitMethod<'a> { init: Slot<'a> }:
    "ko" => "생성자(`init` 메소드)의 타입 `{init}`이(가) \
             첫번째 인자로 올바른 타입을 가지지 않습니다",
    _    => "The type `{init}` of the constructor (`init` method) \
             doesn't have a correct type for the first argument",
}

define_msg! { pub CannotCallCtor:
    "ko" => "생성자(`init` 메소드)는 내부적으로만 호출되며 바깥에서 호출되어서는 안됩니다",
    _    => "The constructor (`init` method) is only internally called and \
             should not be called outside",
}

define_msg! { pub ReservedNewMethod:
    "ko" => "`new` 메소드는 예약되어 있으며 선언될 수 없습니다",
    _    => "`new` method is reserved and cannot be defined",
}

define_msg! { pub CannotAddFieldsToInstance:
    "ko" => "생성자 바깥에서는 클래스 인스턴스에 새 필드가 추가될 수 없습니다",
    _    => "Cannot add a new field to the class instance outside of the constructor",
}

define_msg! { pub BadSelfTypeInMethod:
    "ko" => "메소드의 `self` 타입이 주어질 경우 그 타입은 \
             항상 클래스 인스턴스 타입이어야 합니다",
    _    => "The type of `self` argument to the method, if present, \
             should be a corresponding class instance type",
}

define_msg! { pub NoCheckRequiresTypedSelf:
    "ko" => "[no_check] 속성이 주어졌을 경우 `self` 인자에 타입이 주어져야 합니다",
    _    => "[no_check] attribute requires the `self` argument to be typed",
}

define_msg! { pub NoCheckRequiresTypedArgs:
    "ko" => "[no_check] 속성이 주어졌을 경우 인자에 타입이 주어져야 합니다",
    _    => "[no_check] attribute requires the arguments to be typed",
}

define_msg! { pub NoCheckRequiresTypedVarargs:
    "ko" => "[no_check] 속성이 주어졌을 경우 가변 인자에 타입이 주어져야 합니다",
    _    => "[no_check] attribute requires the variadic arguments to be typed",
}

define_msg! { pub NoCheckRequiresTypedReturns:
    "ko" => "[no_check] 속성이 주어졌을 경우 함수의 반환 타입이 주어져야 합니다",
    _    => "[no_check] attribute requires the return type to be present",
}

define_msg! { pub UnsupportedErrorType:
    "ko" => "`error \"메시지\"` 타입은 아직 지원되지 않습니다",
    _    => "`error \"message\"` type is not yet supported",
}

