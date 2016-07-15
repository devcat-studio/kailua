use ty::{self, Display, Displayed};
use kailua_syntax::Name;

pub type T<'a> = Displayed<'a, 'a, ty::T<'a>>;
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

define_msg! { pub OtherTypeOrigin:
    "ko" => "다른 타입은 여기에서 만들어졌습니다",
    _    => "The other type originates here",
}

define_msg! { pub CannotRedefineGlobalVar<'a> { name: &'a Name }:
    "ko" => "전역 변수 {name}의 타입을 재지정할 수 없습니다",
    _    => "Cannot redefine the type of a global variable {name}",
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

define_msg! { pub CallToNonFunc<'a> { func: T<'a> }:
    "ko" => "함수가 아닌 타입 `{func}`을(를) 호출하려고 했습니다",
    _    => "Tried to call a non-function `{func}`",
}

define_msg! { pub CallToInexactType<'a> { func: T<'a> }:
    "ko" => "`{func}` 타입은 호출 가능하지만 아직 덜 추론되었습니다",
    _    => "The type `{func}` is callable but not known enough to call",
}

define_msg! { pub CallToOverloadedFunc<'a> { func: T<'a> }:
    "ko" => "오버로딩된 함수 타입 `{func}`은(는) 아직 지원되지 않습니다",
    _    => "Overloaded function `{func}` is not yet supported",
}

define_msg! { pub CallToAnyFunc<'a> { func: T<'a> }:
    "ko" => "`{func}` 타입은 다운캐스팅하지 않으면 호출할 수 없습니다",
    _    => "Cannot call `{func}` without downcasting",
}

define_msg! { pub IndexToNonTable<'a> { tab: Slot<'a> }:
    "ko" => "테이블이 아닌 타입 `{tab}`을(를) 인덱싱하려고 했습니다",
    _    => "Tried to index a non-table type `{tab}`",
}

define_msg! { pub IndexToInexactType<'a> { tab: Slot<'a> }:
    "ko" => "`{tab}` 타입은 테이블이긴 하지만 아직 덜 추론되었습니다",
    _    => "The type `{tab}` is tabular but not known enough to index",
}

define_msg! { pub IndexToRecWithInexactStr<'a> { tab: Slot<'a>, key: T<'a> }:
    "ko" => "실행하기 전에 알 수 없는 `{key}` 타입으로 `{tab}`을(를) 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` with index `{key}` that cannot be resolved ahead of time",
}

define_msg! { pub IndexToArrayWithNonInt<'a> { tab: Slot<'a>, key: T<'a> }:
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

define_msg! { pub CannotAdaptTable<'a> { tab: Slot<'a>, adapted: T<'a> }:
    "ko" => "`{tab}` 테이블 타입이 `{adapted}`(으)로 확장되어야 하는데 그럴 수 없습니다",
    _    => "Cannot adapt the table type `{tab}` into `{adapted}`",
}

define_msg! { pub AdaptTriggeredByIndex<'a> { key: Slot<'a> }:
    "ko" => "테이블을 `{key}` 타입으로 인덱싱하려면 테이블이 확장되어야 합니다",
    _    => "The table had to be adapted in order to index it with `{key}`",
}

define_msg! { pub CannotAssign<'a> { lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "`{lhs}` 타입에 `{rhs}` 타입을 대입할 수 없습니다",
    _    => "Cannot assign `{rhs}` into `{lhs}`",
}

define_msg! { pub NonFuncIterator<'a> { iter: T<'a> }:
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

define_msg! { pub ModCannotReturnInexactType<'a> { returns: T<'a> }:
    "ko" => "모듈이 아직 덜 추론된 타입 `{returns}`을(를) 반환하려고 합니다",
    _    => "The module has returned a type `{returns}` that is not yet fully resolved",
}

define_msg! { pub UnknownLiteralTypeName:
    "ko" => "리터럴이 `type`의 반환값으로 나올 수 있는 타입이 아닙니다",
    _    => "The literal cannot appear as a return type name for `type`",
}

define_msg! { pub UnknownBuiltinName<'a> { name: &'a Name }:
    "ko" => "{name} 내장 함수명을 알 수 없어서 무시합니다",
    _    => "{name} is an unknown built-in name and ignored",
}

