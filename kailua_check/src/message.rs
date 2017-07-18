use kailua_types::ty::{self, Key, Displayed, TypeContext};
use kailua_syntax::{Str, Name};

pub type T<'a> = Displayed<'a, ty::T<'a>, &'a TypeContext>;
pub type Ty<'a> = Displayed<'a, ty::Ty, &'a TypeContext>;
pub type SpannedTySeq<'a> = Displayed<'a, ty::SpannedTySeq, &'a TypeContext>;
pub type Slot<'a> = Displayed<'a, ty::Slot, &'a TypeContext>;

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

// can be used for exported types, so avoid using a "locally defined" qualification
define_msg! { pub CannotRedefineLocalType<'a> { name: &'a Name }:
    "ko" => "{name} 타입은 이미 선언되어 있습니다",
    _    => "A type {name} is already defined",
}

define_msg! { pub CannotRedefineGlobalType<'a> { name: &'a Name }:
    "ko" => "{name} 타입은 이미 전역에 선언되어 있습니다",
    _    => "A type {name} is already defined globally",
}

define_msg! { pub CannotImportAlreadyDefinedType<'a> { name: &'a Name }:
    "ko" => "{name} 타입을 들여 오려 했으나 이미 선언되어 있습니다",
    _    => "A type {name} to be imported is already defined",
}

define_msg! { pub CannotReexportType<'a> { name: &'a Name }:
    "ko" => "모듈에서 {name} 타입을 다시 내보낼 수 없습니다",
    _    => "A type {name} cannot be exported again",
}

define_msg! { pub CannotRedefineLocalTypeAsGlobal<'a> { name: &'a Name }:
    "ko" => "지역적으로 선언된 {name} 타입은 전역에서 자기 자신으로만 다시 선언할 수 있습니다",
    _    => "A locally defined type {name} can only be redefined as itself in the global scope",
}

define_msg! { pub CannotRedefineAndReexportType<'a> { name: &'a Name }:
    "ko" => "모듈에서 내보내지 않은 {name} 타입은 자기 자신으로만 선언해서 내보낼 수 있습니다",
    _    => "A non-exported type {name} can only be redefined and exported as itself",
}

define_msg! { pub CannotRedefineTypeAsClass<'a> { name: &'a Name }:
    "ko" => "{name} 타입은 이미 선언되어 있습니다",
    _    => "A type {name} is already defined",
}

define_msg! { pub AlreadyDefinedType:
    "ko" => "이전 타입 선언은 여기에 있습니다",
    _    => "The type was originally defined here",
}

define_msg! { pub WrongUnaryOperand<'a> { op: &'static str, ty: Slot<'a> }:
    "ko" => "{op} 연산자를 `{ty}`에 적용할 수 없습니다",
    _    => "Cannot apply {op} operator to `{ty}`",
}

define_msg! { pub WrongBinaryOperands<'a> { op: &'static str, lhs: Slot<'a>, rhs: Slot<'a> }:
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

define_msg! { pub CallToWrongType<'a> { func: Ty<'a> }:
    "ko" => "`{func}` 타입을 호출할 수 없습니다",
    _    => "The type `{func}` cannot be called",
}

define_msg! { pub CallToAnyFunc<'a> { func: Ty<'a> }:
    "ko" => "타입이 `{func}`(이)라고만 알려져 있어서 호출할 수 없습니다. \
             타입을 더 구체적으로 명시하거나, 여의치 않으면 `--# assume`을 사용하십시오",
    _    => "Cannot call `{func}` without further type information; \
             specify more detailed type, or use `--# assume` as a last resort",
}

define_msg! { pub TableLitWithInvalidRecKey<'a> { key: Ty<'a> }:
    "ko" => "레코드 타입을 가지는 테이블 생성자에서 `{key}` 타입을 키로 쓸 수 없습니다",
    _    => "The type `{key}` cannot be used as a key in the table constructor for records",
}

define_msg! { pub TableLitWithInvalidArrayKey<'a> { key: Ty<'a> }:
    "ko" => "배열 타입을 가지는 테이블 생성자에서 `{key}` 타입을 키로 쓸 수 없습니다",
    _    => "The type `{key}` cannot be used as a key in the table constructor for arrays",
}

define_msg! { pub TableLitWithInvalidArrayValue<'a> { given: Slot<'a>, value: Slot<'a> }:
    "ko" => "`vector<{value}>` 타입을 가지는 테이블 생성자에서 \
             `{given}` 타입을 값으로 쓸 수 없습니다",
    _    => "The type `{given}` cannot be used as a value \
             in the table constructor for the type `vector<{value}>`",
}

define_msg! { pub TableLitWithInvalidMapKey<'a> { given: Ty<'a>, key: Ty<'a>, value: Slot<'a> }:
    "ko" => "`map<{key}, {value}>` 타입을 가지는 테이블 생성자에서 \
             `{given}` 타입을 키로 쓸 수 없습니다",
    _    => "The type `{given}` cannot be used as a key \
             in the table constructor for the type `map<{key}, {value}>`",
}

define_msg! { pub TableLitWithInvalidMapValue<'a> { given: Slot<'a>, key: Ty<'a>, value: Slot<'a> }:
    "ko" => "`map<{key}, {value}>` 타입을 가지는 테이블 생성자에서 \
             `{given}` 타입을 값으로 쓸 수 없습니다",
    _    => "The type `{given}` cannot be used as a value \
             in the table constructor for the type `map<{key}, {value}>`",
}

define_msg! { pub TableLitWithMissingArrayKey:
    "ko" => "배열 타입을 가지는 테이블 생성자에서 빠진 키가 있습니다",
    _    => "Keys in the table constructor for arrays have a missing key",
}

define_msg! { pub TableLitWithNonOneMinArrayKey:
    "ko" => "배열 타입을 가지는 테이블 생성자에서 가장 작은 키가 1이 아닙니다",
    _    => "The minimum key in the table constructor for arrays is not 1",
}

define_msg! { pub TableLitWithUnboundSeq:
    "ko" => "레코드 타입을 가지는 테이블 생성자에서 \
             반환값 갯수가 정해지지 않은 수식을 마지막 수식으로 쓸 수 없습니다",
    _    => "This expression has an unknown number of return values, \
             so cannot be used as the last value in the table constructor for records",
}

define_msg! { pub TableLitWithDuplicateKey<'a> { key: &'a Key }:
    "ko" => "테이블 생성자에서 `{key}` 키가 중복되었습니다",
    _    => "The key `{key}` is duplicated in the table constructor",
}

define_msg! { pub PreviousKeyInTableLit:
    "ko" => "같은 키가 여기에서 이미 할당되었습니다",
    _    => "The key was previously assigned here",
}

define_msg! { pub TableLitIsImplicitlyRec:
    "ko" => "이 테이블의 타입을 알 수 없어서 레코드로 간주했습니다. 타입을 명시해 주십시오",
    _    => "The type of this table was unknown so is assumed to be a record; \
             please specify its type"
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
    "ko" => "`{tab}`에 `{key}`을(를) 키로 써서 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` with `{key}`",
}

define_msg! { pub IndexToClassWithUnknown<'a> { cls: Slot<'a>, key: Ty<'a> }:
    "ko" => "`{cls}`에 `{key}`을(를) 키로 써서 인덱싱할 수 없습니다",
    _    => "Cannot index `{cls}` with `{key}`",
}

define_msg! { pub IndexToArrayWithNonInt<'a> { tab: Slot<'a>, key: Ty<'a> }:
    "ko" => "`{tab}`에 정수가 아닌 `{key}`을(를) 키로 써서 인덱싱할 수 없습니다",
    _    => "Cannot index an array `{tab}` with a non-integral key `{key}`",
}

define_msg! { pub IndexToAnyTable<'a> { tab: Slot<'a> }:
    "ko" => "타입이 `{tab}`(이)라고만 알려져 있어서 인덱싱할 수 없습니다. \
             타입을 더 구체적으로 명시하거나, 여의치 않으면 `--# assume`을 사용하십시오",
    _    => "Cannot index `{tab}` without further type information; \
             specify more detailed type, or use `--# assume` as a last resort",
}

define_msg! { pub CannotUpdate<'a> { tab: Slot<'a> }:
    "ko" => "변경할 수 없는 `{tab}` 타입을 인덱싱해서 갱신할 수 없습니다",
    _    => "Cannot update the immutable type `{tab}` by indexing",
}

define_msg! { pub CannotIndex<'a> { tab: Slot<'a>, key: Slot<'a> }:
    "ko" => "`{tab}`에 `{key}`을(를) 키로 써서 인덱싱할 수 없습니다",
    _    => "Cannot index `{tab}` with `{key}`",
}

// a special case of CannotIndex when `key` is a string literal
define_msg! { pub CannotIndexWithStr<'a> { tab: Slot<'a>, key: &'a Str }:
    "ko" => "`{tab}`에 {key}이(가) 없습니다",
    _    => "Missing key {key} in `{tab}`",
}

define_msg! { pub CannotCreateIndex<'a> { tab: Slot<'a>, key: Slot<'a>, specrhs: Slot<'a> }:
    "ko" => "`{tab}`에 `{key}`을(를) 키로 써서 `{specrhs}` 타입의 필드를 새로 만들 수 없습니다",
    _    => "Cannot index `{tab}` with `{key}` and create a new field of the type `{specrhs}`",
}

define_msg! { pub CannotAssign<'a> { lhs: Slot<'a>, rhs: Slot<'a> }:
    "ko" => "`{lhs}` 타입에 `{rhs}` 타입을 대입할 수 없습니다",
    _    => "Cannot assign `{rhs}` into `{lhs}`",
}

define_msg! { pub NonNumericFor:
    "ko" => "`for` 문의 인자로 숫자가 아닌 타입(들)이 쓰였습니다",
    _    => "`for` statement was given non-numeric type(s)",
}

define_msg! { pub NonFuncIterator<'a> { iter: Ty<'a> }:
    "ko" => "`for`-`in` 문에 주어진 반복자가 함수가 아닌 `{iter}` 타입을 반환했습니다",
    _    => "The iterator given to `for`-`in` statement returned a non-function type `{iter}`",
}

define_msg! { pub BadFuncIterator<'a> { iter: Ty<'a> }:
    "ko" => "`for`-`in` 문에 주어진 반복자가 예상치 못한 `{iter}` 타입을 반환했습니다",
    _    => "The iterator given to `for`-`in` statement returned an unexpected type `{iter}`",
}

define_msg! { pub CannotExtendImplicitReturnType:
    "ko" => "이 함수의 반환 타입을 암묵적으로 확장할 수 없습니다",
    _    => "Cannot extend the implicit return type of this function",
}

define_msg! { pub CannotReturn<'a> { returns: SpannedTySeq<'a>, ty: SpannedTySeq<'a> }:
    "ko" => "지정된 `{returns}` 타입과 호환되지 않는 `{ty}`을(를) 반환하려 했습니다",
    _    => "Attempted to return a type `{ty}` which is incompatible to \
             given return type `{returns}`",
}

define_msg! { pub BadRecursiveCall:
    "ko" => "재귀호출되는 함수가 필요로 하는 타입과 실제 타입이 호환되지 않습니다",
    _    => "A required type and the actual type of the recursive function is not compatible",
}

define_msg! { pub BuiltinGivenLessArgs<'a> { name: &'a str, nargs: usize }:
    "ko" => "`{name}` 내장 함수는 인자가 적어도 {nargs}개 필요합니다",
    _    => "`{name}` needs at least {nargs} argument(s)",
}

define_msg! { pub CannotOpenLibrary:
    "ko" => "`--# open` 명령에 주어진 내장 라이브러리 이름을 찾을 수 없습니다",
    _    => "Cannot find the built-in library name given to `--# open` directive",
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

define_msg! { pub DuplicateAttrInSig:
    "ko" => "이미 속성이 붙어 있는 함수 명세에 속성을 더 붙일 수 없습니다",
    _    => "Cannot add an attribute to a function specification with an existing attribute",
}

define_msg! { pub CannotAssignToPackagePath<'a> { name: &'a str }:
    "ko" => "`{name}` 내장 변수에 값을 저장하다 문제가 생겨서 \
             `require` 경로를 찾는데 문제가 있을 수 있습니다",
    _    => "Cannot assign to the `{name}` built-in variable; \
             subsequent `require` may be unable to find the module path",
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

#[cfg(feature = "warn_on_useless_conds")]
define_msg! { pub IgnoredIfCase:
    "ko" => "`if` 문의 이 조건(들)은 실행되지 않습니다",
    _    => "These `if` case(s) are never executed",
}

#[cfg(feature = "warn_on_useless_conds")]
define_msg! { pub IfCaseWithTruthyCond:
    "ko" => "이 조건이 항상 참인 값으로 평가됩니다",
    _    => "This condition always evaluates to a truthy value",
}

#[cfg(feature = "warn_on_useless_conds")]
define_msg! { pub IfCaseWithFalsyCond:
    "ko" => "이 조건이 항상 거짓인 값으로 평가됩니다",
    _    => "This condition always evaluates to a falsy value",
}

#[cfg(feature = "warn_on_dead_code")]
define_msg! { pub DeadCode:
    "ko" => "이 코드는 실행되지 않을 것입니다",
    _    => "This code will never execute",
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

define_msg! { pub NoCtor:
    "ko" => "생성자(`init` 메소드)가 없이 `new` 메소드를 호출할 수 없습니다",
    _    => "The `new` method cannot be called with no constructor (`init` method) defined",
}

define_msg! { pub CannotAccessCtorThruInstance:
    "ko" => "생성자(`init` 메소드)는 클래스 인스턴스를 통해 접근할 수 없습니다",
    _    => "The constructor (`init` method) should not be accessed through instances",
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

define_msg! { pub ReservedNewMethod:
    "ko" => "`new` 메소드는 예약되어 있으며 선언될 수 없습니다",
    _    => "`new` method is reserved and cannot be defined",
}

define_msg! { pub NoInheritanceInDumbClassSystem:
    "ko" => "클래스 시스템에 속하지 않은 클래스는 상속이 지원되지 않습니다",
    _    => "No inheritance is supported for classes without a class system",
}

define_msg! { pub CannotCreateFieldDefinedInInstance<'a> { key: &'a Key }:
    "ko" => "인스턴스에 `{key}` 키가 이미 선언되어 있어 클래스에 같은 키를 선언할 수 없습니다",
    _    => "Cannot create a class field with the key `{key}` already defined in instances",
}

define_msg! { pub CannotCreateFieldDefinedInChildren<'a> { key: &'a Key }:
    "ko" => "하위 클래스에 `{key}` 키가 이미 선언되어 있어 같은 키를 선언할 수 없습니다",
    _    => "Cannot create a field with the key `{key}` already defined in ancestor classes",
}

define_msg! { pub NoCheckRequiresTypedSelf:
    "ko" => "[NO_CHECK] 속성이 주어졌을 경우 `self` 인자의 타입이 명백해야 합니다. \
             대신 함수 선언 문법으로 타입을 직접 지정하십시오",
    _    => "[NO_CHECK] attribute requires that the type for `self` argument is clear; \
             directly specify the type for `self` with the function declaration instead",
}

define_msg! { pub NoCheckRequiresTypedArgs:
    "ko" => "[NO_CHECK] 속성이 주어졌을 경우 인자에 타입이 주어져야 합니다",
    _    => "[NO_CHECK] attribute requires the arguments to be typed",
}

define_msg! { pub NoCheckRequiresTypedVarargs:
    "ko" => "[NO_CHECK] 속성이 주어졌을 경우 가변 인자에 타입이 주어져야 합니다",
    _    => "[NO_CHECK] attribute requires the variadic arguments to be typed",
}

define_msg! { pub NoCheckRequiresTypedReturns:
    "ko" => "[NO_CHECK] 속성이 주어졌을 경우 함수의 반환 타입이 주어져야 합니다",
    _    => "[NO_CHECK] attribute requires the return type to be present",
}

define_msg! { pub ModuleRequiresTypedSelf:
    "ko" => "`module`로 타입 체크를 지연하려면 `self` 인자의 타입이 명백해야 합니다. \
             대신 함수 선언 문법으로 타입을 직접 지정하십시오",
    _    => "Delayed type checking via `module` requires that \
             the type for `self` argument is clear; \
             directly specify the type for `self` with the function declaration instead",
}

define_msg! { pub ModuleRequiresTypedArgs:
    "ko" => "`module`로 타입 체크를 지연하려면 인자에 타입이 주어져야 합니다",
    _    => "Delayed type checking via `module` requires the arguments to be typed",
}

define_msg! { pub ModuleRequiresTypedVarargs:
    "ko" => "`module`로 타입 체크를 지연하려면 가변 인자에 타입이 주어져야 합니다",
    _    => "Delayed type checking via `module` requires the variadic arguments to be typed",
}

define_msg! { pub ModuleRequiresTypedReturns:
    "ko" => "`module`로 타입 체크를 지연하려면 함수의 반환 타입이 주어져야 합니다",
    _    => "Delayed type checking via `module` requires the return type to be present",
}

define_msg! { pub AssumeFieldToUnknownType:
    "ko" => "`--# assume` 명령이 아직 완전히 추론되지 않은 타입에서 필드를 접근하려 했습니다",
    _    => "`--# assume` directive tried to access a field from a type not yet known enough",
}

define_msg! { pub AssumeFieldToInstance<'a> { slot: Slot<'a> }:
    "ko" => "`--# assume` 명령을 클래스 프로토타입이 아닌 인스턴스 `{slot}`에 적용할 수 없습니다",
    _    => "`--# assume` directive cannot be applied to a class instance `{slot}` \
             instead of its prototype",
}

define_msg! { pub AssumeFieldToUnknownClass<'a> { cls: Slot<'a> }:
    "ko" => "`{cls}` 타입이 정확히 하나의 클래스로 추론되지 않아 \
             `--# assume` 명령을 적용할 수 없습니다",
    _    => "Cannot apply `--# assume` directive to `{cls}` \
             that cannot be inferred to a single class",
}

define_msg! { pub AssumeFieldNestedToClass<'a> { cls: T<'a> }:
    "ko" => "`{cls}` 클래스 프로토타입의 필드 내부에 `--# assume` 명령을 적용할 수 없습니다",
    _    => "Cannot apply `--# assume` directive to the inside of fields \
             in the class prototype of `{cls}`",
}

// this error can also occur for `--# assume C.x: method()`,
// so we have to avoid a mention to `--# assume static`
define_msg! { pub AssumeFieldStaticToNonClass<'a> { slot: Slot<'a> }:
    "ko" => "`--# assume`으로 클래스가 아닌 `{slot}` 타입에 정적 필드를 추가할 수 없습니다",
    _    => "`--# assume` cannot be used to add a static field to a non-class type `{slot}`",
}

define_msg! { pub AssumeFieldToNonRecord<'a> { slot: Slot<'a> }:
    "ko" => "`--# assume` 명령이 레코드가 아닌 `{slot}` 타입에서 필드를 접근하려 했습니다",
    _    => "`--# assume` directive tried to access a field from a non-record type `{slot}`",
}

define_msg! { pub AssumeFieldToMissing:
    "ko" => "`--# assume` 명령이 존재하지 않는 필드를 접근하려 했습니다",
    _    => "`--# assume` directive tried to access a missing field",
}

define_msg! { pub AssumeExistingField:
    "ko" => "`--# assume` 명령이 이미 있는 필드를 덮어 씌우려 합니다",
    _    => "`--# assume` directive tried to overwrite an existing field",
}

// this error should be avoided as much as possible, it doesn't give the exact reason
define_msg! { pub AssumeCannotCreateNewField:
    "ko" => "`--# assume` 명령이 새 필드를 생성할 수 없습니다",
    _    => "`--# assume` directive cannot create a new field",
}

define_msg! { pub NoSuchClassSystem<'a> { name: &'a Name }:
    "ko" => "{name} 클래스 시스템이 정의되지 않았습니다",
    _    => "{name} class system hasn't been defined",
}

define_msg! { pub ClassSystemAlreadyExists<'a> { name: &'a Name }:
    "ko" => "{name} 클래스 시스템이 이미 존재합니다",
    _    => "{name} class system already exists",
}

define_msg! { pub PreviousClassSystem:
    "ko" => "클래스 시스템이 여기에서 이미 선언되었습니다",
    _    => "Previous definition of the class system here",
}

define_msg! { pub NoSuchPredefinedClassSystem<'a> { name: &'a Name }:
    "ko" => "{name} 클래스 시스템은 아직 지원되지 않습니다",
    _    => "{name} class system is not yet supported",
}

define_msg! { pub TooManyClassSystems:
    "ko" => "클래스 시스템은 최대 256개까지 선언할 수 있습니다",
    _    => "There may be at most 256 class systems defined",
}

define_msg! { pub BadClassParent<'a> { ty: Ty<'a> }:
    "ko" => "클래스가 아닌 `{ty}` 타입은 부모 클래스가 될 수 없습니다",
    _    => "The non-class type `{ty}` cannot be a parent class",
}

define_msg! { pub NotSubtypeOfParentField<'a> { key: &'a Key, sub: Slot<'a>, sup: Slot<'a> }:
    "ko" => "부모 클래스의 `{key}` 필드를 오버라이드하려 했으나 \
             변경 가능한 클래스 안에서는 `{sub}`이(가) 기존 타입 `{sup}`의 서브타입이 아닙니다",
    _    => "Tried to override a field `{key}` in a parent class \
             but `{sub}` is not a subtype of `{sup}` when being inside the mutable class",
}

define_msg! { pub PreviousParentFieldType:
    "ko" => "기존 타입은 여기에서 선언되었습니다",
    _    => "Previous definition of the field type here",
}

define_msg! { pub MissingParentClassForGideros:
    "ko" => "`gideros` 클래스 시스템에서 부모가 없는 클래스는 하나만 존재할 수 있습니다",
    _    => "There should be a single class without a parent in the `gideros` class system",
}

define_msg! { pub NotTVar<'a> { slot: Slot<'a> }:
    "ko" => "내부 오류: `{slot}` 타입이 타입 변수가 아닙니다",
    _    => "Internal Error: A type `{slot}` is not a type variable",
}

#[cfg(feature = "no_implicit_func_sig")]
define_msg! { pub ImplicitSigOnNamedFunc:
    "ko" => "이름이 붙은 함수의 모든 인자에는 타입이 붙어야 합니다",
    _    => "Every argument in the named function should have a type specified",
}

#[cfg(feature = "no_implicit_func_sig")]
define_msg! { pub ImplicitArgTypeOnAnonymousFunc:
    "ko" => "익명 함수의 인자에 타입이 없고 호출로부터 추론할 수도 없습니다",
    _    => "The type for this argument in the anonymous function is missing \
             but couldn't be inferred from the calls",
}

#[cfg(feature = "no_implicit_func_sig")]
define_msg! { pub ImplicitVarargsTypeOnAnonymousFunc:
    "ko" => "익명 함수의 가변 인자에 타입이 없고 호출로부터 추론할 수도 없습니다",
    _    => "The type for variadic arguments in the anonymous function is missing \
             but couldn't be inferred from the calls",
}

define_msg! { pub DivergingInExpr:
    "ko" => "중간에 반환하지 않는 함수 호출이 있어서 일부 수식은 영원히 평가되지 않습니다",
    _    => "A portion of this expression won't be evaluated \
             because it contains a call to a function that never returns",
}

define_msg! { pub ReturnInDivergingFunc:
    "ko" => "반환하지 않도록 지정된 함수 안에서 반환하려고 했습니다",
    _    => "Tried to return from a function that is marked that it never returns",
}

define_msg! { pub ClassInheritFromDifferentClassSystem:
    "ko" => "이 클래스는 다른 클래스 시스템을 쓰는 클래스에서 상속받을 수 없습니다",
    _    => "The class cannot inherit from a class using a different class system",
}

