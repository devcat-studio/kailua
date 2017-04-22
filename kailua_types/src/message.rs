use l10nutils::Ordinal;
use ty::{self, Key, Displayed, TypeContext};
use kailua_syntax::Name;

pub type Ty<'a> = Displayed<'a, ty::Ty, &'a TypeContext>;

define_msg! { pub NotSubtype<'a> { sub: &'a str, sup: &'a str }:
    "ko" => "`{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "`{sub}` is not a subtype of `{sup}`",
}

define_msg! { pub NotSubtypeInSelf<'a> { sub: &'a str, sup: &'a str }:
    "ko" => "`self` 자리에 있는 `{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "`{sub}` in the `self` position is not a subtype of `{sup}`",
}

define_msg! { pub NotSubtypeInFuncArgs<'a> { sub: &'a str, sup: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 인자 `{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "{index:+} function argument `{sub}` is not a subtype of `{sup}`",
}

define_msg! { pub NotSubtypeInMethodArgs<'a> { sub: &'a str, sup: &'a str, index: Ordinal }:
    "ko" => "메소드의 {index} 인자 `{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "{index:+} method argument `{sub}` is not a subtype of `{sup}`",
}

define_msg! { pub NotSubtypeInReturns<'a> { sub: &'a str, sup: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 반환값인 `{sub}`이(가) `{sup}`의 서브타입이 아닙니다",
    _    => "{index:+} return type `{sub}` is not a subtype of `{sup}`",
}

define_msg! { pub NotEqual<'a> { lhs: &'a str, rhs: &'a str }:
    "ko" => "`{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "`{lhs}` does not equal to `{rhs}`",
}

define_msg! { pub NotEqualInSelf<'a> { lhs: &'a str, rhs: &'a str }:
    "ko" => "`self` 자리에 있는 `{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "`{lhs}` in the `self` position does not equal to `{rhs}`",
}

define_msg! { pub NotEqualInFuncArgs<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 인자 `{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "{index:+} function argument `{lhs}` does not equal to `{rhs}`",
}

define_msg! { pub NotEqualInMethodArgs<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "메소드의 {index} 인자 `{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "{index:+} method argument `{lhs}` does not equal to `{rhs}`",
}

define_msg! { pub NotEqualInReturns<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 반환값인 `{lhs}`와(과) `{rhs}`이(가) 같은 타입이 아닙니다",
    _    => "{index:+} return type `{lhs}` does not equal to `{rhs}`",
}

define_msg! { pub CannotUnionType<'a> { ty: &'a str }:
    "ko" => "`{ty}` 타입을 포함하는 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type including `{ty}`",
}

define_msg! { pub InvalidUnionType<'a> { lhs: &'a str, rhs: &'a str }:
    "ko" => "`{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}`",
}

define_msg! { pub InvalidUnionTypeInSelf<'a> { lhs: &'a str, rhs: &'a str }:
    "ko" => "`self` 자리에 있는 `{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}` in the `self` position",
}

define_msg! { pub InvalidUnionTypeInFuncArgs<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 인자에서 `{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}` in the {index} function argument",
}

define_msg! { pub InvalidUnionTypeInMethodArgs<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "메소드의 {index} 인자에서 `{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}` in the {index} method argument",
}

define_msg! { pub InvalidUnionTypeInReturns<'a> { lhs: &'a str, rhs: &'a str, index: Ordinal }:
    "ko" => "함수의 {index} 반환값에서 `{lhs}`와(과) `{rhs}`의 합 타입을 만들 수 없습니다",
    _    => "Cannot create a union type of `{lhs}` and `{rhs}` in the {index} return type",
}

define_msg! { pub ArityMismatch<'a> { other: &'a str, index: Ordinal }:
    "ko" => "반대편 타입이 `{other}`이기 때문에 {index} 타입을 생략할 수 없습니다",
    _    => "{index:+} type cannot be omitted because the other type is `{other}`",
}

define_msg! { pub LessArityInFuncArgs<'a> { other: &'a str, index: Ordinal }:
    "ko" => "명시된 타입이 `{other}`이기 때문에 함수의 {index} 인자를 생략할 수 없습니다",
    _    => "{index:+} function argument cannot be omitted because its type is `{other}`",
}

define_msg! { pub LessArityInMethodArgs<'a> { other: &'a str, index: Ordinal }:
    "ko" => "반대편 타입이 `{other}`이기 때문에 메소드의 {index} 인자를 생략할 수 없습니다",
    _    => "{index:+} method argument cannot be omitted because its type is `{other}`",
}

define_msg! { pub LessArityInReturns<'a> { other: &'a str, index: Ordinal }:
    "ko" => "반대편 타입이 `{other}`이기 때문에 {index} 반환값을 생략할 수 없습니다",
    _    => "{index:+} return value cannot be omitted because its type is `{other}`",
}

define_msg! { pub MoreArityInFuncArgs { index: usize }:
    "ko" => "함수에 {index}개를 넘는 인자를 넣을 수 없습니다",
    _    => "Cannot give more than {index} argument(s) to the function",
}

define_msg! { pub MoreArityInMethodArgs { index: usize }:
    "ko" => "`self`를 포함해 메소드에 {index}개를 넘는 인자를 넣을 수 없습니다",
    _    => "Cannot give more than {index} argument(s) including `self` to the method",
}

define_msg! { pub MoreArityInReturns { index: usize }:
    "ko" => "{index}개를 넘는 값을 반환할 수 없습니다",
    _    => "Cannot return more than {index} value(s)",
}

define_msg! { pub OtherTypeOrigin:
    "ko" => "다른 타입은 여기에서 만들어졌습니다",
    _    => "The other type originates here",
}

// TODO should point to the correct span
define_msg! { pub InextensibleRec:
    "ko" => "레코드 타입에 더 이상 새 필드를 추가할 수 없습니다",
    _    => "No longer possible to add a new field to this record type",
}

// TODO should point to the correct span
define_msg! { pub RecursiveRec:
    "ko" => "레코드 타입에서 재귀 참조가 발견되었습니다",
    _    => "Recursive cycles detected in the record type",
}

// TODO should point to the correct span
define_msg! { pub RecDuplicateKey<'a> { key: &'a Key }:
    "ko" => "레코드 타입이 `{key}` 필드를 중복으로 가집니다",
    _    => "Duplicate key `{key}` found in the record type",
}

// TODO should point to the correct span
define_msg! { pub RecCannotHaveKey<'a> { key: &'a Key }:
    "ko" => "레코드 타입이 `{key}` 필드를 가질 수 없습니다",
    _    => "The record cannot have a field with the key `{key}`",
}

// TODO should point to the correct span
define_msg! { pub RecShouldHaveKeys<'a> { keys: &'a str }:
    "ko" => "레코드 타입이 {keys} 필드를 포함하지 않습니다",
    _    => "The record does not have a field with the key(s) {keys}",
}

// TODO should point to the correct span
define_msg! { pub RecExtendedWithNonNil<'a> { key: &'a Key, slot: &'a str }:
    "ko" => "레코드 타입에 원래 존재하지 않던 `{key}` 필드는 \
             명시적으로 nil을 포함하지 않는 `{slot}` 타입으로 추가될 수 없습니다",
    _    => "The record cannot add a new field with the key `{key}` and \
             the value type `{slot}` that is not explicitly nilable",
}

// should be same to kailua_check's version
define_msg! { pub CannotUpdate<'a> { tab: &'a str }:
    "ko" => "변경할 수 없는 `{tab}` 타입을 인덱싱해서 갱신할 수 없습니다",
    _    => "Cannot update the immutable type `{tab}` by indexing",
}

// should be same to kailua_check's version
define_msg! { pub CannotAssign<'a> { lhs: &'a str, rhs: &'a str }:
    "ko" => "`{lhs}` 타입에 `{rhs}` 타입을 대입할 수 없습니다",
    _    => "Cannot assign `{rhs}` into `{lhs}`",
}

define_msg! { pub CannotFilter<'a> { ty: &'a str }:
    "ko" => "`{ty}` 타입을 좁힐 수 없습니다",
    _    => "Cannot narrow `{ty}`",
}

define_msg! { pub UnknownAttrName<'a> { name: &'a Name }:
    "ko" => "{name} 타입 속성을 알 수 없어서 무시합니다",
    _    => "{name} is an unknown type attribute and ignored",
}

define_msg! { pub DuplicateAttr<'a> { ty: Ty<'a> }:
    "ko" => "이미 속성이 붙어 있는 `{ty}` 타입에 속성을 더 붙일 수 없습니다",
    _    => "Cannot add an attribute to a type `{ty}` with an existing attribute",
}

define_msg! { pub UnsupportedErrorType:
    "ko" => "`error \"메시지\"` 타입은 아직 지원되지 않습니다",
    _    => "`error \"message\"` type is not yet supported",
}

define_msg! { pub DuplicateFieldNameInRec<'a> { name: &'a Name }:
    "ko" => "타입에서 레코드 이름 {name}이 중복됩니다",
    _    => "Duplicate record field {name} in the type specification",
}

define_msg! { pub FirstFieldNameInRec:
    "ko" => "여기서 처음 나왔습니다",
    _    => "The first duplicate appeared here",
}

define_msg! { pub UnsupportedUnionTypeSpec:
    "ko" => "이 합 타입은 타입 명세에서 지원되지 않습니다",
    _    => "This union type is not supported in the specification",
}

