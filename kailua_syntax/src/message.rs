use lang::Lua;
use lex::Tok;
use ast::Name;
use parser::Expectable;

define_msg! { pub NoFileForSpan:
    "ko" => "주어진 코드 범위에 대응하는 소스 파일이 존재하지 않습니다",
    _    => "There exists no source file corresponding to given span",
}

// lexer messages

define_msg! { pub BadSurrogate:
    "ko" => "잘못된 UTF-16 서로게이트열이 파일에 포함되어 있습니다",
    _    => "The file contains a bad UTF-16 surrogate sequence",
}

define_msg! { pub PrematureEofInString:
    "ko" => "문자열을 읽던 중 파일이 끝났습니다",
    _    => "Premature end of file in a string",
}

define_msg! { pub UnescapedNewlineInString:
    "ko" => "문자열에 탈출되지 않은 개행 문자가 들어 있습니다",
    _    => "Unescaped newline in a string",
}

define_msg! { pub UnclosedOpeningLongString:
    "ko" => "긴 문자열을 여는 `[`가 제대로 닫히지 않았습니다",
    _    => "Opening long bracket in a string should end with `[`",
}

define_msg! { pub PrematureEofInLongString:
    "ko" => "긴 문자열을 읽던 중 파일이 끝났습니다",
    _    => "Premature end of file in a long string",
}

define_msg! { pub PrematureEofInLongComment:
    "ko" => "긴 주석을 읽던 중 파일이 끝났습니다",
    _    => "Premature end of file in a long comment",
}

define_msg! { pub NoNewlineInLongCommentInMeta:
    "ko" => "카일루아 블록 안에 있는 긴 주석에는 개행 문자가 들어갈 수 없습니다",
    _    => "A newline is disallowed in a long comment inside the meta block",
}

define_msg! { pub NoNewlineInLongStringInMeta:
    "ko" => "카일루아 블록 안에 있는 긴 문자열에는 개행 문자가 들어갈 수 없습니다",
    _    => "A newline is disallowed in a long string inside the meta block",
}

define_msg! { pub UnrecognizedEscapeInString:
    "ko" => "문자열 안에 알 수 없는 탈출열이 있습니다",
    _    => "Unrecognized escape sequence in a string",
}

define_msg! { pub StringStart:
    "ko" => "문자열 리터럴은 여기서 시작되었습니다",
    _    => "The string started here",
}

define_msg! { pub LongStringStart:
    "ko" => "긴 문자열 리터럴은 여기서 시작되었습니다",
    _    => "The long string started here",
}

define_msg! { pub LongCommentStart:
    "ko" => "긴 주석은 여기서 시작되었습니다",
    _    => "The long comment started here",
}

define_msg! { pub MetaStart:
    "ko" => "카일루아 블록은 여기서 시작되었습니다",
    _    => "The meta block started here",
}

define_msg! { pub InvalidNumber:
    "ko" => "숫자 형식이 잘못되었습니다",
    _    => "Invalid number",
}

define_msg! { pub UnexpectedChar:
    "ko" => "알 수 없는 문자가 나왔습니다",
    _    => "Unexpected character",
}

// parser messages

define_msg! { pub ExpectFailed<'a, Exp: Expectable> { expected: Exp, read: &'a Tok }:
    "ko" => "{expected}이(가) 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected {expected}, got {read}",
}

define_msg! { pub NoNewline<'a> { read: &'a Tok }:
    "ko" => "개행 문자가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a newline, got {read}",
}

define_msg! { pub NoName<'a> { read: &'a Tok }:
    "ko" => "이름이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name, got {read}",
}

define_msg! { pub NoExp<'a> { read: &'a Tok }:
    "ko" => "수식이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected an expression, got {read}",
}

define_msg! { pub NoFuncCall:
    "ko" => "문장 위치에 나오는 수식은 함수 호출이어야 합니다",
    _    => "Only function calls are allowed as statement-level expressions",
}

define_msg! { pub NoStmt<'a> { read: &'a Tok }:
    "ko" => "문장이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a statement, got {read}",
}

define_msg! { pub NoVar<'a> { read: &'a Tok }:
    "ko" => "변수나 인덱스 수식이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a left-hand-side expression, got {read}",
}

define_msg! { pub NoVarButExp:
    "ko" => "대입문 좌항에 변수나 인덱스 수식이 아닌 수식이 들어 있습니다",
    _    => "Got a non-assignable expression at the left hand side of assignment",
}

define_msg! { pub NoKind<'a> { read: &'a Tok }:
    "ko" => "타입이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a type, got {read}",
}

define_msg! { pub NoType<'a> { read: &'a Tok }:
    "ko" => "타입이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a type, got {read}",
}

define_msg! { pub NoSingleType<'a> { read: &'a Tok }:
    "ko" => "하나의 타입이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a single type, got {read}",
}

define_msg! { pub NoTypeOrTypeSeq<'a> { read: &'a Tok }:
    "ko" => "하나의 타입이나 타입열이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a single type or type sequence, got {read}",
}

define_msg! { pub NoKindParams<'a> { read: &'a Tok }:
    "ko" => "타입 인자의 목록이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a list of type parameters, got {read}",
}

define_msg! { pub NoEq<'a> { read: &'a Tok }:
    "ko" => "`=`이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `=`, got {read}",
}

define_msg! { pub NoTableSep<'a> { read: &'a Tok }:
    "ko" => "`,`, `;`이나 `}}`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `,`, `;` or `}}`, got {read}",
}

define_msg! { pub NoForInSep<'a> { read: &'a Tok }:
    "ko" => "`=`, `,`, `in`이나 `--:`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `=`, `,`, `in` or `--:` after `for NAME`, got {read}",
}

define_msg! { pub NoFuncArgs<'a> { read: &'a Tok }:
    "ko" => "`function`이나 `function <이름>` 뒤에 `(`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `(` after `function` or `function <name>`, got {read}",
}

define_msg! { pub BadFuncArg<'a> { read: &'a Tok }:
    "ko" => "인자 이름, `)` 또는 `...`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected an argument name, `)` or `...`, got {read}",
}

define_msg! { pub NoNameAfterExpDot<'a> { read: &'a Tok }:
    "ko" => "`<수식> .` 뒤에 이름이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name after `<expression> .`, got {read}",
}

define_msg! { pub NoArgsAfterExpColon<'a> { read: &'a Tok }:
    "ko" => "`<수식> :` 뒤에 이름이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name after `<expression> :`, got {read}",
}

define_msg! { pub NoArgsAfterExpColonName<'a> { read: &'a Tok }:
    "ko" => "`<수식> : <이름>` 뒤에 인자가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected argument(s) after `<expression> : <name>`, got {read}",
}

define_msg! { pub NoFuncOrNameAfterLocal<'a> { read: &'a Tok }:
    "ko" => "`local` 뒤에 이름이나 `function`이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name or `function` after `local`, got {read}",
}

define_msg! { pub NoFunctionOrMethodBeforeSig<'a> { read: &'a Tok }:
    "ko" => "함수 명세 앞에 `function`이나 `method`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a `function` or `method` before the function specification, got {read}",
}

define_msg! { pub DuplicateTypeSpecInLocal:
    "ko" => "타입이 변수 이름과 `local` 선언의 뒷쪽에 동시에 나왔습니다",
    _    => "The type specification cannot appear both at variable names and \
             after the `local` declaration",
}

define_msg! { pub ExcessNamesInLocal:
    "ko" => "선언되는 변수가 선언될 타입보다 많습니다",
    _    => "Excess type specifications in the variable names",
}

define_msg! { pub ExcessTypeSpecsInLocal:
    "ko" => "선언되는 타입이 선언될 변수보다 많습니다",
    _    => "Excess type specifications after the `local` declaration",
}

define_msg! { pub DuplicateTypeSpecInAssign:
    "ko" => "타입이 대입문의 좌항과 대입문의 뒷쪽에 동시에 나왔습니다",
    _    => "The type specification cannot appear both at the left hand side and \
             after the assignment",
}

define_msg! { pub ExcessLvaluesInAssign:
    "ko" => "대입되는 좌항이 대입될 타입보다 많습니다",
    _    => "Excess type specifications in the left hand side",
}

define_msg! { pub ExcessTypeSpecsInAssign:
    "ko" => "대입되는 타입이 대입될 좌항보다 많습니다",
    _    => "Excess type specifications after the assignment",
}

define_msg! { pub MissingFuncDeclAfterFuncSpec:
    "ko" => "함수 타입 뒤에 함수 선언이 없습니다",
    _    => "No function declaration after the function specification",
}

define_msg! { pub MissingFuncLitAfterFuncSpec:
    "ko" => "함수 타입 뒤에 함수 리터럴이 없습니다",
    _    => "No function literal after the function specification",
}

define_msg! { pub NoModfAllowedInVarargs:
    "ko" => "가변 인자에는 변수 종류를 사용할 수 없습니다",
    _    => "Variadic argument specifier cannot have modifiers",
}

define_msg! { pub ExcessArgsInFuncDecl:
    "ko" => "함수 타입보다 함수 선언에 인자 수가 더 많습니다",
    _    => "Excess arguments in the function declaration",
}

define_msg! { pub ExcessArgsInFuncSpec:
    "ko" => "함수 선언보다 함수 타입에 인자 수가 더 많습니다",
    _    => "Excess arguments in the function specification",
}

define_msg! { pub MissingArgTypeInFuncSpec:
    "ko" => "함수 타입에 주어진 인자에 타입이 붙어 있지 않습니다",
    _    => "Arguments in the function specification are missing their types",
}

define_msg! { pub MissingVarargsInFuncDecl:
    "ko" => "가변 인자가 함수 선언에는 있는데 함수 타입에는 없습니다",
    _    => "Variadic arguments appear in the function specification \
             but not in the function itself",
}

define_msg! { pub MissingVarargsInFuncSpec:
    "ko" => "가변 인자가 함수 타입에는 있는데 함수 선언에는 없습니다",
    _    => "Variadic arguments appear in the function but not in the function specification",
}

define_msg! { pub DuplicateSpecInFuncDecl:
    "ko" => "인자의 타입이 함수 타입과 함수 선언에 중복으로 들어 있습니다",
    _    => "Inline argument type specification cannot appear with the function specification",
}

define_msg! { pub DuplicateVarargsSpecInFuncDecl:
    "ko" => "가변 인자의 타입이 함수 타입과 함수 선언에 중복으로 들어 있습니다",
    _    => "Inline variadic argument type specification cannot \
             appear with the function specification",
}

define_msg! { pub DuplicateReturnSpecInFuncDecl:
    "ko" => "함수 반환 타입이 함수 타입과 함수 선언에 중복으로 들어 있습니다",
    _    => "Inline return type specification cannot appear with the function specification",
}

define_msg! { pub ArgNameMismatchInFuncDecl:
    "ko" => "함수 선언에 나온 인자 이름이 함수 타입과 맞지 않습니다",
    _    => "Mismatching argument name in the function specification",
}

define_msg! { pub PriorVarargsSpecInFuncSpec:
    "ko" => "함수 타입에 이미 가변 인자가 있습니다",
    _    => "The corresponding argument in the function specification was here",
}

define_msg! { pub PriorArgNameInFuncSpec:
    "ko" => "함수 타입의 원래 인자 이름은 이렇습니다",
    _    => "The corresponding argument was here",
}

define_msg! { pub PriorFuncSpec:
    "ko" => "기존 함수 타입은 여기 있습니다",
    _    => "The function specification appeared here",
}

define_msg! { pub NoKindBeforeEllipsis:
    "ko" => "함수 명세가 아닌 곳에서는 `...` 앞에 항상 타입이 존재해야 합니다",
    _    => "`...` should be preceded with a kind outside of the function specification",
}

define_msg! { pub VarargsNameInFuncKind:
    "ko" => "가변 인자에는 이름이 붙을 수 없습니다",
    _    => "Variadic arguments cannot have a name",
}

define_msg! { pub DuplicateFieldNameInRec<'a> { name: &'a Name }:
    "ko" => "타입에서 레코드 이름 {name}이 중복됩니다",
    _    => "Duplicate record field {name} in the type specification",
}

define_msg! { pub FirstFieldNameInRec:
    "ko" => "여기서 처음 나왔습니다",
    _    => "The first duplicate appeared here",
}

define_msg! { pub DuplicateArgNameInFuncKind<'a> { name: &'a Name }:
    "ko" => "타입에서 인자 이름 {name}이 중복됩니다",
    _    => "Duplicate argument name {name} in the type specification",
}

define_msg! { pub FirstArgNameInFuncKind:
    "ko" => "여기서 처음 나왔습니다",
    _    => "The first duplicate appeared here",
}

define_msg! { pub PartiallyNamedFieldsInFuncKind:
    "ko" => "타입에서 일부 인자에만 이름이 붙어 있습니다",
    _    => "Not all but only some arguments in the type are named",
}

define_msg! { pub FunctionWithMethodSig:
    "ko" => "일반 함수의 명세는 `function`으로 시작해야 합니다",
    _    => "A function specification for ordinary functions should start with `function`",
}

define_msg! { pub MethodWithFuncSig:
    "ko" => "메소드의 함수 명세는 `method`로 시작해야 합니다",
    _    => "A function specification for methods should start with `method`",
}

define_msg! { pub NoTypeSeqInUnion:
    "ko" => "합집합 타입에는 타입열이 들어갈 수 없습니다",
    _    => "A sequence of types cannot be inside a union",
}

define_msg! { pub NoSingleTypeButTypeSeq:
    "ko" => "하나의 타입이 나와야 하는데 타입열이 나왔습니다",
    _    => "Expected a single type, not type sequence",
}

define_msg! { pub CannotRedefineBuiltin:
    "ko" => "내장 타입은 재선언할 수 없습니다",
    _    => "Cannot redefine a builtin type",
}

define_msg! { pub AttrToKindSeq:
    "ko" => "[name] 꼴의 타입 속성 선언은 타입열에는 붙일 수 없습니다",
    _    => "Cannot attach the type attribute (like [name]) to the type sequence",
}

define_msg! { pub StmtAfterReturnOrBreak:
    "ko" => "`return`이나 `break` 다음에는 다른 문장이 올 수 없습니다",
    _    => "`return` or `break` cannot be followed by other statements",
}

define_msg! { pub WrongVectorParamsArity:
    "ko" => "`vector` 타입에는 타입 인자가 하나 있어야 합니다",
    _    => "`vector` type needs a single type parameter",
}

define_msg! { pub WrongMapParamsArity:
    "ko" => "`map` 타입에는 타입 인자가 두 개 있어야 합니다",
    _    => "`map` type needs two type parameters",
}

define_msg! { pub WrongMapParamsModf:
    "ko" => "`map` 타입의 첫 타입 인자에는 변수 종류를 사용할 수 없습니다",
    _    => "The first type parameter of `map` type cannot have modifiers",
}

define_msg! { pub ReservedKindName<'a> { name: &'a Name }:
    "ko" => "{name} 타입 이름은 예약되어 있으며 사용할 수 없습니다",
    _    => "The type name {name} is reserved and cannot be used",
}

define_msg! { pub NoKindParamsClose<'a> { read: &'a Tok }:
    "ko" => "`>`이나 `>>`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `>` or `>>`, got {read}",
}

define_msg! { pub FutureKeyword<'a> { read: &'a Tok, current: Lua, future: Lua }:
    "ko" => "{read}은(는) {current}에서는 이름으로 처리되지만 \
             {future}부터는 예약어가 되었으므로 쓰지 않는 것이 좋습니다",
    _    => "The use of {read} is discouraged as it was a name in {current} \
             but it became a keyword since {future}",
}

define_msg! { pub AssumeNameStatic:
    "ko" => "`--# assume static`은 클래스 프로토타입의 필드를 설정하는 데만 쓸 수 있습니다",
    _    => "`--# assume static` can only be used to set fields in class prototypes",
}

define_msg! { pub AssumeFieldGlobal:
    "ko" => "필드는 제자리에서 `--# assume`되므로 `global`을 쓸 필요가 없습니다",
    _    => "`global` is redundant here because a field gets `--# assume`d in place",
}

define_msg! { pub AssumeShadowedGlobal<'a> { name: &'a Name }:
    "ko" => "`--# assume` 명령이 전역 변수 {name}을(를) 설정하려 했으나, \
             같은 이름의 지역 변수가 전역 변수를 감추었습니다",
    _    => "`--# assume` directive tried to set a global variable {name}, \
             but it was shadowed by a local variable of the same name",
}

define_msg! { pub AssumeGlobalInLocalScope:
    "ko" => "`--# assume global`은 최상위 블록에서만 쓸 수 있습니다",
    _    => "`--# assume global` should be in the top-level scope",
}

define_msg! { pub AssumeFieldGlobalInLocalScope<'a> { name: &'a Name }:
    "ko" => "전역 변수 {name}의 필드에 대한 `--# assume`은 최상위 블록에서만 쓸 수 있습니다",
    _    => "`--# assume` for fields in a global variable {name} should be in the top-level scope",
}

define_msg! { pub TypeGlobalInLocalScope:
    "ko" => "`--# type global`은 최상위 블록에서만 쓸 수 있습니다",
    _    => "`--# type global` should be in the top-level scope",
}

define_msg! { pub TypeExportInLocalScope:
    "ko" => "타입을 바깥으로 내보내는 `--# type`은 최상위 블록에서만 쓸 수 있습니다",
    _    => "`--# type` with an exported type should be in the top-level scope",
}

define_msg! { pub AssumeMethodToNonInstanceField:
    "ko" => "`method(...) --> ...` 타입은 정적이 아닌 필드를 `--# assume` 할 때만 쓸 수 있습니다",
    _    => "`method(...) --> ...` type is only available when using `--# assume` \
             to a non-static field",
}

