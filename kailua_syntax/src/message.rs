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

define_msg! { pub UnclosedOpeningLongBracket:
    "ko" => "긴 문자열을 여는 `[`가 제대로 닫히지 않았습니다",
    _    => "Opening long bracket should end with `[`",
}

define_msg! { pub PrematureEofInLongString:
    "ko" => "긴 문자열을 읽던 중 파일이 끝났습니다",
    _    => "Premature end of file in a long string",
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

define_msg! { pub NoVar<'a> { read: &'a Tok }:
    "ko" => "변수나 인덱스 수식이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a left-hand-side expression, got {read}",
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

define_msg! { pub NoTableSep<'a> { read: &'a Tok }:
    "ko" => "`,`, `;`이나 `}}`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `,`, `;` or `}}`, got {read}",
}

define_msg! { pub NoForInSep<'a> { read: &'a Tok }:
    "ko" => "`=`, `,`, `in`이나 `--:`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected `=`, `,`, `in` or `--:` after `for NAME`, got {read}",
}

define_msg! { pub BadFuncArg<'a> { read: &'a Tok }:
    "ko" => "인자 이름, `)` 또는 `...`가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected an argument name, `)` or `...`, got {read}",
}

define_msg! { pub NoNameAfterExpDot<'a> { read: &'a Tok }:
    "ko" => "`<수식> .` 뒤에 이름이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name after `<expression> .`, got {read}",
}

define_msg! { pub NoArgsAfterExpColonName<'a> { read: &'a Tok }:
    "ko" => "`<수식> : <이름>` 뒤에 인자가 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected argument(s) after `<expression> : <name>`, got {read}",
}

define_msg! { pub NoFuncOrNameAfterLocal<'a> { read: &'a Tok }:
    "ko" => "`local` 뒤에 이름이나 `function`이 나와야 하는데 {read}이(가) 나왔습니다",
    _    => "Expected a name or `function` after `local`, got {read}",
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
    "ko" => "함수 타입이 아닌 곳에서는 `...` 앞에 항상 타입이 존재해야 합니다",
    _    => "`...` should be preceded with a kind in the ordinary kinds",
}

define_msg! { pub DuplicateFieldNameInRec<'a> { name: &'a Name }:
    "ko" => "타입에서 레코드 이름 {name}이 중복됩니다",
    _    => "Duplicate record field {name} in the type specification",
}

define_msg! { pub FirstFieldNameInRec:
    "ko" => "여기서 처음 나왔습니다",
    _    => "The first duplicate appeared here",
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

define_msg! { pub MissingSelfInFuncSpec:
    "ko" => "메소드의 함수 타입에서 첫 인자가 `self`가 아닙니다",
    _    => "The first argument in the function specification for a method is not `self`",
}

