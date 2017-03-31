extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;

use std::str;
use std::str::FromStr;
use std::fmt;
use std::error::Error;
use std::collections::BTreeMap as Object;
use serde::{de, Serialize, Serializer, Deserialize, Deserializer};
use serde_json::Value;

// utilities for serde_derive

trait IsDefault: Default {
    fn is_default(&self) -> bool;
}

impl<T> IsDefault for Option<T> {
    fn is_default(&self) -> bool { self.is_none() }
}

impl IsDefault for bool {
    fn is_default(&self) -> bool { !*self }
}

impl<T> IsDefault for Vec<T> {
    fn is_default(&self) -> bool { self.is_empty() }
}

impl IsDefault for () {
    fn is_default(&self) -> bool { true }
}

fn is_default<T: IsDefault>(v: &T) -> bool { v.is_default() }

macro_rules! interface {
    (@fields $acc:tt ($($attrs:tt)*) $default:tt #[$fattr:meta] $($t:tt)*) => (
        interface!(@fields $acc ($($attrs)* #[$fattr]) $default $($t)*);
    );

    (@fields ($($acc:tt)*) ($($attrs:tt)*) $_default:tt
             pub $fname:ident: $ftype:ty, $($t:tt)*) => (
        interface!(@fields
                   ($($acc)*
                    $($attrs)* pub $fname: $ftype,)
                   () - $($t)*);
    );

    (@fields ($($acc:tt)*) ($($attrs:tt)*) $default:tt
             pub $fname:ident?: $ftype:ty, $($t:tt)*) => (
        interface!(@fields
                   ($($acc)*
                    #[serde(skip_serializing_if = "::is_default")]
                    $($attrs)* pub $fname: $ftype,)
                   () $default $($t)*);
    );

    (@fields $acc:tt $attrs:tt $default:tt pub $fname:ident: $ftype:ty) => (
        interface!(@fields $acc $attrs $default pub $fname: $ftype,);
    );

    (@fields $acc:tt $attrs:tt $default:tt pub $fname:ident?: $ftype:ty) => (
        interface!(@fields $acc $attrs $default pub $fname?: $ftype,);
    );

    (@fields ($sname:ident ($($sattrs:tt)*) $($acc:tt)*) () -) => (
        #[allow(non_snake_case)]
        #[derive(Serialize, Deserialize, Debug, Clone)] $($sattrs)*
        pub struct $sname { $($acc)* }
    );

    (@fields ($sname:ident ($($sattrs:tt)*) $($acc:tt)*) () +) => (
        #[allow(non_snake_case)]
        #[derive(Serialize, Deserialize, Debug, Clone, Default)] $($sattrs)*
        pub struct $sname { $($acc)* }
    );

    ($($(#[$sattr:meta])* pub struct $sname:ident { $($fields:tt)* })*) => (
        $(interface!(@fields ($sname ($(#[$sattr])*)) () + $($fields)*);)*
    );
}

macro_rules! enum_number {
    (@replace $_ignored:tt $($t:tt)*) => ($($t)*);

    () => ();

    (
        $(#[$attrs:meta])* pub enum $name:ident {
            $($variant:ident = $value:expr),* $(,)*
        }
        $($t:tt)*
    ) => (
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        $(#[$attrs])*
        pub enum $name {
            $($variant = $value,)*
        }

        impl Serialize for $name {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                s.serialize_u64(*self as u64)
            }
        }

        impl Deserialize for $name {
            fn deserialize<D: Deserializer>(d: D) -> Result<Self, D::Error> {
                struct EnumVisitor;
                impl de::Visitor for EnumVisitor {
                    type Value = $name;
                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                        write!(f, "{}", stringify!($name))
                    }
                    fn visit_u64<E: de::Error>(self, v: u64) -> Result<$name, E> {
                        match v {
                            $($value => Ok($name::$variant),)*
                            _ => Err(E::invalid_value(de::Unexpected::Unsigned(v), &self)),
                        }
                    }
                }
                d.deserialize_u64(EnumVisitor)
            }
        }

        enum_number! { $($t)* }
    );

    (
        $(#[$attrs:meta])* pub enum $name:ident {
            default $defvariant:ident = $defvalue:expr
            $(, $variant:ident = $value:expr)* $(,)*
        }
        $($t:tt)*
    ) => (
        enum_number! {
            $(#[$attrs])* pub enum $name {
                $defvariant = $defvalue,
                $($variant = $value,)*
            }
        }

        impl Default for $name {
            fn default() -> Self { $name::$defvariant }
        }

        impl IsDefault for $name {
            fn is_default(&self) -> bool { *self == $name::$defvariant }
        }

        enum_number! { $($t)* }
    );
}

// JSON-RPC layer

#[derive(Clone)]
pub struct Version;

impl fmt::Debug for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Version 2.0>")
    }
}

impl Serialize for Version {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str("2.0")
    }
}

impl Deserialize for Version {
    fn deserialize<D: Deserializer>(d: D) -> Result<Version, D::Error> {
        struct VersionVisitor;
        impl de::Visitor for VersionVisitor {
            type Value = Version;
            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "a string `2.0`")
            }
            fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
                if v == "2.0" {
                    Ok(Version)
                } else {
                    Err(E::custom("invalid JSON-RPC version"))
                }
            }
        }
        d.deserialize_str(VersionVisitor)
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Id { Number(i64), String(String) }

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Id::Number(id) => id.fmt(f),
            Id::String(ref id) => id.fmt(f),
        }
    }
}

impl Serialize for Id {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match *self {
            Id::Number(id) => s.serialize_i64(id),
            Id::String(ref id) => s.serialize_str(id),
        }
    }
}

impl Deserialize for Id {
    fn deserialize<D: Deserializer>(d: D) -> Result<Id, D::Error> {
        struct IdVisitor;
        impl de::Visitor for IdVisitor {
            type Value = Id;
            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "a string or an integer")
            }
            fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
                Ok(Id::Number(v))
            }
            fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
                if (v >> 63) == 0 {
                    Ok(Id::Number(v as i64))
                } else {
                    Err(E::custom("message ID out of range"))
                }
            }
            fn visit_string<E: de::Error>(self, v: String) -> Result<Self::Value, E> {
                Ok(Id::String(v))
            }
        }
        d.deserialize(IdVisitor)
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    Request(RequestMessage),
    Response(ResponseMessage),
    Notification(NotificationMessage),
}

impl Serialize for Message {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match *self {
            Message::Request(ref msg) => msg.serialize(s),
            Message::Response(ref msg) => msg.serialize(s),
            Message::Notification(ref msg) => msg.serialize(s),
        }
    }
}

impl Deserialize for Message {
    fn deserialize<D: Deserializer>(d: D) -> Result<Message, D::Error> {
        #[derive(Deserialize)]
        struct MessageInternal {
            #[serde(rename = "jsonrpc")] version: Version,
            #[serde(default)] id: Option<Option<Id>>,
            #[serde(default)] method: Option<String>,
            #[serde(default)] params: Option<Value>,
            #[serde(default)] result: Option<Value>,
            #[serde(default)] error: Option<ResponseError<Value>>,
        }

        let msg: MessageInternal = Deserialize::deserialize(d)?;

        if let Some(id) = msg.id {
            if let Some(method) = msg.method {
                // RequestMessage
                const FIELDS: &'static [&'static str] = &["jsonrpc", "id", "method", "params"];
                if let Some(id) = id {
                    if msg.result.is_some() {
                        Err(de::Error::unknown_field("result", FIELDS))
                    } else if msg.error.is_some() {
                        Err(de::Error::unknown_field("error", FIELDS))
                    } else {
                        Ok(Message::Request(RequestMessage {
                            version: msg.version, id: id, method: method, params: msg.params,
                        }))
                    }
                } else {
                    Err(de::Error::invalid_type(de::Unexpected::Unit, &"a string or an integer"))
                }
            } else {
                // ResponseMessage
                const FIELDS: &'static [&'static str] = &["jsonrpc", "id", "result", "error"];
                if msg.method.is_some() {
                    Err(de::Error::unknown_field("method", FIELDS))
                } else if msg.params.is_some() {
                    Err(de::Error::unknown_field("params", FIELDS))
                } else if msg.result.is_some() && msg.error.is_some() {
                    Err(de::Error::custom("both `result` and `error` fields are present \
                                           in the response message"))
                } else {
                    Ok(Message::Response(ResponseMessage {
                        version: msg.version, id: id, result: msg.result, error: msg.error,
                    }))
                }
            }
        } else {
            // NotificationMessage
            const FIELDS: &'static [&'static str] = &["jsonrpc", "method", "params"];
            if let Some(method) = msg.method {
                if msg.result.is_some() {
                    Err(de::Error::unknown_field("result", FIELDS))
                } else if msg.error.is_some() {
                    Err(de::Error::unknown_field("error", FIELDS))
                } else {
                    Ok(Message::Notification(NotificationMessage {
                        version: msg.version, method: method, params: msg.params,
                    }))
                }
            } else {
                Err(de::Error::missing_field("method"))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize"))]
pub struct RequestMessage<T = Value> {
    #[serde(rename = "jsonrpc")] pub version: Version,
    pub id: Id,
    pub method: String,
    #[serde(default, skip_serializing_if = "is_default")] pub params: Option<T>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize"))]
pub struct NotificationMessage<T = Value> {
    #[serde(rename = "jsonrpc")] pub version: Version,
    pub method: String,
    #[serde(default, skip_serializing_if = "is_default")] pub params: Option<T>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(bound(serialize = "R: Serialize, E: Serialize",
              deserialize = "R: Deserialize, E: Deserialize"))]
pub struct ResponseMessage<R = Value, E = Value> {
    #[serde(rename = "jsonrpc")] pub version: Version,
    pub id: Option<Id>,
    #[serde(default, skip_serializing_if = "is_default")] pub result: Option<R>,
    #[serde(default, skip_serializing_if = "is_default")] pub error: Option<ResponseError<E>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize"))]
pub struct ResponseError<T> {
    pub code: i64,
    pub message: String,
    #[serde(default, skip_serializing_if = "is_default")] pub data: Option<T>,
}

pub mod error_codes {
    pub const PARSE_ERROR: i64 = -32700;
    pub const INVALID_REQUEST: i64 = -32600;
    pub const METHOD_NOT_FOUND: i64 = -32601;
    pub const INVALID_PARAMS: i64 = -32602;
    pub const INTERNAL_ERROR: i64 = -32603;
    pub const SERVER_ERROR_START: i64 = -32099;
    pub const SERVER_ERROR_END: i64 = -32000;
    pub const SERVER_NOT_INITIALIZED: i64 = -32002;
    pub const UNKNOWN_ERROR_CODE: i64 = -32001;

    // language server protocol specific
    pub const UNKNOWN_PROTOCOL_VERSION: i64 = 1;
}

// the actual language server protocol continues

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodParseError(());

impl fmt::Display for MethodParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "No such method exists")
    }
}

impl Error for MethodParseError {
    fn description(&self) -> &str { "No such method exists" }
}

macro_rules! define_methods {
    ($($name:ident $str:tt $(#[$attr:meta])* = $val:expr,)*) => (
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Method {
            $($(#[$attr])* $name = $val,)*
        }

        impl Method {
            pub fn as_str(&self) -> &str {
                match *self {
                    $($(#[$attr])* Method::$name => $str,)*
                }
            }
        }

        impl FromStr for Method {
            type Err = MethodParseError;

            fn from_str(s: &str) -> Result<Method, MethodParseError> {
                match s {
                    $($(#[$attr])* $str => Ok(Method::$name),)*
                    _ => Err(MethodParseError(())),
                }
            }
        }
    )
}

define_methods! {
    CancelRequest "$/cancelRequest" = 0x200,
    Initialize "initialize" = 0x100,
    Initialized "initialized" = 0x300,
    Shutdown "shutdown" = 0x101,
    Exit "exit" = 0x102,
    ShowMessage "window/showMessage" = 0x103,
    ShowMessageRequest "window/showMessageRequest" = 0x201,
    LogMessage "window/logMessage" = 0x104,
    TelemetryEvent "telemetry/event" = 0x202,
    RegisterCapability "client/registerCapability" = 0x301,
    UnregisterCapability "client/unregisterCapability" = 0x302,
    DidChangeConfiguration "workspace/didChangeConfiguration" = 0x105,
    DidChangeWatchedFiles "workspace/didChangeWatchedFiles" = 0x106,
    WorkspaceSymbol "workspace/symbol" = 0x107,
    PublishDiagnostics "textDocument/publishDiagnostics" = 0x108,
    DidOpen "textDocument/didOpen" = 0x109,
    DidChange "textDocument/didChange" = 0x10a,
    WillSave "textDocument/willSave" = 0x305,
    WillSaveWaitUntil "textDocument/willSaveWaitUntil" = 0x306,
    DidSave "textDocument/didSave" = 0x203,
    DidClose "textDocument/didClose" = 0x10b,
    Completion "textDocument/completion" = 0x10c,
    CompletionItemResolve "completionItem/resolve" = 0x10d,
    Hover "textDocument/hover" = 0x10e,
    SignatureHelp "textDocument/signatureHelp" = 0x10f,
    References "textDocument/references" = 0x110,
    DocumentHighlight "textDocument/documentHighlight" = 0x111,
    DocumentSymbol "textDocument/documentSymbol" = 0x112,
    Formatting "textDocument/formatting" = 0x113,
    RangeFormatting "textDocument/rangeFormatting" = 0x114,
    OnTypeFormatting "textDocument/onTypeFormatting" = 0x115,
    Definition "textDocument/definition" = 0x116,
    CodeAction "textDocument/codeAction" = 0x117,
    CodeLens "textDocument/codeLens" = 0x118,
    CodeLensResolve "codeLens/resolve" = 0x119,
    DocumentLink "textDocument/documentLink" = 0x204,
    DocumentLinkResolve "documentLink/resolve" = 0x205,
    Rename "textDocument/rename" = 0x11a,
    ExecuteCommand "workspace/executeCommand" = 0x303,
    ApplyEdit "workspace/applyEdit" = 0x304,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageError {
    MethodNotFound(String),
    InvalidParams(String),
}

#[derive(Debug, Clone)]
pub enum Request {
    Initialize(InitializeParams),
    Shutdown,
    ShowMessageRequest(ShowMessageRequestParams),
    Registration(RegistrationParams),
    Unregistration(UnregistrationParams),
    WorkspaceSymbol(WorkspaceSymbolParams),
    ExecuteCommand(ExecuteCommandParams),
    ApplyWorkspaceEdit(ApplyWorkspaceEditParams),
    WillSaveWaitUntilTextDocument(WillSaveTextDocumentParams),
    Completion(TextDocumentPositionParams),
    CompletionItemResolve(CompletionItem),
    Hover(TextDocumentPositionParams),
    SignatureHelp(TextDocumentPositionParams),
    FindReferences(ReferenceParams),
    DocumentHighlight(TextDocumentPositionParams),
    DocumentSymbol(DocumentSymbolParams),
    DocumentFormatting(DocumentFormattingParams),
    DocumentRangeFormatting(DocumentRangeFormattingParams),
    DocumentOnTypeFormatting(DocumentOnTypeFormattingParams),
    GotoDefinition(TextDocumentPositionParams),
    CodeAction(CodeActionParams),
    CodeLens(CodeLensParams),
    CodeLensResolve(CodeLens),
    DocumentLink(DocumentLinkParams),
    DocumentLinkResolve(DocumentLink),
    Rename(RenameParams),
}

impl Request {
    pub fn from_message(msg: RequestMessage) -> (Id, Result<Request, MessageError>) {
        fn parse<T, F>(msg: RequestMessage, wrap: F) -> (Id, Result<Request, MessageError>)
            where T: Deserialize, F: Fn(T) -> Request
        {
            if let Some(params) = msg.params {
                match serde_json::from_value(params) {
                    Ok(p) => (msg.id, Ok(wrap(p))),
                    Err(e) => (msg.id, Err(MessageError::InvalidParams(e.to_string()))),
                }
            } else {
                (msg.id, Err(MessageError::InvalidParams("no parameters given".into())))
            }
        }

        use self::Method as M;
        use self::Request as R;

        match msg.method.parse::<M>() {
            Ok(M::Initialize) => parse(msg, R::Initialize),
            Ok(M::Shutdown) => (msg.id, Ok(R::Shutdown)),
            Ok(M::ShowMessageRequest) => parse(msg, R::ShowMessageRequest),
            Ok(M::RegisterCapability) => parse(msg, R::Registration),
            Ok(M::UnregisterCapability) => parse(msg, R::Unregistration),
            Ok(M::WorkspaceSymbol) => parse(msg, R::WorkspaceSymbol),
            Ok(M::ExecuteCommand) => parse(msg, R::ExecuteCommand),
            Ok(M::ApplyEdit) => parse(msg, R::ApplyWorkspaceEdit),
            Ok(M::WillSaveWaitUntil) => parse(msg, R::WillSaveWaitUntilTextDocument),
            Ok(M::Completion) => parse(msg, R::Completion),
            Ok(M::CompletionItemResolve) => parse(msg, R::CompletionItemResolve),
            Ok(M::Hover) => parse(msg, R::Hover),
            Ok(M::SignatureHelp) => parse(msg, R::SignatureHelp),
            Ok(M::References) => parse(msg, R::FindReferences),
            Ok(M::DocumentHighlight) => parse(msg, R::DocumentHighlight),
            Ok(M::DocumentSymbol) => parse(msg, R::DocumentSymbol),
            Ok(M::Formatting) => parse(msg, R::DocumentFormatting),
            Ok(M::RangeFormatting) => parse(msg, R::DocumentRangeFormatting),
            Ok(M::OnTypeFormatting) => parse(msg, R::DocumentOnTypeFormatting),
            Ok(M::Definition) => parse(msg, R::GotoDefinition),
            Ok(M::CodeAction) => parse(msg, R::CodeAction),
            Ok(M::CodeLens) => parse(msg, R::CodeLens),
            Ok(M::CodeLensResolve) => parse(msg, R::CodeLensResolve),
            Ok(M::DocumentLink) => parse(msg, R::DocumentLink),
            Ok(M::DocumentLinkResolve) => parse(msg, R::DocumentLinkResolve),
            Ok(M::Rename) => parse(msg, R::Rename),

            _ => (msg.id, Err(MessageError::MethodNotFound(msg.method))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Notification {
    CancelRequest(CancelParams),
    Initialized,
    Exit,
    ShowMessage(ShowMessageParams),
    LogMessage(LogMessageParams),
    TelemetryEvent(Value),
    DidChangeConfiguration(DidChangeConfigurationParams),
    DidChangeWatchedFiles(DidChangeWatchedFilesParams),
    PublishDiagnostics(PublishDiagnosticsParams),
    DidOpenTextDocument(DidOpenTextDocumentParams),
    DidChangeTextDocument(DidChangeTextDocumentParams),
    WillSaveTextDocument(WillSaveTextDocumentParams),
    DidSaveTextDocument(DidSaveTextDocumentParams),
    DidCloseTextDocument(DidCloseTextDocumentParams),
}

impl Notification {
    pub fn from_message(msg: NotificationMessage) -> Result<Notification, MessageError> {
        fn parse<T, F>(msg: NotificationMessage, wrap: F) -> Result<Notification, MessageError>
            where T: Deserialize, F: Fn(T) -> Notification
        {
            if let Some(params) = msg.params {
                match serde_json::from_value(params) {
                    Ok(p) => Ok(wrap(p)),
                    Err(e) => Err(MessageError::InvalidParams(e.to_string())),
                }
            } else {
                Err(MessageError::InvalidParams("no parameters given".into()))
            }
        }

        use self::Method as M;
        use self::Notification as N;

        match msg.method.parse::<M>() {
            Ok(M::CancelRequest) => parse(msg, N::CancelRequest),
            Ok(M::Initialized) => Ok(N::Initialized),
            Ok(M::Exit) => Ok(N::Exit),
            Ok(M::ShowMessage) => parse(msg, N::ShowMessage),
            Ok(M::LogMessage) => parse(msg, N::LogMessage),
            Ok(M::TelemetryEvent) => parse(msg, N::TelemetryEvent),
            Ok(M::DidChangeConfiguration) => parse(msg, N::DidChangeConfiguration),
            Ok(M::DidChangeWatchedFiles) => parse(msg, N::DidChangeWatchedFiles),
            Ok(M::PublishDiagnostics) => parse(msg, N::PublishDiagnostics),
            Ok(M::DidOpen) => parse(msg, N::DidOpenTextDocument),
            Ok(M::DidChange) => parse(msg, N::DidChangeTextDocument),
            Ok(M::WillSave) => parse(msg, N::WillSaveTextDocument),
            Ok(M::DidSave) => parse(msg, N::DidSaveTextDocument),
            Ok(M::DidClose) => parse(msg, N::DidCloseTextDocument),

            _ => Err(MessageError::MethodNotFound(msg.method)),
        }
    }
}

// common types

interface! {
    pub struct Position {
        pub line: u64,
        pub character: u64,
    }

    pub struct Range {
        pub start: Position,
        pub end: Position,
    }

    pub struct Location {
        pub uri: String,
        pub range: Range,
    }

    pub struct Diagnostic {
        pub range: Range,
        pub severity?: Option<DiagnosticSeverity>,
        pub code?: Option<Value>, // TODO can be a number or a string
        pub source?: Option<String>,
        pub message: String,
    }

    pub struct Command {
        pub title: String,
        pub command: String,
        pub arguments?: Vec<String>,
    }

    pub struct TextEdit {
        pub range: Range,
        pub newText: String,
    }

    pub struct WorkspaceEdit {
        pub changes: Object<String, Vec<TextEdit>>,
    }

    pub struct TextDocumentIdentifier {
        pub uri: String,
    }

    pub struct TextDocumentItem {
        pub uri: String,
        pub languageId: String,
        pub version: u64,
        pub text: String,
    }

    pub struct VersionedTextDocumentIdentifier {
        pub uri: String,
        pub version: u64,
    }

    pub struct TextDocumentPositionParams {
        pub textDocument: TextDocumentIdentifier,
        pub position: Position,
    }

    pub struct DocumentFilter {
        pub language?: Option<String>,
        pub scheme?: Option<String>,
        pub pattern?: Option<String>,
    }
}

pub type DocumentSelector = Vec<DocumentFilter>;

enum_number! {
    pub enum DiagnosticSeverity {
        Error = 1,
        Warning = 2,
        Information = 3,
        Hint = 4,
    }
}

// cancel

interface! {
    pub struct CancelParams {
        pub id: Id,
    }
}

// initialize

interface! {
    pub struct InitializeParams {
        pub processId: Option<i64>,
        pub rootPath: Option<String>,
        pub rootUri: Option<String>,
        pub initializationOptions?: Option<Value>,
        pub capabilities: ClientCapabilities,
        pub trace?: Trace,
    }

    pub struct ClientCapabilities {
        pub workspace?: Option<WorkspaceClientCapabilites>,
        pub textDocument?: Option<TextDocumentClientCapabilities>,
        pub experimental?: Option<Value>,
    }
}

interface! {
    pub struct WorkspaceClientCapabilites {
        pub applyEdit?: bool,
        pub didChangeConfiguration?: Option<client_caps::DidChangeConfiguration>,
        pub didChangeWatchedFiles?: Option<client_caps::DidChangeWatchedFiles>,
        pub symbol?: Option<client_caps::Symbol>,
        pub executeCommand?: Option<client_caps::ExecuteCommand>,
    }

    pub struct TextDocumentClientCapabilities {
        pub synchronization?: Option<client_caps::Synchronization>,
        pub completion?: Option<client_caps::Completion>,
        pub hover?: Option<client_caps::Hover>,
        pub signatureHelp?: Option<client_caps::SignatureHelp>,
        pub references?: Option<client_caps::References>,
        pub documentHighlight?: Option<client_caps::DocumentHighlight>,
        pub documentSymbol?: Option<client_caps::DocumentSymbol>,
        pub formatting?: Option<client_caps::Formatting>,
        pub rangeFormatting?: Option<client_caps::RangeFormatting>,
        pub onTypeFormatting?: Option<client_caps::OnTypeFormatting>,
        pub definition?: Option<client_caps::Definition>,
        pub codeAction?: Option<client_caps::CodeAction>,
        pub codeLens?: Option<client_caps::CodeLens>,
        pub documentLink?: Option<client_caps::DocumentLink>,
        pub rename?: Option<client_caps::Rename>,
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Trace {
    #[serde(rename = "off")] Off,
    #[serde(rename = "messages")] Messages,
    #[serde(rename = "verbose")] Verbose,
}

impl Default for Trace {
    fn default() -> Self { Trace::Off }
}

impl IsDefault for Trace {
    fn is_default(&self) -> bool { *self == Trace::Off }
}

pub mod client_caps {
    interface! {
        pub struct DidChangeConfiguration {
            pub dynamicRegistration?: bool,
        }

        pub struct DidChangeWatchedFiles {
            pub dynamicRegistration?: bool,
        }

        pub struct Symbol {
            pub dynamicRegistration?: bool,
        }

        pub struct ExecuteCommand {
            pub dynamicRegistration?: bool,
        }

        pub struct Synchronization {
            pub dynamicRegistration?: bool,
            pub willSave?: bool,
            pub willSaveWaitUntil?: bool,
            pub didSave?: bool,
        }

        pub struct Completion {
            pub dynamicRegistration?: bool,
            pub completionItem?: Option<CompletionItem>,
        }

        pub struct CompletionItem {
            pub snippetSupport?: bool,
        }

        pub struct Hover {
            pub dynamicRegistration?: bool,
        }

        pub struct SignatureHelp {
            pub dynamicRegistration?: bool,
        }

        pub struct References {
            pub dynamicRegistration?: bool,
        }

        pub struct DocumentHighlight {
            pub dynamicRegistration?: bool,
        }

        pub struct DocumentSymbol {
            pub dynamicRegistration?: bool,
        }

        pub struct Formatting {
            pub dynamicRegistration?: bool,
        }

        pub struct RangeFormatting {
            pub dynamicRegistration?: bool,
        }

        pub struct OnTypeFormatting {
            pub dynamicRegistration?: bool,
        }

        pub struct Definition {
            pub dynamicRegistration?: bool,
        }

        pub struct CodeAction {
            pub dynamicRegistration?: bool,
        }

        pub struct CodeLens {
            pub dynamicRegistration?: bool,
        }

        pub struct DocumentLink {
            pub dynamicRegistration?: bool,
        }

        pub struct Rename {
            pub dynamicRegistration?: bool,
        }
    }
}

interface! {
    pub struct InitializeResult {
        pub capabilities: ServerCapabilities,
    }

    pub struct InitializeError {
        pub retry: bool,
    }

    pub struct CompletionOptions {
        pub resolveProvider?: bool,
        pub triggerCharacters?: Vec<String>,
    }

    pub struct SignatureHelpOptions {
        pub triggerCharacters?: Vec<String>,
    }

    pub struct CodeLensOptions {
        pub resolveProvider?: bool,
    }

    pub struct DocumentOnTypeFormattingOptions {
        pub firstTriggerCharacter: String,
        pub moreTriggerCharacter?: Vec<String>,
    }

    pub struct DocumentLinkOptions {
        pub resolveProvider?: bool,
    }

    pub struct ExecuteCommandOptions {
        pub commands: Vec<String>,
    }

    pub struct SaveOptions {
        pub includeText?: bool,
    }

    pub struct TextDocumentSyncOptions {
        pub openClose?: bool,
        pub change?: TextDocumentSyncKind,
        pub willSave?: bool,
        pub willSaveWaitUntil?: bool,
        pub save?: Option<SaveOptions>,
    }

    pub struct ServerCapabilities {
        // vscode-languageclient<=3.2.0 has a critical bug that prevents this code
        //pub textDocumentSync?: Option<TextDocumentSyncOptions>,
        pub textDocumentSync?: TextDocumentSyncKind,
        pub hoverProvider?: bool,
        pub completionProvider?: Option<CompletionOptions>,
        pub signatureHelpProvider?: Option<SignatureHelpOptions>,
        pub definitionProvider?: bool,
        pub referencesProvider?: bool,
        pub documentHighlightProvider?: bool,
        pub documentSymbolProvider?: bool,
        pub workspaceSymbolProvider?: bool,
        pub codeActionProvider?: bool,
        pub codeLensProvider?: Option<CodeLensOptions>,
        pub documentFormattingProvider?: bool,
        pub documentRangeFormattingProvider?: bool,
        pub documentOnTypeFormattingProvider?: Option<DocumentOnTypeFormattingOptions>,
        pub renameProvider?: bool,
        pub documentLinkProvider?: Option<DocumentLinkOptions>,
        pub executeCommandProvider?: Option<ExecuteCommandOptions>,
    }
}

enum_number! {
    pub enum TextDocumentSyncKind {
        default None = 0,
        Full = 1,
        Incremental = 2,
    }
}

// show and log message

interface! {
    pub struct ShowMessageParams {
        #[serde(rename = "type")] pub type_: MessageType,
        pub message: String,
    }

    pub struct ShowMessageRequestParams {
        #[serde(rename = "type")] pub type_: MessageType,
        pub message: String,
        pub actions?: Vec<MessageActionItem>,
    }

    pub struct MessageActionItem {
        pub title: String,
    }

    pub struct LogMessageParams {
        #[serde(rename = "type")] pub type_: MessageType,
        pub message: String,
    }
}

enum_number! {
    pub enum MessageType {
        Error = 1,
        Warning = 2,
        Info = 3,
        Log = 4,
    }
}

// dynamic client capabilities

interface! {
    pub struct Registration {
        pub id: String,
        pub method: String,
        pub registerOption?: Option<Value>,
    }

    pub struct RegistrationParams {
        pub registrations: Vec<Registration>,
    }

    pub struct TextDocumentRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
    }

    pub struct Unregistration {
        pub id: String,
        pub method: String,
    }

    pub struct UnregistrationParams {
        pub unregistrations: Vec<Unregistration>,
    }
}

// workspace notifications

interface! {
    pub struct DidChangeConfigurationParams {
        pub settings: Value,
    }

    pub struct DidChangeWatchedFilesParams {
        pub changes: Vec<FileEvent>,
    }

    pub struct FileEvent {
        pub uri: String,
        #[serde(rename = "type")] pub type_: FileChangeType,
    }
}

enum_number! {
    pub enum FileChangeType {
        Created = 1,
        Changed = 2,
        Deleted = 3,
    }
}

// text document notifications

interface! {
    pub struct DidOpenTextDocumentParams {
        pub textDocument: TextDocumentItem,
    }

    pub struct DidChangeTextDocumentParams {
        pub textDocument: VersionedTextDocumentIdentifier,
        pub contentChanges: Vec<TextDocumentContentChangeEvent>,
    }

    pub struct TextDocumentContentChangeEvent {
        pub range?: Option<Range>,
        pub rangeLength?: Option<u64>,
        pub text: String,
    }

    pub struct TextDocumentChangeRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub syncKind: TextDocumentSyncKind,
    }

    pub struct DidSaveTextDocumentParams {
        pub textDocument: TextDocumentIdentifier,
        pub text?: Option<String>,
    }

    pub struct TextDocumentSaveRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub includeText?: bool,
    }

    pub struct DidCloseTextDocumentParams {
        pub textDocument: TextDocumentIdentifier,
    }

    pub struct PublishDiagnosticsParams {
        pub uri: String,
        pub diagnostics: Vec<Diagnostic>,
    }
}

interface! {
    pub struct WillSaveTextDocumentParams {
        pub textDocument: TextDocumentIdentifier,
        pub reason: TextDocumentSaveReason,
    }
}

enum_number! {
    pub enum TextDocumentSaveReason {
        Manual = 1,
        AfterDelay = 2,
        FocusOut = 3,
    }
}

// completion

interface! {
    pub struct CompletionList {
        pub isIncomplete: bool,
        pub items: Vec<CompletionItem>,
    }

    pub struct CompletionItem {
        pub label: String,
        pub kind?: Option<CompletionItemKind>,
        pub detail?: Option<String>,
        pub documentation?: Option<String>,
        pub sortText?: Option<String>,
        pub filterText?: Option<String>,
        pub insertText?: Option<String>,
        pub textEdit?: Option<TextEdit>,
        pub additionalTextEdits?: Vec<TextEdit>,
        pub command?: Option<Command>,
        pub data?: Option<Value>,
    }

    pub struct CompletionRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub triggerCharacters?: Vec<String>,
        pub resolveProvider?: bool,
    }
}

enum_number! {
    pub enum CompletionItemKind {
        Text = 1,
        Method = 2,
        Function = 3,
        Constructor = 4,
        Field = 5,
        Variable = 6,
        Class = 7,
        Interface = 8,
        Module = 9,
        Property = 10,
        Unit = 11,
        Value = 12,
        Enum = 13,
        Keyword = 14,
        Snippet = 15,
        Color = 16,
        File = 17,
        Reference = 18,
    }
}

// hover

interface! {
    pub struct Hover {
        pub contents: Vec<MarkedString>, // TODO a single MarkedString is allowed
        pub range?: Option<Range>,
    }

    pub struct MarkedString { // TODO should allow a single string as well
        pub language: String,
        pub value: String,
    }
}

// signature help

interface! {
    pub struct SignatureHelp {
        pub signatures: Vec<SignatureInformation>,
        pub activeSignature?: Option<u32>,
        pub activeParameter?: Option<u32>,
    }

    pub struct SignatureInformation {
        pub label: String,
        pub documentation?: Option<String>,
        pub parameters?: Vec<ParameterInformation>,
    }

    pub struct ParameterInformation {
        pub label: String,
        pub documentation?: Option<String>,
    }

    pub struct SignatureHelpRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub triggerCharacters?: Vec<String>,
    }
}

// find references

interface! {
    pub struct ReferenceParams {
        pub textDocument: TextDocumentIdentifier,
        pub position: Position,
        pub context: ReferenceContext,
    }

    pub struct ReferenceContext {
        pub includeDeclaration: bool,
    }
}

// document highlights

interface! {
    pub struct DocumentHighlight {
        pub range: Range,
        pub kind?: DocumentHighlightKind,
    }
}

enum_number! {
    pub enum DocumentHighlightKind {
        default Text = 1,
        Read = 2,
        Write = 3,
    }
}

// document & workspace symbols

interface! {
    pub struct DocumentSymbolParams {
        pub textDocument: TextDocumentIdentifier,
    }

    pub struct SymbolInformation {
        pub name: String,
        pub kind: SymbolKind,
        pub location: Location,
        pub containerName?: Option<String>,
    }

    pub struct WorkspaceSymbolParams {
        pub query: String,
    }
}

enum_number! {
    pub enum SymbolKind {
        File = 1,
        Module = 2,
        Namespace = 3,
        Package = 4,
        Class = 5,
        Method = 6,
        Property = 7,
        Field = 8,
        Constructor = 9,
        Enum = 10,
        Interface = 11,
        Function = 12,
        Variable = 13,
        Constant = 14,
        String = 15,
        Number = 16,
        Boolean = 17,
        Array = 18,
    }
}

// code action

interface! {
    pub struct CodeActionParams {
        pub textDocument: TextDocumentIdentifier,
        pub range: Range,
        pub context: CodeActionContext,
    }

    pub struct CodeActionContext {
        pub diagnostics: Vec<Diagnostic>,
    }
}

// code lens

interface! {
    pub struct CodeLensParams {
        pub textDocument: TextDocumentIdentifier,
    }

    pub struct CodeLens {
        pub range: Range,
        pub command?: Option<Command>,
        pub data?: Option<Value>,
    }

    pub struct CodeLensRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub resolveProvider?: bool,
    }
}

// document link

interface! {
    pub struct DocumentLinkParams {
        pub textDocument: TextDocumentIdentifier,
    }

    pub struct DocumentLink {
        pub range: Range,
        pub target: String,
    }

    pub struct DocumentLinkRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub resolveProvider?: bool,
    }
}

// document formatting

interface! {
    pub struct DocumentFormattingParams {
        pub textDocument: TextDocumentIdentifier,
        pub options: FormattingOptions,
    }

    pub struct DocumentRangeFormattingParams {
        pub textDocument: TextDocumentIdentifier,
        pub range: Range,
        pub options: FormattingOptions,
    }

    pub struct DocumentOnTypeFormattingParams {
        pub textDocument: TextDocumentIdentifier,
        pub position: Position,
        pub ch: String,
        pub options: FormattingOptions,
    }
}

pub type FormattingOptions = Object<String, Value>; // should be more typed

// rename

interface! {
    pub struct RenameParams {
        pub textDocument: TextDocumentIdentifier,
        pub position: Position,
        pub newName: String,
    }
}

// execute command

interface! {
    pub struct ExecuteCommandParams {
        pub command: String,
        pub arguments?: Vec<Value>,
    }
}

// workspace edits

interface! {
    pub struct ApplyWorkspaceEditParams {
        pub edit: WorkspaceEdit,
    }

    pub struct ApplyWorkspaceEditResponse {
        pub applied: bool,
    }
}

