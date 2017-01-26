use std::str;
use std::fmt;
use std::collections::BTreeMap as Object;
use serde::{de, Serialize, Serializer, Deserialize, Deserializer};
use serde_json::{self, Value};

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
                    #[serde(skip_serializing_if = "::protocol::is_default")]
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

// also acts as NotificationMessage when id is None
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize"))]
pub struct RequestMessage<T = Value> {
    #[serde(rename = "jsonrpc")] pub version: Version,
    #[serde(default, skip_serializing_if = "is_default")] pub id: Option<Id>,
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Request {
    // proper requests, use Id for reply
    Initialize(Id, InitializeParams),
    Shutdown(Id),
    ShowMessageRequest(Id, ShowMessageRequestParams),
    #[cfg(protocol_v3)] Registration(Id, RegistrationParams),
    #[cfg(protocol_v3)] Unregistration(Id, UnregistrationParams),
    WorkspaceSymbol(Id, WorkspaceSymbolParams),
    #[cfg(protocol_v3)] ExecuteCommand(Id, ExecuteCommandParams),
    #[cfg(protocol_v3)] ApplyWorkspaceEdit(Id, ApplyWorkspaceEditParams),
    #[cfg(protocol_v3)] WillSaveWaitUntilTextDocument(Id, WillSaveTextDocumentParams),
    Completion(Id, TextDocumentPositionParams),
    CompletionItemResolve(Id, CompletionItem),
    Hover(Id, TextDocumentPositionParams),
    SignatureHelp(Id, TextDocumentPositionParams),
    FindReferences(Id, ReferenceParams),
    DocumentHighlight(Id, TextDocumentPositionParams),
    DocumentSymbol(Id, DocumentSymbolParams),
    DocumentFormatting(Id, DocumentFormattingParams),
    DocumentRangeFormatting(Id, DocumentRangeFormattingParams),
    DocumentOnTypeFormatting(Id, DocumentOnTypeFormattingParams),
    GotoDefinition(Id, TextDocumentPositionParams),
    CodeAction(Id, CodeActionParams),
    CodeLens(Id, CodeLensParams),
    CodeLensResolve(Id, CodeLens),
    DocumentLink(Id, DocumentLinkParams),
    DocumentLinkResolve(Id, DocumentLink),
    Rename(Id, RenameParams),

    // notifications
    CancelRequest(CancelParams),
    #[cfg(protocol_v3)] Initialized,
    Exit,
    ShowMessage(ShowMessageParams),
    LogMessage(LogMessageParams),
    TelemetryEvent(Value),
    DidChangeConfiguration(DidChangeConfigurationParams),
    DidChangeWatchedFiles(DidChangeWatchedFilesParams),
    PublishDiagnostics(PublishDiagnosticsParams),
    DidOpenTextDocument(DidOpenTextDocumentParams),
    DidChangeTextDocument(DidChangeTextDocumentParams),
    #[cfg(protocol_v3)] WillSaveTextDocument(WillSaveTextDocumentParams),
    DidSaveTextDocument(DidSaveTextDocumentParams),
    DidCloseTextDocument(DidCloseTextDocumentParams),
}

impl Request {
    pub fn id(&self) -> Option<&Id> {
        match *self {
            Request::Initialize(ref id, ..) |
            Request::Shutdown(ref id) |
            Request::ShowMessageRequest(ref id, ..) |
            Request::WorkspaceSymbol(ref id, ..) |
            Request::Completion(ref id, ..) |
            Request::CompletionItemResolve(ref id, ..) |
            Request::Hover(ref id, ..) |
            Request::SignatureHelp(ref id, ..) |
            Request::FindReferences(ref id, ..) |
            Request::DocumentHighlight(ref id, ..) |
            Request::DocumentSymbol(ref id, ..) |
            Request::DocumentFormatting(ref id, ..) |
            Request::DocumentRangeFormatting(ref id, ..) |
            Request::DocumentOnTypeFormatting(ref id, ..) |
            Request::GotoDefinition(ref id, ..) |
            Request::CodeAction(ref id, ..) |
            Request::CodeLens(ref id, ..) |
            Request::CodeLensResolve(ref id, ..) |
            Request::DocumentLink(ref id, ..) |
            Request::DocumentLinkResolve(ref id, ..) |
            Request::Rename(ref id, ..) => Some(id),

            #[cfg(protocol_v3)]
            Request::Registration(ref id, ..) |
            Request::Unregistration(ref id, ..) |
            Request::ExecuteCommand(ref id, ..) |
            Request::ApplyWorkspaceEdit(ref id, ..) |
            Request::WillSaveWaitUntilTextDocument(ref id, ..) => Some(id),

            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RequestError {
    MethodNotFound(String),
    InvalidRequest(String),
    InvalidParams(String),
}

impl Request {
    pub fn from_message(msg: RequestMessage) -> Result<Request, (Option<Id>, RequestError)> {
        fn request_simple<F>(msg: RequestMessage,
                             wrap: F) -> Result<Request, (Option<Id>, RequestError)>
            where F: Fn(Id) -> Request
        {
            if let Some(id) = msg.id {
                Ok(wrap(id))
            } else {
                Err((None, RequestError::InvalidRequest("missing ID".into())))
            }
        }

        fn notification_simple<F>(msg: RequestMessage,
                                  wrap: F) -> Result<Request, (Option<Id>, RequestError)>
            where F: Fn() -> Request
        {
            if msg.id.is_some() {
                Err((None, RequestError::InvalidRequest("unexpected ID".into())))
            } else {
                Ok(wrap())
            }
        }

        fn request<T, F>(msg: RequestMessage,
                         wrap: F) -> Result<Request, (Option<Id>, RequestError)>
            where T: Deserialize, F: Fn(Id, T) -> Request
        {
            if let Some(id) = msg.id {
                if let Some(params) = msg.params {
                    match serde_json::from_value(params) {
                        Ok(p) => Ok(wrap(id, p)),
                        Err(e) => Err((Some(id), RequestError::InvalidParams(e.to_string()))),
                    }
                } else {
                    Err((Some(id), RequestError::InvalidParams("no parameters given".into())))
                }
            } else {
                Err((None, RequestError::InvalidRequest("missing ID".into())))
            }
        }

        fn notification<T, F>(msg: RequestMessage,
                              wrap: F) -> Result<Request, (Option<Id>, RequestError)>
            where T: Deserialize, F: Fn(T) -> Request
        {
            if let Some(id) = msg.id {
                Err((Some(id), RequestError::InvalidRequest("unexpected ID".into())))
            } else {
                if let Some(params) = msg.params {
                    match serde_json::from_value(params) {
                        Ok(p) => Ok(wrap(p)),
                        Err(e) => Err((None, RequestError::InvalidParams(e.to_string()))),
                    }
                } else {
                    Err((None, RequestError::InvalidParams("no parameters given".into())))
                }
            }
        }

        match &msg.method[..] {
            "$/cancelRequest" => notification(msg, Request::CancelRequest),
            "initialize" => request(msg, Request::Initialize),
            "shutdown" => request_simple(msg, Request::Shutdown),
            "exit" => notification_simple(msg, || Request::Exit),
            "window/showMessage" => notification(msg, Request::ShowMessage),
            "window/showMessageRequest" => request(msg, Request::ShowMessageRequest),
            "window/logMessage" => notification(msg, Request::LogMessage),
            "telemetry/event" => notification(msg, Request::TelemetryEvent),
            "workspace/didChangeConfiguration" =>
                notification(msg, Request::DidChangeConfiguration),
            "workspace/didChangeWatchedFiles" => notification(msg, Request::DidChangeWatchedFiles),
            "workspace/symbol" => request(msg, Request::WorkspaceSymbol),
            "textDocument/publishDiagnostics" => notification(msg, Request::PublishDiagnostics),
            "textDocument/didOpen" => notification(msg, Request::DidOpenTextDocument),
            "textDocument/didChange" => notification(msg, Request::DidChangeTextDocument),
            "textDocument/didSave" => notification(msg, Request::DidSaveTextDocument),
            "textDocument/didClose" => notification(msg, Request::DidCloseTextDocument),
            "textDocument/completion" => request(msg, Request::Completion),
            "completionItem/resolve" => request(msg, Request::CompletionItemResolve),
            "textDocument/hover" => request(msg, Request::Hover),
            "textDocument/signatureHelp" => request(msg, Request::SignatureHelp),
            "textDocument/references" => request(msg, Request::FindReferences),
            "textDocument/documentHighlight" => request(msg, Request::DocumentHighlight),
            "textDocument/documentSymbol" => request(msg, Request::DocumentSymbol),
            "textDocument/formatting" => request(msg, Request::DocumentFormatting),
            "textDocument/rangeFormatting" => request(msg, Request::DocumentRangeFormatting),
            "textDocument/onTypeFormatting" => request(msg, Request::DocumentOnTypeFormatting),
            "textDocument/definition" => request(msg, Request::GotoDefinition),
            "textDocument/codeAction" => request(msg, Request::CodeAction),
            "textDocument/codeLens" => request(msg, Request::CodeLens),
            "codeLens/resolve" => request(msg, Request::CodeLensResolve),
            "textDocument/documentLink" => request(msg, Request::DocumentLink),
            "documentLink/resolve" => request(msg, Request::DocumentLinkResolve),
            "textDocument/rename" => request(msg, Request::Rename),

            #[cfg(protocol_v3)]
            "initialized" => notification_simple(msg, || Request::Initialized),
            #[cfg(protocol_v3)]
            "client/registerCapability" => request(msg, Request::Registration),
            #[cfg(protocol_v3)]
            "client/unregisterCapability" => request(msg, Request::Unregistration),
            #[cfg(protocol_v3)]
            "workspace/executeCommand" => request(msg, Request::ExecuteCommand),
            #[cfg(protocol_v3)]
            "workspace/applyEdit" => request(msg, Request::ApplyWorkspaceEdit),
            #[cfg(protocol_v3)]
            "textDocument/willSave" => notification(msg, Request::WillSaveTextDocument),
            #[cfg(protocol_v3)]
            "textDocument/willSaveWaitUntil" =>
                request(msg, Request::WillSaveWaitUntilTextDocument),

            _ => Err((msg.id, RequestError::MethodNotFound(msg.method))),
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

    #[cfg(protocol_v3)]
    pub struct DocumentFilter {
        pub language?: Option<String>,
        pub scheme?: Option<String>,
        pub pattern?: Option<String>,
    }
}

#[cfg(protocol_v3)]
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
        #[cfg(protocol_v3)] pub rootUri: Option<String>,
        pub initializationOptions?: Option<Value>,
        pub capabilities: ClientCapabilities,
        #[cfg(protocol_v3)] pub trace?: Trace,
    }

    pub struct ClientCapabilities {
        #[cfg(protocol_v3)] pub workspace?: Option<WorkspaceClientCapabilites>,
        #[cfg(protocol_v3)] pub textDocument?: Option<TextDocumentClientCapabilities>,
        #[cfg(protocol_v3)] pub experimental?: Option<Value>,
    }
}

#[cfg(protocol_v3)]
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

#[cfg(protocol_v3)]
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Trace {
    #[serde(rename = "off")] Off,
    #[serde(rename = "messages")] Messages,
    #[serde(rename = "verbose")] Verbose,
}

#[cfg(protocol_v3)]
impl Default for Trace {
    fn default() -> Self { Trace::Off }
}

#[cfg(protocol_v3)]
impl IsDefault for Trace {
    fn is_default(&self) -> bool { *self == Trace::Off }
}

pub mod client_caps {
    #[cfg(protocol_v3)]
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
            pub rangeProperty?: bool,
            pub typedString?: bool,
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

    #[cfg(protocol_v3)]
    pub struct DocumentLinkOptions {
        pub resolveProvider?: bool,
    }

    #[cfg(protocol_v3)]
    pub struct ExecuteCommandOptions {
        pub commands: Vec<String>,
    }

    #[cfg(protocol_v3)]
    pub struct SaveOptions {
        pub includeText?: bool,
    }

    #[cfg(protocol_v3)]
    pub struct TextDocumentSyncOptions {
        pub openClose?: bool,
        pub change?: TextDocumentSyncKind,
        pub willSave?: bool,
        pub willSaveWaitUntil?: bool,
        pub save?: Option<SaveOptions>,
    }

    pub struct ServerCapabilities {
        #[cfg(protocol_v3)] pub textDocumentSync?: Option<TextDocumentSyncOptions>,
        #[cfg(not(protocol_v3))] pub textDocumentSync?: TextDocumentSyncKind,
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
        #[cfg(protocol_v3)] pub documentLinkProvider?: Option<DocumentLinkOptions>,
        #[cfg(protocol_v3)] pub executeCommandProvider?: Option<ExecuteCommandOptions>,
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

#[cfg(protocol_v3)]
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

    #[cfg(protocol_v3)]
    pub struct TextDocumentChangeRegistrationOptions {
        pub documentSelector: Option<DocumentSelector>,
        pub syncKind: TextDocumentSyncKind,
    }

    pub struct DidSaveTextDocumentParams {
        pub textDocument: TextDocumentIdentifier,
        #[cfg(protocol_v3)] pub text?: Option<String>,
    }

    #[cfg(protocol_v3)]
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

#[cfg(protocol_v3)]
interface! {
    pub struct WillSaveTextDocumentParams {
        pub textDocument: TextDocumentIdentifier,
        pub reason: TextDocumentSaveReason,
    }
}

#[cfg(protocol_v3)]
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

    #[cfg(protocol_v3)]
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

    #[cfg(protocol_v3)]
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

    #[cfg(protocol_v3)]
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

    #[cfg(protocol_v3)]
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

#[cfg(protocol_v3)]
interface! {
    pub struct ExecuteCommandParams {
        pub command: String,
        pub arguments?: Vec<Value>,
    }
}

// workspace edits

#[cfg(protocol_v3)]
interface! {
    pub struct ApplyWorkspaceEditParams {
        pub edit: WorkspaceEdit,
    }

    pub struct ApplyWorkspaceEditResponse {
        pub applied: bool,
    }
}

