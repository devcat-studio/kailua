//! Type system for Kailua.
//!
//! The Kailua type system is in principle a Hindler-Milner type system
//! with significant extension for subtyping, gradual typing and row polymorphism.
//! The "in principle" qualification is there because there are (unfortunately)
//! several known discrepancies, but they are at least considered (low-priority) bugs.
//! Naturally, the full type inference is not supported (which is even more undesirable
//! when we want records, arrays and maps *not* freely convertible to each other anyway).
//!
//! Two defining aspects of the Kailua type system are
//! that **`nil` is not checked** and
//! that **(inextensible) records may still contain unknown labels**.
//! This is a result of making the type system usable without breaking Lua compatibility;
//! it is quite comparable to the type system of TypeScript 1.x.
//! The type system *does* have types for not allowing `nil` or accesses to unknown labels,
//! but they exist mostly to avoid mistakes and not to make the type system sound.
//!
//! Generics are planned but not yet implemented;
//! this is probably the biggest missing type system feature.
//! Many recursive types are not properly checked too,
//! which requires proper occurs check that the type system still doesn't implement (ugh).

#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate log;
extern crate vec_map;
extern crate take_mut;
extern crate atomic;
extern crate parking_lot;

mod l10nutils; // used by diag and message but should not be public
pub mod diag;
mod message;
pub mod ty;
pub mod env;

