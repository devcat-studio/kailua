#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

impl Builtin {
    pub fn name(&self) -> &'static str {
        match *self {
            Builtin::Require => "require",
        }
    }
}

