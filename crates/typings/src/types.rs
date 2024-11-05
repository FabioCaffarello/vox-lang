use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone)]
pub enum Type {
    Float,
    Bool,
    Unresolved,
    Void,
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let type_name = match self {
            Self::Float => "float",
            Self::Bool => "bool",
            Self::Unresolved => "unresolved",
            Self::Void => "void",
            Self::Error => "?",
        };

        write!(f, "{}", type_name)
    }
}

impl Type {
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        // match (self, other) {
        //     (Type::Float, Type::Float) => true,
        //     (Type::Bool, Type::Bool) => true,
        //     (Type::Error, _) => true,
        //     (_, Type::Error) => true,
        //     _ => false,
        // }
        matches!(
            (self, other),
            (Type::Float, Type::Float)
                | (Type::Bool, Type::Bool)
                | (Type::Error, _)
                | (_, Type::Error)
        )
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(type_name: &str) -> Option<Type> {
        match type_name {
            "float" => Some(Type::Float),
            "bool" => Some(Type::Bool),
            "void" => Some(Type::Void),
            _ => None,
        }
    }
}
