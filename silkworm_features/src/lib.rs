/// Structure containing all feature flags in the language.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Features {
    /// Whether pragmas are allowed at all.
    pub pragmas: bool,

    pub unicode_identifiers: bool,
    pub scoped_nodes: bool,
    pub scoped_variables: bool,
    pub subroutine: bool,
    pub string_interpolation: bool,
    pub extended_escape: bool,
}

impl Features {
    /// Returns a feature set with "default" silkworm extensions enabled. Same as `Default::default`.
    pub fn new() -> Self {
        Features {
            pragmas: true,

            unicode_identifiers: false,
            scoped_nodes: true,
            scoped_variables: true,
            subroutine: true,
            string_interpolation: true,
            extended_escape: true,
        }
    }

    /// Returns a feature set with everything disabled except pragmas.
    ///
    /// Features can still be turned on at the file level using feature pragmas.
    pub fn basic() -> Self {
        Features {
            pragmas: true,

            unicode_identifiers: false,
            scoped_nodes: false,
            scoped_variables: false,
            subroutine: false,
            string_interpolation: false,
            extended_escape: false,
        }
    }

    /// Returns a feature set that is intended to be compatible with YarnSpinner 1.1.
    ///
    /// This disables everything including pragmas. Features cannot be turned on at the file level.
    pub fn compatible() -> Self {
        Features {
            pragmas: false,

            unicode_identifiers: false,
            scoped_nodes: false,
            scoped_variables: false,
            subroutine: false,
            string_interpolation: false,
            extended_escape: false,
        }
    }

    /// Returns a feature set with everything enabled.
    pub fn all() -> Self {
        Features {
            pragmas: true,

            unicode_identifiers: true,
            scoped_nodes: true,
            scoped_variables: true,
            subroutine: true,
            string_interpolation: true,
            extended_escape: true,
        }
    }
}

impl Default for Features {
    fn default() -> Self {
        Features::new()
    }
}
