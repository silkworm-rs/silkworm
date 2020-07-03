# Keywords

**silkworm is currently in the initial development phase. Any and all features advertised might not be implemented yet!**

## Strict keywords

Strict keywords can only be used in their correct contexts. They cannot be used as identifiers of:

- Nodes
- Variables and subroutine parameters
- Custom commands and format functions

### Used keywords

These keywords are actually in use:

- `set`
- `call`
- `true`
- `false`
- `null`
- `if`
- `elseif`
- `else`
- `endif`
- `to`
- `is`
- `eq`
- `neq`
- `lt`
- `lte`
- `gt`
- `gte`
- `and`
- `or`
- `xor`
- `not`
- `return`

### Reserved keywords

These keywords are not currently used by silkworm, but reserved for forward compatibility:

- `for`
- `loop`
- `while`
- `do`
- `done`
- `continue`
- `break`
- `in`
- `switch`
- `case`

## Weak keywords

Weak keywords have special meaning only in certain contexts. Outside these contexts, it is possible to declare identifiers with the same names.

### Special header keys

These keywords are only special when used as header keys:

- `title`
- `tags`

### Pragma names

These keywords are only special in pragmas:

- `feature`
- `disable_feature`
- `private`
- `sub`

### Feature names

These keywords are only special in pragmas:

- `unicode_identifiers`
- `scoped_nodes`
- `scoped_variables`
- `subroutine`
- `string_interpolation`
- `extended_escape`