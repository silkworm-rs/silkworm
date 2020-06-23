# Language features

**silkworm is currently in the initial development phase. Any and all features advertised might not be implemented yet!**

silkworm's dialect includes a number of useful extensions over Yarn 1.1. These features can be enabled or disabled individually on the runtime, or for individual files using [`feature` pragmas](pragmas.md#feature-disable-feature) at the top of the file.

## `unicode_identifiers`

Disabled by default.

When enabled, Unicode `XID_Start` and `XID_Continue` characters can be used in titles and variable identifiers, along with ASCII alphanumerics and underscores:

```
//#! feature(unicode_identifiers)

title: すまん_不都合なことばかりで
---
<<set $答え to 42>>
===
```

## `scoped_nodes`

Enabled by default.

When enabled, nodes with the [`private` pragma](pragmas.md#private) will only be visible to nodes within the same file:

```
title: entry
---
[[_hello_world]]
===

//# private
title: _hello_world
---
Hello, world!
===
```

## `scoped_variables`

Enabled by default.

Users may declare and use temporary variables without sigils. Such variables will be node-scoped, and ephemeral. When `subroutine` is enabled, the variables will be allocated separately for each visit, so that recursion works correctly.

Unlike persistent variables, the storage of temporary variables is handled by the runtime.

```
<<set sum to $a + $b>>
{$a} + {$b} is {sum}!
```

Users may also declare and use node-scoped variables with the `@` sigil, or file-scoped variables with the `@@` sigil. These variables persist between visits like normal Yarn variables:

```
<<set @node_var to 42>>
<<set @@file_var to 42>>
```

## `subroutine`

Enabled by default. Requires `scoped_variables`.

Users may declare subroutines using the [`sub` pragma](pragmas.md#sub) on a node. The `return` instruction becomes special. Users may call subroutines by adding arguments list or using the `set` command in the target. Content after the call will be run after the called node ends or returns.

Subroutines cannot be called in expressions.

To declare a subroutine node, add the `sub` pragma:

```
//# sub(person_name)
title: greet
---
// Arguments declared in the pragma are declared as local temporary
// variables.
Hello, {person_name}!
// A value may be returned using the return instruction.
<<return person_name == "World">>
===

//# sub
title: hello_world
---
[[set is_grandiose to greet("World")]]
<<return is_grandiose>>
===
```

To call them, use the subroutine variants of option and jump instructions:

- `[[sub_name(arg, ..)]]`

    Call `sub_name` unconditionally. Discard the return value.

- `[[set ret to sub_name(arg, ..)]]`

    Call `sub_name` unconditionally. Set the value of `ret` to the return value.

- `[[Option | sub_name(arg, ..)]]`

    Call `sub_name` if `Option` is chosen. Discard the return value.

- `[[Option | set ret to sub_name(arg, ..)]]`

    Call `sub_name` if `Option` is chosen. Set the value of `ret` to the return value.

Example:

```
title: intro
---
What's your name?

// Use set to take the return value of a subroutine.
[[Jane Roe | set is_grandiose to greet("Jane Roe")]]
// The parens must be added even if the argument list is empty.
[[World | set is_grandiose to hello_world()]]
// Return values of subroutines maybe ignored.
[[...(say nothing) | produce_error()]]

<<if is_grandiose>>
    One with a lot of self-importance, aren't you?
    Anyway, Let's get to the main story.
<<else>>
    Let's get to the main story.
<<endif>>

[[start_story]]
===

//# sub
title: produce_error
---
// The runtime will produce an error in case of argument count mismatch
[[greet()]]
===
```

Even while within a subroutine, `stop` will still end the dialogue immediately. To exit the subroutine, use `return` without an argument instead. The return value will be `null`.

## `string_interpolation`

Enabled by default.

When enabled, users may create string literals delimited with `` ` ``. Such string literals may contain inline expressions in `{}` and format functions in `[]`, like in ordinary text:

```
<<set $a to 4>>
<<set $b to 2>>
<<set $text to `{$a}{$b}`>>
The answer is {$text}!
```

To include a literal `{` or `[`, one could escape them with `\`.

## `extended_escape`

Enabled by default.

When enabled, adds a number of new escape sequences:

- `\n` - Newline
- `\t` - Tab
- `\#` - Hashtag symbol (only valid in free text)
- `\x41` - 7-bit character code
- `\u{7FFF}` - 24-bit Unicode character code