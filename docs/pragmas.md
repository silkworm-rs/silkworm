# silkworm Pragmas

**silkworm is currently in the initial development phase. Any and all features advertised might not be implemented yet!**

## `feature` / `disable_feature`

```
//#! feature(<feature_name>, ...)`
//#! disable_feature(<feature_name>, ...)`
```

The `feature` / `disable_feature` pragmas are used to enable or disable individual language features for the file they are contained in. These directives override default settings on the runtime. `feature` and `disable_feature` pragmas must be in the "inner"-style and placed at the beginning of files.

For example, to enable the `scoped_variables` feature and disable the `function_nodes` feature:

```
//#! feature(scoped_variables), disable_feature(function_nodes)

title: some_node
---
Some code.
===
```

## `private`

```
//# private
```

The `private` pragma is used to mark a node as private when the [`scoped_nodes` feature](features.md#scoped-nodes) is enabled. Nodes with the `private` pragma will only be visible to nodes within the same file.

For example, the following code will work fine because both nodes are contained in the same file:

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

Meanwhile, attempting to access `_hello_world` in another file will trigger an error if another node with the same name is not defined in that file, because the identifier is not visible in the global scope.

## `sub`

```
//# sub(<arg_name>, ...)
```

The `sub` pragma is used to mark a node as a subroutine when the [`subroutine` feature](features.md#subroutine) is enabled.