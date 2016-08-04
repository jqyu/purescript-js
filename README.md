# purescript-js

Native JS data types for better performance.

## Modules

### `JS.Data.Nullable`

Native alternative to `purescript-maybe` using `null` instead of `Nothing`.
Note that `Nullable` loses data. There is no way to express `Just Nothing`.

### `JS.Control.Promise`

An async monad represented internally by `Promise`. This is intended for interop with npm modules.
If you don't need to interop with JS, use something like `purescript-aff`, it's maintained by people who know what they're doing.

For server-side code, it's recommended to use `global.Promise = require('bluebird')`, as bluebird promises are significantly more performant than native v8 promises.

### `JS.Control.Promise.Context`

A stateful promise monad
