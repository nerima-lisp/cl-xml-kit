# Migration to 2.0.0

The 2.0.0 API changes five exported functions to take success and failure callbacks (continuation-passing style, or CPS).

## Comparison basis

The pre-CPS side is the [untagged source snapshot `a659f4bc06a75634bf6698954ff17a6a32d88f98`](https://github.com/nerima-lisp/cl-xml-kit/tree/a659f4bc06a75634bf6698954ff17a6a32d88f98). Its ASDF system declares version `1.0.0`, but that snapshot is not a published release or tag. The repository has no published 1.x release or tag for a release-to-release comparison.

The 2.0.0 target signatures below match the [source snapshot at commit `2a7b4e011053edd444baae181f26e0e958720349`](https://github.com/nerima-lisp/cl-xml-kit/tree/2a7b4e011053edd444baae181f26e0e958720349). Treat this as a source-to-source comparison, not a comparison between released versions.

## Export changes

**Exports removed in 2.0.0: none.** The `src/package.lisp` export lists are identical in the two snapshots.

## Signature changes

Each function adds positional `success` and `failure` callbacks after its required arguments and before its keyword arguments. The function calls `success` once with the operation result or `failure` once with the condition, then returns the values from the callback it called.

### `select-xml`

- Pre-CPS: `(source path &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`
- 2.0.0 target: `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`

#### Pre-CPS example

```lisp
(asdf:load-system "cl-xml-kit")
(let ((matches
        (cl-xml-kit:select-xml "<root><item/></root>" "/root/item")))
  (write-line (cl-xml-kit:xml-match-path (first matches))))
```

#### 2.0.0 target example

```lisp
(asdf:load-system "cl-xml-kit")
(cl-xml-kit:select-xml
 "<root><item/></root>" "/root/item"
 (lambda (matches)
   (write-line (cl-xml-kit:xml-match-path (first matches))))
 (lambda (condition)
   (error condition)))
```

### `edit-xml-set`

- Pre-CPS: `(source path value &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`
- 2.0.0 target: `(source path value success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`

#### Pre-CPS example

```lisp
(asdf:load-system "cl-xml-kit")
(let ((result
        (cl-xml-kit:edit-xml-set "<root>old</root>" "/root/text()" "new")))
  (write-line result))
```

#### 2.0.0 target example

```lisp
(asdf:load-system "cl-xml-kit")
(cl-xml-kit:edit-xml-set
 "<root>old</root>" "/root/text()" "new"
 (lambda (result)
   (write-line result))
 (lambda (condition)
   (error condition)))
```

### `edit-xml-delete`

- Pre-CPS: `(source path &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`
- 2.0.0 target: `(source path success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`

#### Pre-CPS example

```lisp
(asdf:load-system "cl-xml-kit")
(let ((result
        (cl-xml-kit:edit-xml-delete "<root><item/></root>" "/root/item")))
  (write-line result))
```

#### 2.0.0 target example

```lisp
(asdf:load-system "cl-xml-kit")
(cl-xml-kit:edit-xml-delete
 "<root><item/></root>" "/root/item"
 (lambda (result)
   (write-line result))
 (lambda (condition)
   (error condition)))
```

### `edit-xml-append`

- Pre-CPS: `(source path fragment &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`
- 2.0.0 target: `(source path fragment success failure &key (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`

#### Pre-CPS example

```lisp
(asdf:load-system "cl-xml-kit")
(let ((result
        (cl-xml-kit:edit-xml-append "<root/>" "/root" "<item/>")))
  (write-line result))
```

#### 2.0.0 target example

```lisp
(asdf:load-system "cl-xml-kit")
(cl-xml-kit:edit-xml-append
 "<root/>" "/root" "<item/>"
 (lambda (result)
   (write-line result))
 (lambda (condition)
   (error condition)))
```

### `format-xml`

- Pre-CPS: `(source &key (indent 2) (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`
- 2.0.0 target: `(source success failure &key (indent 2) (resource-limits *default-xml-resource-limits*) (external-entity-policy :reject))`

#### Pre-CPS example

```lisp
(asdf:load-system "cl-xml-kit")
(let ((result (cl-xml-kit:format-xml "<root><item/></root>")))
  (write-line result))
```

#### 2.0.0 target example

```lisp
(asdf:load-system "cl-xml-kit")
(cl-xml-kit:format-xml
 "<root><item/></root>"
 (lambda (result)
   (write-line result))
 (lambda (condition)
   (error condition)))
```

The other exported signatures are unchanged in this comparison. `scan-xml-spans` is not exported in either snapshot and is not part of the public API comparison.
