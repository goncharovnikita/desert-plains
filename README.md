# desert-plains

DesertPlains is a template language, it compiles down to HTML

Language files has extension `.desertp`

Html, css and js are all written in one file

- [desert-plains](#desert-plains)
  - [Installation](#installation)
    - [Mac OS](#mac-os)
  - [Why?](#why)
  - [Syntax](#syntax)
    - [Css](#css)
  - [Examples](#examples)
  - [Usage](#usage)
- [TODO:](#todo)

## Installation

### Mac OS

```bash
  curl -L https://github.com/goncharovnikita/desert-plains/releases/v1.0.5/download/desert-plains-1.0.5-macOS > ./desert-plains-1.0.5
  mv ./desert-plains-1.0.5 /usr/local/bin/desert-plains
  rm ./desert-plains-1.0.5
```

## Why?

That's my fun project to learn haskell

Main idea is to create lightweight syntax to describe state of the page and add ability to compile it code using some popular libraries such as `React`

## Syntax

Everything must be enclosed in tags - `[]`

Page should contain at least `[head]` and `[body]`

```desertp
[body
    [h1 "Title of the page"]
]
```

Will be compiled to

```html
<body>
    <h1>Title of the page</h1>
</body>
```

Classes written after name of tag, divided by dot:

```desertp
[body.some.class]
```

Will be compiled to

```html
<body class="some class"></body>
```

### Css

For some reason every css modifier should be in tag:

```
[style
    [.some-class
        height: 100px;
        width: auto;
    ]
]
```

Will be compiled to

```html
<style>
    .some-class {
        height: 100px;
        width: auto;
    }
</style>
```

## Examples

Test example with .desertp file and compiled html are in [examples folder](./test-data)

## Usage

To start compiler run `desert-plains-exe --src .. --dest ..`
It will start a compiler with watcher, which will compile index.desertp to index.html

# TODO:
- [ ] Add script engine
- [ ] Add javascript support
