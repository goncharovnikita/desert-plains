# desert-plains

DesertPlains is a template language, it compiles down to HTML

Language files has extension `.desertp`

Html, css and js are all written in one file

- [desert-plains](#desert-plains)
  - [Why?](#why)
  - [Syntax](#syntax)
    - [Css](#css)
    - [Components](#components)
  - [Examples](#examples)
  - [Usage](#usage)
- [TODO:](#todo)

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

### Components

Same as html tag, but with props and raw javascript support

Component definition:

```
[Counter[up, name] -- up and name are props
    -- {{ raw javascript }}
    {{
        const [count, setCount] = useState(0);
        const increment = useCallback(() => setCount(c => up ? c + 1 : c - 1), []);
    }}

    -- Every string is interpolated after compilation by default (i.e. will look like this: `Counter ${name} is ${count}`)
    -- And all props and javascript variables are in scope of component
    
    "Counter ${name} is ${count}"
    [button.btn onClick={increment}
        "Increment"
    ]
]
```

Component usage:

```
    -- Bool props without value will be fallback to true
    [Counter up name="Up"]
    -- Not passed props fallback to undefined
    [Counter name="Down 2"]
```

## Examples

Test example with .desertp file and compiled html are in [examples folder](./test-data)

## Usage

To start compiler run `desert-plains-exe --src .. --dest ..`
It will start a compiler with watcher, which will compile index.desertp to index.html

# TODO:
- [ ] Add script engine
- [ ] Add javascript support
