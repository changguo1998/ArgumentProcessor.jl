# ArgumentProcessor

A module to help parse command line arguments and parameters.

## Structure

There are three type of input arguments:

- `Flag` arguments that are not assigned with values.(Or it can be seen as logical value)
- `Option` arguments that are specified with both name and value
- `Parameter` arguments that are distinguished by position

The arguments can be divided to several `Group`

## Usage

### `Flag`

The `Flag` type can be created by

```julia
Flag(innername::AbstractString; outername::AbstractString = "",
    abbr::AbstractString = "", help::AbstractString = "")
@flag_str "--flag"
flag"--flag"
```

when generating `Flag` type using macro, the other properties like `abbr`, `help` can't be specified.

### `Option`

`Option` type can be created by

```julia
Option(innername::AbstractString; outername::AbstractString = "",
    abbr::AbstractString = "", default::AbstractString = "",
    fmt::AbstractString = "%s", required::Bool = false,
    help::AbstractString = "")
@opt_str "--optionFmt"
opt"--optionFmt"
```

When using macros to create `Option` type, one can only specify the name and input format. To add more information,
using function format instead. The `Fmt` represent the input format, see next chapter for detail.

### `Parameter`

The `Parameter` can be created by

```julia
Parameter(position::Int; innername::AbstractString = "",
    default::AbstractString = "", fmt::AbstractString = "%s",
    required::Bool = false, help::AbstractString = "")
```

The `Parameter` type is recognized by position, and the innername will be `par1`, `par2`, ...
if `innername` is not specified.

### `Group`

A `Group` contains several `Flag`, `Option` and `Parameter`, which is created by

```julia
Group(name::AbstractString, flags::Vector{Flag}=Flag[],
    opts::Vector{Option}=Option[], pars::Vector{Parameter}=Parameter[])
```

### Input format

The input format is indicated by a C-like string.
The symbols below
`"%s", "%f", "%g", "%h", "%o", "%b", "%c", "%d", "%l"`
describ the data type, and other symbols will be explained to delimiter.

Their meanings are:

- `"%s"`        string
- `"%f"/"%g"`   decimal float
- `"%c"`        complex float
- `"%h"`        hexadecimal **integer**
- `"%o"`        octal **integer**
- `"%b"`        binary **integer**
- `"%d"`        integer
- `"%l"`        logical (true, false, 0 or 1)


## Quick example

### Example 1 (using defined `Group`)

Set julia script like:

```julia
using ArgumentProcessor

group = Group(
    "group1",
    [
        Flag("flag1")
    ],
    [
        Option("float", abbr="F", fmt=" %f", help="Input a float",
            required=true), # pay attention to the space before %f
        Option("datatime", abbr="D", fmt="%d/%d/%dT%d:%d:%f",
            help="Input a datetime format"),
        Option("string", abbr="S", fmt=" \"%s\"") # had better add " around the string
    ],
    [
        Parameter(1, fmt="%f", default="0.1", help="First float parameter")
    ]
)

input = ArgumentProcessor.parse(ARGS, group)
```

and run the script using

```shell
julia -- program.jl --flag1 --float 0.1 -D2022/01/01T10:01:10.5 --string '"filename"'
```

the input will be parsed to

```julia
(flag1 = true, float = 0.1, datatime = (2022, 1, 1, 10, 1, 10.5),
    string = "filename", par1 = 0.1)
```

---

when run the script using

```shell
julia -- program.jl --help
```

it will print text like

```shell
$ julia program.jl --help
Usage: test.jl [--datatime(int)/(int)/(int)T(int):(int):(float)|-D(int)/(int)/(int)T(int):(int):(float)] [--flag1] --float (float)|-F (float) [--string "(string)"|-S "(string)"] [par1(float)]

Example:
    test.jl --datatime1/1/1T1:1:1.0 --flag1 --float 1.0 --string ""1"" 0.1

Argument:

    -D, --datatime Input a datetime format
        --flag1
    -F, --float    Input a float
    -S, --string
        par1       First float parameter
```

### Example 2 (using inner buffer)

Set julia script like:

```julia
using ArgumentProcessor
addflag!("flag1")
addopt!("float", abbr="F", fmt=" %f", help="Input a float", required=true)
addopt!("datetime", abbr="D", fmt="%d/%d/%dT%d:%d:%f", help="Input a datetime format")
addopt!("string", abbr="S", fmt=" \"%s\"")
addpar!(1; fmt="%f", default="0.1", help="First float parameter")
input = ArgumentProcessor.parse(ARGS)
```

and it will act as `Example 1`

### Example 3 (save or load settings)

The settings can be parsed to `Dict` type to save,
and can be parsed from loaded `Dict` type.

you can save with script below:

```julia
using ArgumentProcessor, TOML

addflag!("flag1")
addopt!("float", abbr="F", fmt=" %f", help="Input a float", required=true)
addopt!("datetime", abbr="D", fmt="%d/%d/%dT%d:%d:%f", help="Input a datetime format")
addopt!("string", abbr="S", fmt=" \"%s\"")
addpar!(1; fmt="%f", default="0.1", help="First float parameter")

open("arg_setting.toml", "w") do io
    TOML.print(io, Dict(group))
end
```

the settings are saved like:

```toml
name = "group1"

[[flags]]
outername = "flag1"
help = ""
abbr = ""
innername = "flag1"

[[opts]]
outername = "float"
format = " %f"
default = ""
required = true
help = "Input a float"
abbr = "F"
innername = "float"
[[opts]]
outername = "datatime"
format = "%d/%d/%dT%d:%d:%f"
default = ""
required = false
help = "Input a datetime format"
abbr = "D"
innername = "datatime"
[[opts]]
outername = "string"
format = " \"%s\""
default = ""
required = false
help = ""
abbr = "S"
innername = "string"

[[pars]]
format = "%f"
default = "0.1"
required = false
help = "First float parameter"
position = 1
innername = "par1"
```

and the settings can be loaded like:

```julia
using ArgumentProcessor, TOML

group = TOML.parsefile("arg_setting.toml") |> Group
```

## History

- `1.0.0` submit first version
- `1.1.0` add function to check parameter setting
