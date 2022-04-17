"""
    ArgumentProcessor

A module to help parse command line arguments and parameters.

## functions

        argument
        parameter
        layer
        addarg!
        addpar!
"""
module ArgumentProcessor

import Base.parse
export Varformat, Delimiter, Flag, Option, Parameter, Layer,
    @flag_str, @opt_str, helpstr, printhelp, addflag!, addopt!, addpar!, clearinnerbuffer!, @addflag, @addopt


const FMTLIST = ("%s", "%f", "%g", "%h", "%o", "%b", "%c", "%d", "%l")

"""
    Varformat

must be one of:
- `"%s"`        string
- `"%f"/"%g"`   decimal float
- `"%c"`        complex float
- `"%h"`        hexadecimal **integer**
- `"%o"`        octal **integer**
- `"%b"`        binary **integer**
- `"%d"`        integer
- `"%l"`        logical (true, false, 0 or 1)
"""
struct Varformat
    string::String

    function Varformat(x::String)
        if !(x in FMTLIST)
            error("Invalid type character for \"" * x * "\"")
        end
        return new(x)
    end
end

function Varformat(x)
    try
        t = String(x)
        return Varformat(t)
    catch e
        error("Format $(x) can't be converted to String")
    end
end

function parsefunc(F::Varformat)
    fmt = F.string
    if fmt == "%s"
        return v -> String(v)
    elseif fmt in ("%f", "%g")
        return v -> parse(Float64, v)
    elseif fmt == "%c"
        return v -> parse(ComplexF64, v)
    elseif fmt == "%h"
        return v -> parse(Int, v; base=16)
    elseif fmt == "%o"
        return v -> parse(Int, v; base=8)
    elseif fmt == "%b"
        return v -> parse(Int, v; base=2)
    elseif fmt == "%d"
        return v -> parse(Int, v)
    elseif fmt == "%l"
        return v -> parse(Bool, v)
    else
        error("invalid type character " * fmt)
    end
end

function example(F::Varformat)
    fmt = F.string
    if fmt == "%s"
        return "\"1\""
    elseif fmt in ("%f", "%g")
        return "1.0"
    elseif fmt == "%c"
        return "1.0 + 1.0im"
    elseif fmt == "%h"
        return "0x01"
    elseif fmt == "%o"
        return "0o01"
    elseif fmt == "%b"
        return "0b01"
    elseif fmt == "%d"
        return "1"
    elseif fmt == "%l"
        return "true"
    else
        error("invalid type character " * fmt)
    end
end

const DELIMLIST = ('=', ' ', '/', ',', ';', ''', '"', '%', ':')

"""
    Delimiter

characters included in discription string. the character can be letter or punctuations below:
`'=', ' ', '/', ',', ';', ''', '"'` and `'%'`
"""
struct Delimiter
    string::String

    function Delimiter(d::String)
        if !all(v->(v in DELIMLIST) || isletter(v), collect(d))
            error("Invalid delimiter for \"" * d * "\"")
        end
        return new(d)
    end
end

function Delimiter(x)
    try
        t = String(x)
        return Delimiter(t)
    catch e
        error("Delimiter $(x) can't be converted to String")
    end
end

FormatPart = Union{Varformat, Delimiter}

function _parseinputfmt(str::AbstractString)
    chars = collect(String(str))
    valueloc = Int[]
    L = length(chars)
    i = 1
    while i <= L
        if chars[i] == '%'
            if chars[i+1] == '%'
                i += 2
            elseif String(chars[i:i+1]) in FMTLIST
                push!(valueloc, i)
                i += 2
            end
        else
            i += 1
        end
    end
    if isempty(valueloc)
        error("input format string must consist value type defination.")
    end
    ibegins = [1; valueloc; (valueloc .+ 2)] |> sort |> unique |> t->filter(<=(L), t)
    fmt = FormatPart[]
    for i = 1:length(ibegins)
        ib = ibegins[i]
        ie = i == length(ibegins) ? L : ibegins[i+1]-1
        ts = String(chars[ib:ie])
        if ib in valueloc
            push!(fmt, Varformat(ts))
        else
            push!(fmt, Delimiter(replace(ts, "%%"=>"%")))
        end
    end
    @debug "parse \"$(str)\" to $(fmt)"
    return fmt
end

function _parse(str::String, fmt::Vector{FormatPart})
    @debug "parsing \"$(str)\" using format: $(fmt)"
    Lfmt = length(fmt)
    ibegins = zeros(Int, Lfmt)
    ibegins[1] = 1
    b = 1
    for i = 1:length(fmt)
        if typeof(fmt[i]) <: Delimiter
            id = findnext(fmt[i].string, str, b)
            ibegins[i] = id[1]
            if i < Lfmt
                ibegins[i+1] = id[end] + 1
            end
            b = id[end] + 1
        end
    end
    @debug "part of \"$(str)\" begins at $(ibegins)"
    ivar = findall(v->typeof(v)<:Varformat, fmt)
    v = Any[]
    endat = 0
    for i = 1:length(ivar)
        ib = ibegins[ivar[i]]
        if ivar[i] == Lfmt
            it = findnext(' ', str, ib)
            ie = isnothing(it) ? length(str) : it-1
        else
            ie = ibegins[ivar[i]+1] - 1
        end
        endat = max(endat, ie)
        push!(v, parsefunc(fmt[ivar[i]])(str[ib:ie]))
    end
    if length(v) == 1
        return (v[1], endat)
    else
        return (Tuple(v), endat)
    end
end

function example(fmt::Vector{FormatPart})
    return begin
        map(fmt) do f
            if typeof(f) <: Delimiter
                return f.string
            else
                return example(f)
            end
        end |> join
    end
end

"""
    Flag

Commandline argument which can be specified but not need to assigned to values, like `--argname` or `-argabbr`.
On other word, it's an argument with logical value.
If the variable name exist in commandline input, the value will be set to `true`, otherwise to `false`

contains:

- `innername`       varname which will be the name of variable name after parse
- `outername`       varname which will be displayed and be the input variable name
- `abbreviation`    abbreviation of outername
- `help`            help information
"""
struct Flag
    innername::String
    outername::String
    abbreviation::String
    help::String
end

"""
```julia
Flag(innername::AbstractString; outername::AbstractString = "", abbr::AbstractString = "", help::AbstractString = "")
```
"""
function Flag(innername::AbstractString;
    outername::AbstractString = "",
    abbr::AbstractString = "",
    help::AbstractString = "")
    if isempty(innername)
        error("Name of argument can't be ignored.")
    end
    outername = isempty(outername) ? innername : outername
    return Flag(String(innername), String(outername), String(abbr), String(help))
end

"""
    `@flag_str` -> Flag
"""
macro flag_str(s)
    if !(typeof(s) <: AbstractString)
        error("flag_str should be string")
    end
    if !startswith(s, "--")
        error("flag_str should start with \"--\"")
    end
    Flag(String(s[3:end]))
end

"""
    Option

Commandline argument which should be assigned to values,
usually spicified like `--optname=optval`, `--optname optval` or  `-Abbreviation optval`

contains:

- `innername`       varname which will be the name of variable name after parse
- `outername`       varname which will be displayed and be the input variable name
- `abbreviation`    abbreviation of outername
- `default`         default value (input as string)
- `parsefmt`        parse pattern
- `required`        whether throw error when not exist or not
- `help`            help information
"""
struct Option
    innername::String
    outername::String
    abbreviation::String
    default::String
    parsefmt::Vector{FormatPart}
    required::Bool
    help::String
end

"""
```julia
Option(innername::AbstractString; outername::AbstractString = "", abbreviation::AbstractString = "",
    default::AbstractString = "", fmt::AbstractString = "%s", required::Bool = false, help::AbstractString = "")
```
fmt: C like format discription of input format. The discription is a combination of `Varformat` and `Delimiter`, and will
be appended after the outername. See [`Varformat`](@Varformat) and [`Delimiter`](@Delimiter) for more information.

# Example

To parse the commandline argument:
```shell
program --test=0.1/0.2
```

The argument is setted like:

```julia
Option("test", fmt="=%f/%f")
```
"""
function Option(innername::AbstractString;
    outername::AbstractString = "",
    abbr::AbstractString = "",
    default::AbstractString = "",
    fmt::AbstractString = "%s",
    required::Bool = false,
    help::AbstractString = "")
    if isempty(innername)
        error("Name of argument can't be ignored.")
    end
    if !isempty(default)
        try
            _parse(String(default), _parseinputfmt(fmt))
        catch e
            error("Error while parsing default value of option variable $(innername).")
        end
    end
    outername = isempty(outername) ? innername : outername
    return Option(String(innername), String(outername), String(abbr), String(default),
        _parseinputfmt(fmt), required, String(help))
end

macro opt_str(s)
    if !(typeof(s) <: AbstractString)
        error("opt_str should be string")
    end
    if !startswith(s, "--")
        error("opt_str should start with \"--\"")
    end
    i = findfirst(v->v in DELIMLIST, s)
    varn = String(s[3:i-1])
    fmt = String(s[i:end])
    Option(varn, fmt=fmt)
end

"""
    Parameter

Commandline argument which can only be distinguished by position, like `program par1 par2`.
Different parameters are seperated by space. And other `Delimiter` can be used to discribe the
input format.

contains:

- `position`        position of the variable
- `innername`       varname which will be the name of variable name after parse
- `default`         default value (input as string)
- `parsefmt`        parse pattern
- `required`        whether throw error when not exist or not
- `help`            help information
"""
struct Parameter
    position::Int
    innername::String
    default::String
    parsefmt::Vector{FormatPart}
    required::Bool
    help::String
end

"""
```julia
Parameter(position::Int; innername::AbstractString = "", default::AbstractString = "",fmt::AbstractString = "%s",
    required::Bool = false, help::AbstractString = "")
```

fmt: C like format discription of input format. The discription is a combination of `Varformat` and `Delimiter`, and will
be appended after the outername. See [`Varformat`](@Varformat) and [`Delimiter`](@Delimiter) for more information.

# Example

1. To parse the commandline argument:
```shell
   program 0.1
```
   The argument is setted like:
```julia
   Option(1, fmt="%f")
```
2. commandline argument:
```shell
   program 2022/01/01T00:00:00.0
```
   setting:
```julia
   Option(1, fmt="%d/%d/%dT%d:%d:%f")
```
"""
function Parameter(position::Int;
    innername::AbstractString = "",
    default::AbstractString = "",
    fmt::AbstractString = "%s",
    required::Bool = false,
    help::AbstractString = "")
    if !isempty(default)
        try
            _parse(String(default), _parseinputfmt(fmt))
        catch e
            error("Error while parsing default value of position variable $(innername).")
        end
    end
    innername = isempty(innername) ? "par"*string(position) : innername
    return Parameter(Int(position), String(innername), String(default), _parseinputfmt(fmt), required, String(help))
end

"""
    Layer

A group of `Option` and `Parameter`.

- `name` name of the layer
- `flgs` collect of `Flag`
- `opts` collect of `Option`
- `pars` collect of `Parameter`
"""
struct Layer
    name::String
    flgs::Vector{Flag}
    opts::Vector{Option}
    pars::Vector{Parameter}
end

"""
```julia
Layer(name::AbstractString, flags::Vector{Flag}=Flag[], opts::Vector{Option}=Option[], pars::Vector{Parameter}=Parameter[])
```
"""
Layer(
    name::AbstractString,
    flags::Vector{Flag}=Flag[],
    opts::Vector{Option}=Option[],
    pars::Vector{Parameter}=Parameter[]) = Layer(String(name), flags, opts, pars)

"""
    parse string to defined data structure

```julia
parse(cmdstr::String, lyr::Layer)
```
"""
function parse(cmdstr::String, lyr::Layer)
    chars = collect(cmdstr)
    unused = trues(length(cmdstr))
    argpairs = Pair{Symbol, Any}[]
    for flag in lyr.flgs
        vloc1 = findall("--"*flag.outername, cmdstr)
        vloc2 = isempty(flag.abbreviation) ? UnitRange{Int64}[] : findall("-"*flag.abbreviation, cmdstr)
        vloc = [vloc1; vloc2]
        if isempty(vloc)
            push!(argpairs, Symbol(flag.innername)=>false)
        else
            push!(argpairs, Symbol(flag.innername)=>true)
            for r in vloc
                unused[r] .= false
            end
        end
    end
    for opt in lyr.opts
        vloc1 = findall("--"*opt.outername, cmdstr)
        vloc2 = isempty(opt.abbreviation) ? UnitRange{Int64}[] : findall("-"*opt.abbreviation, cmdstr)
        vloc = [vloc1; vloc2]
        if length(vloc) > 1
            error("more than one values are assigned to $(opt.outername).")
        elseif isempty(vloc)
            if opt.required
                error("variable $(opt.outername) must be specified.")
            else
                if isempty(opt.default)
                    v = nothing
                else
                    (v, _) = _parse(opt.default, opt.parsefmt)
                end
            end
        else
            tloc = vloc[1]
            (v, l) = _parse(String(chars[tloc[end]+1:end]), opt.parsefmt)
            unused[tloc[1]:tloc[end]+l] .= false
        end
        push!(argpairs, Symbol(opt.innername)=>v)
    end
    for i = 1:length(chars)-1
        if chars[i] == ' ' && chars[i+1] == ' ' && unused[i] && unused[i+1]
            unused[i] = false
        end
    end
    parstr = String(chars[unused]) |> strip |> split .|> String
    parloc = [p.position for p in lyr.pars]
    parperm = sortperm(parloc)
    for ip = parperm
        par = lyr.pars[ip]
        if par.position > length(parstr) && par.required
            error("parameter on location $(par.position) must be specified.")
        elseif par.position > length(parstr) && !par.required
            if isempty(par.default)
                v = nothing
            else
                (v, _) = _parse(par.default, par.parsefmt)
            end
        else
            (v, _) = _parse(parstr[par.position], par.parsefmt)
        end
        push!(argpairs, Symbol(par.innername)=>v)
    end
    return NamedTuple(argpairs)
end

"""
```julia
parse(cmdstr::AbstractString, lys::Vector{Layer})
```
"""
function parse(cmdstr::AbstractString, lys::Vector{Layer})
    ps = Pair{Symbol, NamedTuple}[]
    for l in lys
        v = parse(cmdstr, l)
        push!(ps, Symbol(l.name)=>v)
    end
    return NamedTuple(ps)
end

"""
```julia
parse(line::Vector{<:AbstractString}, l::Layer)
```
"""
parse(line::Vector{<:AbstractString}, l::Layer) = parse(join(line, ' '), l)

"""
```julia
parse(line::Vector{<:AbstractString}, l::Vector{Layer})
```
"""
parse(line::Vector{<:AbstractString}, l::Vector{Layer}) = parse(join(line, ' '), l)

"""
    generate help string from defined layers

```julia
helpstr(layers::Vector{Layer})
```

return value:

```julia
(usage_line::String, example_line::String, varname_list::Vector{String},
    abbreviation_list::Vector{String}, helps::Vector{String})
```
"""
function helpstr(layers::Vector{Layer})
    arg_varname = String[]
    arg_usage_line = String[]
    arg_detail_var = String[]
    arg_detail_abbr = String[]
    arg_detail_doc = String[]
    arg_example_line = String[]
    for l in layers
        for flag in l.flgs
            p1 = "--" * flag.outername
            p2 = isempty(flag.abbreviation) ? "" : "-"*flag.abbreviation
            push!(arg_varname, flag.outername)
            push!(arg_usage_line, "["*p1*(isempty(p2) ? "" : "|")*p2*"]")
            push!(arg_detail_var, p1)
            push!(arg_detail_abbr, p2)
            push!(arg_detail_doc, flag.help)
            push!(arg_example_line, p1)
        end
        for opt in l.opts
            p1 = "--" * opt.outername
            p2 = isempty(opt.abbreviation) ? "" : "-"*opt.abbreviation
            push!(arg_varname, opt.outername)
            push!(arg_usage_line, (opt.required ? "" : "[")*p1*(isempty(p2) ? "" : "|")*p2*(opt.required ? "" : "]"))
            push!(arg_detail_var, p1)
            push!(arg_detail_abbr, p2)
            push!(arg_detail_doc, opt.help)
            push!(arg_example_line, p1* (isempty(opt.default) ? example(opt.parsefmt) : opt.default))
        end
    end
    argrange = sortperm(arg_varname)
    arg_varname = arg_varname[argrange]
    arg_usage_line = arg_usage_line[argrange]
    arg_detail_var = arg_detail_var[argrange]
    arg_detail_abbr = arg_detail_abbr[argrange]
    arg_detail_doc = arg_detail_doc[argrange]
    arg_example_line = arg_example_line[argrange]

    parid = Int[]
    pars = Parameter[]
    for l in layers
        for par in l.pars
            push!(parid, par.position)
            push!(pars, par)
        end
    end
    parrange = sortperm(parid)
    pars = pars[parrange]
    par_usage_line = String[]
    par_detail_var = String[]
    par_detail_doc = String[]
    par_example_line = String[]
    for par in pars
        push!(par_usage_line,  (par.required ? "" : "[")*par.innername*(par.required ? "" : "]"))
        push!(par_detail_var, par.innername)
        push!(par_detail_doc, par.help)
        push!(par_example_line, isempty(par.default) ? example(par.parsefmt) : par.default)
    end
    return (
        join([arg_usage_line; par_usage_line], ' '),
        join([arg_example_line; par_example_line]),
        vcat(arg_detail_var, par_detail_var),
        arg_detail_abbr,
        vcat(arg_detail_doc, par_detail_doc))
end

function splitbymargin(str::AbstractString, ruler::Int)
    words = split(str, keepempty=false)
    lines = String[""]
    for i = 1:length(words)
        if length(lines[end]) + 1 + length(words[i]) <= ruler
            lines[end] *= (isempty(lines[end]) ? "" : " ")*words[i]
        else
            push!(lines, words[i])
        end
    end
    return lines
end

"""
    print help doc of defined data structure

```julia
printhelp(layers::Vector{Layer}, programname::AbstractString=""; indent::Int=4,
    maxabbrcol::Int=5, maxvarcol::Int=10, maxdoccol::Int=60)
```
"""
function printhelp(layers::Vector{Layer}, programname::AbstractString=""; indent::Int=4,
    maxabbrcol::Int=5, maxvarcol::Int=10, maxdoccol::Int=60)
    (usagestr, examplestr, varlist, argabbr, docs) = helpstr(layers)
    println("Usage: ", programname, " ", usagestr)
    println("\nExample:\n", " "^indent, programname, " ", examplestr)
    varl = min(maximum(length.(varlist)), maxvarcol)
    abbrl = min(maximum(length.(argabbr)), maxabbrcol)
    L = length(argabbr)
    println("\nArgument:\n")
    for i = 1:length(varlist)
        print(" "^indent)
        if i <= L && !isempty(argabbr[i])
            print(argabbr[i], ',')
            if length(argabbr[i]) + 1 < abbrl
                print(" "^(abbrl-1-length(argabbr[i])))
            end
        else
            print(" "^abbrl)
        end
        print(varlist[i])
        if length(varlist[i]) > varl
            print('\n', " "^(indent+varl+abbrl))
        else
            print(" "^(varl-length(varlist[i])))
        end
        if length(docs[i]) < maxdoccol
            println(docs[i])
        else
            hl = splitbymargin(docs[i], maxdoccol)
            println(hl[1])
            if length(hl) > 1
                for i = 2:length(hl)
                    println(" "^(indent+varl+abbrl), hl[i])
                end
            end
        end
    end
    return nothing
end

global INNER_FLAG = Flag[]
global INNER_OPTION = Option[]
global INNER_PARAMETER = Parameter[]

"""
    add a `Flag` type variable to inner buffer

```julia
addflag!(f::Flag)
```
"""
function addflag!(f::Flag)
    global INNER_FLAG
    push!(INNER_FLAG, f)
    return nothing
end

"""
    add a `Option` type variable to inner buffer

```julia
addopt!(a::Option)
```
"""
function addopt!(a::Option)
    global INNER_OPTION
    push!(INNER_OPTION, a)
    return nothing
end

"""
    add a `Parameter` type variable to inner buffer

```julia
addpar!(p::Parameter)
```
"""
function addpar!(p::Parameter)
    global INNER_PARAMETER
    push!(INNER_PARAMETER, p)
    return nothing
end

"""
```julia
addflag!(innername::AbstractString; outername::AbstractString = "",
    abbr::AbstractString = "", help::AbstractString = "")
```
"""
addflag!(innername::AbstractString; outername::AbstractString = "",
    abbr::AbstractString = "", help::AbstractString = "") =
    addflag!(Flag(innername, outername=outername, abbr=abbr, help=help))

"""
```julia
addopt!(innername::AbstractString; outername::AbstractString = "", abbr::AbstractString = "",
    default::AbstractString = "", fmt::AbstractString = "%s", required::Bool = false, help::AbstractString = "")
```
"""
addopt!(
    innername::AbstractString;
    outername::AbstractString = "",
    abbr::AbstractString = "",
    default::AbstractString = "",
    fmt::AbstractString = "%s",
    required::Bool = false,
    help::AbstractString = ""
) = addopt!(Option(innername; outername=outername, abbr=abbr, default=default, fmt=fmt, required=required, help=help))

"""
```julia
addpar!(position::Int; innername::AbstractString = "", default::AbstractString = "", fmt::AbstractString = "%s",
    required::Bool = false, help::AbstractString = "")
```
"""
function addpar!(position::Int = 0;
    innername::AbstractString = "",
    default::AbstractString = "",
    fmt::AbstractString = "%s",
    required::Bool = false,
    help::AbstractString = "")
    if position == 0
        position = length(INNER_PARAMETER) + 1
    end
    addpar!(Parameter(position; innername=innername, default=default, fmt=fmt, required=required, help=help))
    return nothing
end

"""
    @addflag s

like `addflag!`, add a `Flag` type variable to inner buffer
"""
macro addflag(s)
    return :(addflag!(@flag_str($s)))
end

"""
    @addopt s

like `addopt!`, add a `Option` type variable to inner buffer
"""
macro addopt(s)
    return :(addopt!(@opt_str($s)))
end

"""
    clean the inner buffer

```julia
clearinnerbuffer!()
```
"""
function clearinnerbuffer!()
    global INNER_FLAG, INNER_OPTION, INNER_PARAMETER
    INNER_FLAG = Flag[]
    INNER_OPTION = Option[]
    INNER_PARAMETER = Parameter[]
    return nothing
end

"""
    `parse(line::AbstractString)`

parse commandline input according to inner buffer
"""
parse(line::AbstractString) = parse(String(line), Layer("UNKNOWN", INNER_FLAG, INNER_OPTION, INNER_PARAMETER))

"""
    `parse(lines::Vector{<:AbstractString})`

parse commandline input according to inner buffer
"""
parse(lines::Vector{<:AbstractString}) = parse(join(lines, ' '))
end
