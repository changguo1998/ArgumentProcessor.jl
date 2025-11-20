"""
    ArgumentProcessor

A module to help parse command line arguments and parameters.
"""
module ArgumentProcessor

export Varformat, Delimiter, Flag, Option, Parameter, Group,
       helpstr, printhelp, addflag!, addopt!, addpar!, clearinnerbuffer!, checksetting,
       @addflag, @addopt, @printhelp, @flag_str, @opt_str

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
        return v -> Base.parse(Float64, v)
    elseif fmt == "%c"
        return v -> Base.parse(ComplexF64, v)
    elseif fmt == "%h"
        return v -> Base.parse(Int, v; base=16)
    elseif fmt == "%o"
        return v -> Base.parse(Int, v; base=8)
    elseif fmt == "%b"
        return v -> Base.parse(Int, v; base=2)
    elseif fmt == "%d"
        return v -> Base.parse(Int, v)
    elseif fmt == "%l"
        return v -> Base.parse(Bool, v)
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

function vartype(F::Varformat)
    fmt = F.string
    if fmt == "%s"
        s = "string"
    elseif fmt in ("%f", "%g")
        s = "float"
    elseif fmt == "%c"
        s = "real+image*im"
    elseif fmt == "%h"
        s = "hexInt"
    elseif fmt == "%o"
        s = "octInt"
    elseif fmt == "%b"
        s = "binInt"
    elseif fmt == "%d"
        s = "int"
    elseif fmt == "%l"
        s = "bool"
    else
        error("invalid type character " * fmt)
    end
    return "(" * s * ")"
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
        if !all(v -> (v in DELIMLIST) || isletter(v), collect(d))
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

FormatPart = Union{Varformat,Delimiter}

function _parseinputfmt(str::AbstractString)
    # split to Char vector
    chars = collect(String(str))
    # find the location of input value
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
    # find the head location of each segment
    ibegins = [1; valueloc; (valueloc .+ 2)] |> sort |> unique |> t -> filter(<=(L), t)
    # parse each segment
    fmt = FormatPart[]
    for i = eachindex(ibegins)
        ib = ibegins[i]
        ie = i == length(ibegins) ? L : ibegins[i+1] - 1
        ts = String(chars[ib:ie])
        if ib in valueloc
            push!(fmt, Varformat(ts))
        else
            push!(fmt, Delimiter(replace(ts, "%%" => "%")))
        end
    end
    @debug "parse \"$(str)\" to $(fmt)"
    return fmt
end

"""
    `_parse(str::String, fmt::Vector{FormatPart})`

inner function.

parse string to specified format
"""
function _parse(str::String, fmt::Vector{FormatPart})
    @debug "parsing \"$(str)\" using format: $(fmt)"
    # find begin location of each segment in string
    Lfmt = length(fmt)
    ibegins = zeros(Int, Lfmt)
    ibegins[1] = 1
    b = 1
    for i = 1:length(fmt)
        if typeof(fmt[i]) <: Delimiter
            id = findnext(fmt[i].string, str, b)
            ibegins[i] = id[1] # begin of the delimiter segment
            if i < Lfmt
                ibegins[i+1] = id[end] + 1 # begin of the var segment after the delimiter
            end
            b = id[end] + 1
        end
    end
    @debug "part of \"$(str)\" begins at $(ibegins)"
    ivar = findall(v -> typeof(v) <: Varformat, fmt) # variable location
    v = Any[]
    endat = 0 # record the used characters
    for i = eachindex(ivar)
        ib = ibegins[ivar[i]]
        if ivar[i] == Lfmt
            # if the last segment is var, the segment will be end before a space;
            # if there is no space in the rest part, it will end at the end of input string
            it = findnext(' ', str, ib)
            ie = isnothing(it) ? length(str) : it - 1
        else
            ie = ibegins[ivar[i]+1] - 1
        end
        endat = max(endat, ie)
        push!(v, parsefunc(fmt[ivar[i]])(str[ib:ie]))
    end
    if typeof(fmt[end]) <: Delimiter
        # correct the endat record if the last segment is delimiter
        endat = ibegins[end] + length(fmt[end].string) - 1
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
Flag(innername::AbstractString; outername::AbstractString="", abbr::AbstractString="", help::AbstractString="")
```
"""
function Flag(innername::AbstractString;
              outername::AbstractString="",
              abbr::AbstractString="",
              help::AbstractString="")
    if isempty(innername)
        error("Name of argument can't be ignored.")
    end
    outername = isempty(outername) ? innername : outername
    return Flag(String(innername), String(outername), String(abbr), String(help))
end

"""
    `Flag(d::Dict)`

generate `Flag` type from `Dict` type. The `Dict` must contain keys:
`"innername"` and optional keys `"outername"`, `"abbr"` and `"help"`
"""
function Flag(d::Base.Dict)
    ks = keys(d)
    inner = d["innername"]
    outer = ("outername" in ks) ? d["outername"] : ""
    abbr = ("abbr" in ks) ? d["abbr"] : ""
    hp = ("help" in ks) ? d["help"] : ""
    return Flag(inner; outername=outer, abbr=abbr, help=hp)
end

function Dict(f::Flag)
    return Dict{String,String}("innername" => f.innername, "outername" => f.outername, "abbr" => f.abbreviation,
                               "help" => f.help)
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
Option(innername::AbstractString; outername::AbstractString="", abbr::AbstractString="",
       default::AbstractString="", fmt::AbstractString="%s", required::Bool=false, help::AbstractString="")
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
Option("test"; fmt="=%f/%f")
```
"""
function Option(innername::AbstractString;
                outername::AbstractString="",
                abbr::AbstractString="",
                default::AbstractString="",
                fmt::AbstractString="%s",
                required::Bool=false,
                help::AbstractString="")
    if isempty(innername)
        error("Name of argument can't be ignored.")
    end
    if !isempty(default)
        try
            _parse(String(default), _parseinputfmt(fmt))
        catch e
            error("Error while parsing default value \"$(default)\" of option variable $(innername).")
        end
    end
    outername = isempty(outername) ? innername : outername
    return Option(String(innername), String(outername), String(abbr), String(default),
                  _parseinputfmt(fmt), required, String(help))
end

"""
    `Option(d::Dict)`

generate `Option` type from `Dict` type. The `Dict` must contain keys:
`"innername"` and optional keys `"outername"`, `"abbr"`, `"default"`, `"format"`, `"required"` and `"help"`
"""
function Option(d::Base.Dict)
    ks = keys(d)
    inner = d["innername"]
    outer = ("outername" in ks) ? d["outername"] : ""
    abbr = ("abbr" in ks) ? d["abbr"] : ""
    dft = ("default" in ks) ? d["default"] : ""
    fmt = ("format" in ks) ? d["format"] : ""
    rqd = ("required" in ks) ? d["required"] : false
    hp = ("help" in ks) ? d["help"] : ""
    return Option(inner; outername=outer, abbr=abbr, default=dft, fmt=fmt, required=rqd, help=hp)
end

"""
    @opt_str -> Option

Create `Option` type using input format string
"""
macro opt_str(s)
    if !(typeof(s) <: AbstractString)
        error("opt_str should be string")
    end
    if !startswith(s, "--")
        error("opt_str should start with \"--\"")
    end
    i = findfirst(v -> v in DELIMLIST, s)
    varn = String(s[3:i-1])
    fmt = String(s[i:end])
    Option(varn; fmt=fmt, required=true)
end

"""
"""
function Dict(o::Option)
    fmt = map(o.parsefmt) do v
              if typeof(v) <: Delimiter
                  t = replace(v.string, "%" => "%%")
              else
                  t = v.string
              end
              t
          end |> join |> String
    return Base.Dict("innername" => o.innername, "outername" => o.outername, "abbr" => o.abbreviation,
                "default" => o.default, "format" => fmt, "required" => o.required, "help" => o.help)
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
Parameter(position::Int; innername::AbstractString="", default::AbstractString="", fmt::AbstractString="%s",
          required::Bool=false, help::AbstractString="")
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
Option(1; fmt="%f")
```

 2. commandline argument:

```shell
   program 2022/01/01T00:00:00.0
```

setting:

```julia
Option(1; fmt="%d/%d/%dT%d:%d:%f")
```
"""
function Parameter(position::Int;
                   innername::AbstractString="",
                   default::AbstractString="",
                   fmt::AbstractString="%s",
                   required::Bool=false,
                   help::AbstractString="")
    if !isempty(default)
        try
            _parse(String(default), _parseinputfmt(fmt))
        catch e
            error("Error while parsing default value \"$(default)\" of position variable $(innername).")
        end
    end
    innername = isempty(innername) ? "par" * string(position) : innername
    return Parameter(Int(position), String(innername), String(default), _parseinputfmt(fmt), required, String(help))
end

"""
    `Parameter(d::Dict)`

generate `Parameter` type from `Dict` type. The `Dict` must contain keys:
`"position"` and optional keys `"innername"`, `"abbr"`, `"default"`, `"format"`, `"required"` and `"help"`
"""
function Parameter(d::Base.Dict)
    ks = keys(d)
    pos = d["position"]
    inner = ("innername" in ks) ? d["innername"] : ""
    dft = ("default" in ks) ? d["default"] : ""
    fmt = ("format" in ks) ? d["format"] : ""
    rqd = ("required" in ks) ? d["required"] : false
    hp = ("help" in ks) ? d["help"] : ""
    return Parameter(pos; innername=inner, default=dft, fmt=fmt, required=rqd, help=hp)
end

function Dict(p::Parameter)
    fmt = map(p.parsefmt) do v
              if typeof(v) <: Delimiter
                  t = replace(v.string, "%" => "%%")
              else
                  t = v.string
              end
              t
          end |> join |> String
    return Base.Dict("position" => p.position, "innername" => p.innername,
                "default" => p.default, "format" => fmt, "required" => p.required, "help" => p.help)
end

"""
    Group

A group of `Option` and `Parameter`.

  - `name` name of the Group
  - `flgs` collect of `Flag`
  - `opts` collect of `Option`
  - `pars` collect of `Parameter`
"""
struct Group
    name::String
    flgs::Vector{Flag}
    opts::Vector{Option}
    pars::Vector{Parameter}
end

"""
```julia
Group(name::AbstractString; flags::Vector{Flag}=Flag[], opts::Vector{Option}=Option[],
      pars::Vector{Parameter}=Parameter[])
```
"""
Group(name::AbstractString;
flags::Vector{Flag}=Flag[],
opts::Vector{Option}=Option[],
pars::Vector{Parameter}=Parameter[]) = Group(String(name), flags, opts, pars)

"""
    `Group(d::Dict)`

generate `Group` type from `Dict` type. The `Dict` must contain keys:
`"name"`, `"flags"`, `"opts"` and `"pars"`
"""
function Group(d::Base.Dict)
    return Group(d["name"], Flag.(d["flags"]), Option.(d["opts"]), Parameter.(d["pars"]))
end

function Dict(g::Group)
    return Base.Dict("name" => g.name, "flags" => Dict.(g.flgs), "opts" => Dict.(g.opts), "pars" => Dict.(g.pars))
end

function _checkuniqueness(var::AbstractVector, msg)
    flag_conflict = false
    L = length(var)
    t1 = 0
    t2 = 0
    for i = 1:L-1
        for j = i+1:L
            flag_conflict |= (var[i] == var[j])
            if flag_conflict
                t1 = i
                t2 = j
                break
            end
        end
        if flag_conflict
            break
        end
    end
    if flag_conflict
        error("More than one `$(var[t1])` exist in $(msg) list.")
    end
    return nothing
end

"""
    checksetting(grps::Vector{Group})

check if there are conflicts between parameter settings
"""
function checksetting(grps::Vector{Group})
    flgs = Flag[]
    opts = Option[]
    pars = Parameter[]
    for g in grps
        append!(flgs, g.flgs)
        append!(opts, g.opts)
        append!(pars, g.pars)
    end
    flg_outernames = map(v -> v.outername, flgs)
    flg_innernames = map(v -> v.innername, flgs)
    flg_abbr = map(v -> v.abbreviation, flgs)
    opt_outernames = map(v -> v.outername, opts)
    opt_innernames = map(v -> v.innername, opts)
    opt_abbr = map(v -> v.abbreviation, opts)
    par_position = map(v -> v.position, pars)
    par_default = map(v -> !isempty(v.default), pars)
    par_innernames = map(v -> v.innername, pars)
    all_outernames = [flg_outernames; opt_outernames]
    all_innernames = [flg_innernames; opt_innernames; par_innernames]
    all_abbr = [flg_abbr; opt_abbr]
    # check uniqueness
    _checkuniqueness(all_outernames, "outername")
    _checkuniqueness(all_innernames, "innername")
    _checkuniqueness(all_abbr, "abbreviation")
    # check the location of parameter
    for i = 1:maximum(par_position)
        if !(i in par_position)
            @warn "the $(i)th parameter is not used"
        end
    end
    # check default value settings of parameter
    par_range = sortperm(par_position)
    for i = eachindex(par_range)
        if par_default[par_range[i]] && all(par_default[par_range[i:end]])
            continue
        else
            error("the parameters after $(i)th parameter need default value")
        end
    end
    # check help, usage and h option
    if "help" in all_outernames
        error("help option has special meaning, and can't be used as outername")
    end
    if "help" in all_innernames
        error("help option has special meaning, and can't be used as innername")
    end
    if "usage" in all_outernames
        error("usage option has special meaning, and can't be used as outername")
    end
    if "usage" in all_innernames
        error("usage option has special meaning, and can't be used as innername")
    end
    if "h" in all_abbr
        error("h has special meaning, and can't be used as abbreviation")
    end
    return nothing
end

function _is_help_flag_exist(str::AbstractString)
    t = split(str, ' '; keepempty=false)
    return any(l->(l in ("--help", "--usage", "-h")), t)
end

"""
    `checksetting(grp::Group)`
"""
checksetting(grp::Group) = checksetting([grp])

"""
    parse string to defined data structure. If there is `--help`, `--usage` or `-h` in commandline,
    the program will print help document and exit.

```julia
parse(cmdstr::String, grp::Group)
```
"""
function parse(cmdstr::String, grp::Group)
    if _is_help_flag_exist(cmdstr)
        printhelp(grp)
        exit(0)
    end
    chars = collect(cmdstr)
    unused = trues(length(cmdstr))
    argpairs = Pair{Symbol,Any}[]
    for flag in grp.flgs
        vloc1 = findall("--" * flag.outername, cmdstr)
        vloc2 = isempty(flag.abbreviation) ? UnitRange{Int64}[] : findall("-" * flag.abbreviation, cmdstr)
        vloc = [vloc1; vloc2]
        if isempty(vloc)
            push!(argpairs, Symbol(flag.innername) => false)
        else
            push!(argpairs, Symbol(flag.innername) => true)
            for r in vloc
                unused[r] .= false
            end
        end
    end
    for opt in grp.opts
        vloc1 = findall("--" * opt.outername, cmdstr)
        vloc2 = isempty(opt.abbreviation) ? UnitRange{Int64}[] : findall("-" * opt.abbreviation, cmdstr)
        vloc = [vloc1; vloc2]
        if length(vloc) > 1
            error("more than one values are assigned to $(opt.outername).")
        elseif isempty(vloc)
            if opt.required
                @error("variable $(opt.outername) must be specified.")
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
        push!(argpairs, Symbol(opt.innername) => v)
    end
    for i = 1:length(chars)-1
        if chars[i] == ' ' && chars[i+1] == ' ' && unused[i] && unused[i+1]
            unused[i] = false
        end
    end
    parstr = String(chars[unused]) |> strip |> split .|> String
    @debug "parse $(parstr) to parameters"
    parloc = [p.position for p in grp.pars]
    parperm = sortperm(parloc)
    for ip in parperm
        par = grp.pars[ip]
        if par.position > length(parstr) && par.required
            @error("parameter on location $(par.position) must be specified.")
        elseif par.position > length(parstr) && !par.required
            if isempty(par.default)
                v = nothing
            else
                (v, _) = _parse(par.default, par.parsefmt)
            end
        else
            (v, _) = _parse(parstr[par.position], par.parsefmt)
        end
        push!(argpairs, Symbol(par.innername) => v)
    end
    return NamedTuple(argpairs)
end

"""
```julia
parse(cmdstr::AbstractString, grps::Vector{Group})
```
"""
function parse(cmdstr::AbstractString, grps::Vector{Group})
    if _is_help_flag_exist(cmdstr)
        printhelp(grps)
        exit(0)
    end
    ps = Pair{Symbol,NamedTuple}[]
    for g in grps
        v = parse(cmdstr, g)
        push!(ps, Symbol(g.name) => v)
    end
    return NamedTuple(ps)
end

"""
```julia
parse(line::Vector{<:AbstractString}, g::Group)
```
"""
parse(line::Vector{<:AbstractString}, g::Group) = parse(join(line, ' '), g)

"""
```julia
parse(line::Vector{<:AbstractString}, g::Vector{Group})
```
"""
parse(line::Vector{<:AbstractString}, g::Vector{Group}) = parse(join(line, ' '), g)

"""
    generate help string from defined groups

```julia
helpstr(groups::Vector{Group})
```

return value:

```julia
(usage_line::String, example_line::String, varname_list::Vector{String},
 abbreviation_list::Vector{String}, helps::Vector{String})
```
"""
function helpstr(groups::Vector{Group})
    arg_varname = String[]
    arg_usage_line = String[]
    arg_detail_var = String[]
    arg_detail_abbr = String[]
    arg_detail_doc = String[]
    arg_example_line = String[]
    for g in groups
        for flag in g.flgs
            p1 = "--" * flag.outername
            p2 = isempty(flag.abbreviation) ? "" : "-" * flag.abbreviation
            push!(arg_varname, flag.outername)
            push!(arg_usage_line, "[" * p1 * (isempty(p2) ? "" : "|") * p2 * "]")
            push!(arg_detail_var, p1)
            push!(arg_detail_abbr, p2)
            push!(arg_detail_doc, flag.help)
            push!(arg_example_line, p1)
        end
        for opt in g.opts
            p1 = "--" * opt.outername
            p2 = isempty(opt.abbreviation) ? "" : "-" * opt.abbreviation
            emp = map(opt.parsefmt) do v
                if typeof(v) <: Delimiter
                    s = v.string
                else
                    s = vartype(v)
                end
                s
            end |> join
            push!(arg_varname, opt.outername)
            push!(arg_usage_line,
                  (opt.required ? "" : "[") * p1 * emp * (isempty(p2) ? "" : "|") * p2 * emp *
                  (opt.required ? "" : "]"))
            push!(arg_detail_var, p1)
            push!(arg_detail_abbr, p2)
            push!(arg_detail_doc, opt.help)
            push!(arg_example_line, p1 * (isempty(opt.default) ? example(opt.parsefmt) : opt.default))
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
    for g in groups
        for par in g.pars
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
        emp = map(par.parsefmt) do v
            if typeof(v) <: Delimiter
                s = v.string
            else
                s = vartype(v)
            end
            s
        end |> join
        push!(par_usage_line, (par.required ? "" : "[") * par.innername * emp * (par.required ? "" : "]"))
        push!(par_detail_var, par.innername)
        push!(par_detail_doc, par.help)
        push!(par_example_line, isempty(par.default) ? example(par.parsefmt) : par.default)
    end
    return (join([arg_usage_line; par_usage_line], ' '),
            join([arg_example_line; par_example_line], ' '),
            vcat(arg_detail_var, par_detail_var),
            arg_detail_abbr,
            vcat(arg_detail_doc, par_detail_doc))
end

function splitbymargin(str::AbstractString, ruler::Int)
    words = split(str; keepempty=false)
    lines = String[""]
    for i = eachindex(words)
        if length(lines[end]) + 1 + length(words[i]) <= ruler
            lines[end] *= (isempty(lines[end]) ? "" : " ") * words[i]
        else
            push!(lines, words[i])
        end
    end
    return lines
end

"""
    print help doc of defined data structure

```julia
printhelp(groups::Vector{Group}; programname::AbstractString="", indent::Int=4,
          maxabbrcol::Int=5, maxvarcol::Int=10, maxdoccol::Int=60)
```
"""
function printhelp(groups::Vector{Group}, programname::AbstractString=""; indent::Int=4,
                   maxabbrcol::Int=5, maxvarcol::Int=30, maxdoccol::Int=60)
    if isempty(programname)
        fn = splitdir(PROGRAM_FILE)
        programname = fn[2]
    end
    (usagestr, examplestr, varlist, argabbr, docs) = helpstr(groups)
    println("Usage: ", programname, " ", usagestr)
    println("\nExample:\n", " "^indent, programname, " ", examplestr)
    varl = min(maximum(length.(varlist)), maxvarcol)
    abbrl = min(maximum(length.(argabbr)), maxabbrcol)
    L = length(argabbr)
    println("\nArgument:\n")
    for i = eachindex(varlist)
        print(" "^indent)
        if i <= L && !isempty(argabbr[i])
            print(argabbr[i], ',')
            if length(argabbr[i]) + 1 < abbrl
                print(" "^(abbrl - 1 - length(argabbr[i])))
            end
            print(" ")
        else
            print(" "^(abbrl + 2))
        end
        print(varlist[i])
        if length(varlist[i]) - 1 > varl
            print('\n', " "^(indent + varl + abbrl))
        else
            print(" "^(varl - length(varlist[i]) + 1))
        end
        if length(docs[i]) < maxdoccol
            println(docs[i])
        else
            hl = splitbymargin(docs[i], maxdoccol)
            println(hl[1])
            if length(hl) > 1
                for i = eachindex(hl)
                    if i == 1
                        continue
                    end
                    println(" "^(indent + varl + abbrl), hl[i])
                end
            end
        end
    end
    return nothing
end

"""
```julia
printhelp(group::Group; programname::AbstractString="", indent::Int=4,
          maxabbrcol::Int=5, maxvarcol::Int=30, maxdoccol::Int=60)
```
"""
printhelp(group::Group, programname::AbstractString=""; indent::Int=4,
maxabbrcol::Int=5, maxvarcol::Int=30, maxdoccol::Int=60) = printhelp([group],
                                                                     programname; indent=indent, maxabbrcol=maxabbrcol,
                                                                     maxvarcol=maxvarcol, maxdoccol=maxdoccol)

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
addflag!(innername::AbstractString; outername::AbstractString="",
         abbr::AbstractString="", help::AbstractString="")
```
"""
addflag!(innername::AbstractString; outername::AbstractString="",
abbr::AbstractString="", help::AbstractString="") = addflag!(Flag(innername; outername=outername, abbr=abbr, help=help))

"""
```julia
addopt!(innername::AbstractString; outername::AbstractString="", abbr::AbstractString="",
        default::AbstractString="", fmt::AbstractString="%s", required::Bool=false, help::AbstractString="")
```
"""
addopt!(innername::AbstractString;
outername::AbstractString="",
abbr::AbstractString="",
default::AbstractString="",
fmt::AbstractString="%s",
required::Bool=false,
help::AbstractString="") = addopt!(Option(innername; outername=outername, abbr=abbr, default=default, fmt=fmt,
                                          required=required, help=help))

"""
```julia
addpar!(position::Int; innername::AbstractString="", default::AbstractString="", fmt::AbstractString="%s",
        required::Bool=false, help::AbstractString="")
```
"""
function addpar!(position::Int=0;
                 innername::AbstractString="",
                 default::AbstractString="",
                 fmt::AbstractString="%s",
                 required::Bool=false,
                 help::AbstractString="")
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
parse(line::AbstractString) = parse(String(line), Group("UNKNOWN", INNER_FLAG, INNER_OPTION, INNER_PARAMETER))

"""
    `parse(lines::Vector{<:AbstractString})`

parse commandline input according to inner buffer
"""
parse(lines::Vector{<:AbstractString}) = parse(join(lines, ' '))

function printhelp(programname::AbstractString=""; indent::Int=4,
                   maxabbrcol::Int=5, maxvarcol::Int=30, maxdoccol::Int=60)
    printhelp([Group("UNKNOWN", INNER_FLAG, INNER_OPTION, INNER_PARAMETER)],
              programname; indent=indent, maxabbrcol=maxabbrcol, maxvarcol=maxvarcol, maxdoccol=maxdoccol)
end

end
