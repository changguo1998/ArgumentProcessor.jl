module ArgumentProcessor

import Base.parse
export argument, parameter, layer, printhelp

struct Argument
    name::String
    abbreviation::String
    default::Any
    inputformat::String
    errorwhennotexist::Bool
    help::String
    showname::String
end

function argument(name::AbstractString; abbreviation::AbstractString="", default::Any="",
                  inputformat::AbstractString="%s",
                  e::Bool=true, help::AbstractString="", showname::AbstractString="")
    if isempty(name)
        error("Name of argument can't be ignored.")
    end
    if !(inputformat in ("%s", "%f", "%g", "%h", "%o", "%b", "%c", "%d", "%l"))
        error("Invalid type character for " * name)
    end
    return Argument(String(name), String(abbreviation), default, String("%k " * inputformat), e, String(help),
                    String(showname))
end

struct Parameter
    position::Int
    name::String
    inputformat::String
    default::Any
    errorwhennotexist::Bool
    help::String
    showname::String
end

parameter(position::Int; name::AbstractString="", inputformat::AbstractString="%s", default::Any="", e::Bool=true,
help::AbstractString="", showname::AbstractString="") = Parameter(position, String(name), String(inputformat),
                                                                  default, e, help, String(showname))

struct Layer
    name::String
    args::Vector{Argument}
    pars::Vector{Parameter}
end

layer(name::AbstractString, args::Vector{Argument}=Argument[], pars::Vector{Parameter}=Parameter[]) = Layer(String(name),
                                                                                                            args, pars)

function parsefunc(fmt::AbstractString)
    if fmt == "%s"
        return v -> String(v)
    elseif fmt in ("%f", "%g")
        return v -> parse(Float64, v)
    elseif fmt == "%h"
        return v -> parse(Float64, v; base=16)
    elseif fmt == "%o"
        return v -> parse(Float64, v; base=8)
    elseif fmt == "%b"
        return v -> parse(Float64, v; base=2)
    elseif fmt == "%d"
        return v -> parse(Int, v)
    elseif fmt == "%c"
        return v -> parse(ComplexF64, v)
    elseif fmt == "%l"
        return v -> parse(Bool, v)
    else
        error("invalid type character " * fmt)
    end
end

function parse(lines::Vector{T}, arg::Argument) where {T<:AbstractString}
    pf = parsefunc(arg.inputformat[end-1:end])
    pat = "--" * replace(arg.inputformat[1:end-2], "%k" => arg.name)
    i = findfirst(startswith(pat), lines)
    if isnothing(i)
        pat2 = "-" * replace(arg.inputformat[1:end-2], "%k" => arg.abbreviation)
        j = findfirst(startswith(pat2), lines)
        if isnothing(j)
            return (arg.default, 0)
        end
        vs = replace(lines[j], pat2 => "")
        return (pf(vs), j)
    end
    vs = replace(lines[i], pat => "")
    return (pf(vs), i)
end

function parse(lines::Vector{T}, par::Parameter) where {T<:AbstractString}
    if length(lines) < par.position
        return (par.default, 0)
    end
    pf = parsefunc(par.inputformat)
    return (pf(lines[par.position]), par.position)
end

function parse(lines::Vector{T}, layer::Layer) where {T<:AbstractString}
    arglist = String[]
    parlist = String[]
    argloc = Int[]
    parloc = Int[]
    L = length(lines)
    i = 1
    while i <= L
        if startswith(lines[i], '-')
            push!(arglist, lines[i] * " " * lines[i+1])
            push!(argloc, i)
            i += 2
        else
            push!(parlist, lines[i])
            push!(parloc, i)
            i += 1
        end
    end
    flag = falses(L)
    argkey = Symbol[]
    argval = Any[]
    for arg in layer.args
        push!(argkey, Symbol(arg.name))
        (v, i) = parse(arglist, arg)
        push!(argval, v)
        if i > 0
            iflag = argloc[i]
            flag[iflag] = true
            flag[iflag+1] = true
        elseif arg.errorwhennotexist
            error("Argument " * arg.name * " not spicified.")
        end
    end
    parkey = Symbol[]
    parval = Any[]
    for par in layer.pars
        if isempty(par.name)
            push!(parkey, Symbol("par", par.position))
        else
            push!(parkey, Symbol(par.name))
        end
        (v, i) = parse(parlist, par)
        push!(parval, v)
        if i > 0
            iflag = parloc[i]
            flag[iflag] = true
        elseif par.errorwhennotexist
            error("Parameter " * par.name * " not spicified.")
        end
    end
    return ((args=(; zip(argkey, argval)...), pars=(; zip(parkey, parval)...)), findall(flag))
end

function parse(lines::Vector{S}, layer::Vector{Layer}) where {S<:AbstractString}
    tags = Symbol[]
    vals = NamedTuple[]
    cover = Int[]
    for l in layer
        (v, i) = parse(lines, l)
        push!(tags, Symbol(l.name))
        push!(vals, v)
        append!(cover, i)
    end
    nocover = Int[]
    for i = 1:length(lines)
        if i in cover
            continue
        end
        push!(nocover, i)
    end
    return ((; zip(tags, vals)...), nocover)
end

parse(l::AbstractString, layer::Layer) = parse(split(l, ' '; keepempty=false), layer)

function helpstr(layers::Vector{Layer})
    namelen = vcat(map(layers) do l
                       [map(a -> length(a.name) + length(a.abbreviation), l.args); map(p -> length(p.name), l.pars)]
                   end...)
    arg_varname = String[]
    arg_abbra_line = String[]
    arg_detail_var = String[]
    arg_detail_doc = String[]
    for l in layers
        for arg in l.args
            push!(arg_varname, arg.name)
            if isempty(arg.abbreviation)
                s = arg.name
                t = "--" * replace(arg.inputformat[1:end-2], "%k" => arg.name) * arg.showname
            else
                s = arg.name * "," * arg.abbreviation
                t = "--" * replace(arg.inputformat[1:end-2], "%k" => arg.name) * arg.showname * "/" * "-" *
                    replace(arg.inputformat[1:end-2], "%k" => arg.abbreviation) * arg.showname
            end
            if arg.errorwhennotexist
                push!(arg_abbra_line, t)
            else
                push!(arg_abbra_line, "[" * t * "]")
            end
            push!(arg_detail_var, s)
            push!(arg_detail_doc, arg.help)
        end
    end
    argrange = sortperm(arg_varname)
    arg_varname = arg_varname[argrange]
    arg_abbra_line = arg_abbra_line[argrange]
    arg_detail_var = arg_detail_var[argrange]
    arg_detail_doc = arg_detail_doc[argrange]

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
    par_abbra_line = String[]
    par_detail_var = String[]
    par_detail_doc = String[]
    for par in pars
        if par.errorwhennotexist
            push!(par_abbra_line, par.showname)
        else
            push!(par_abbra_line, "[" * par.showname * "]")
        end
        push!(par_detail_var, par.showname)
        push!(par_detail_doc, par.help)
    end
    return (join(arg_abbra_line, ' ') * " " * join(par_abbra_line),
            [arg_detail_var; par_detail_var],
            [arg_detail_doc; par_detail_doc])
end

function printhelp(layers::Vector{Layer}, indent::Int=4)
    (abbr, var, doc) = helpstr(layers)
    println(abbr, "\n")
    varl = maximum(length.(var)) + 2
    for i = 1:length(var)
        println(" "^(indent + varl - length(var[i])), var[i], " ", doc[i], "\n")
    end
end

global INNER_ARGUMENT = Argument[]
global INNER_PARAMETER = Parameter[]

addarg!(a::Argument) = push!(INNER_ARGUMENT, a)
addpar!(p::Parameter) = push!(INNER_PARAMETER, p)
addarg!(name::AbstractString; abbreviation::AbstractString="", default::Any="", inputformat::AbstractString="%s",
e::Bool=true, help::AbstractString="", showname::AbstractString="") = push!(INNER_ARGUMENT,
                                                                            argument(name;
                                                                                     abbreviation=abbreviation,
                                                                                     default=default,
                                                                                     inputformat=inputformat, e=e,
                                                                                     help=help, showname=showname))
addpar!(position::Int; name::AbstractString="", inputformat::AbstractString="%s", default::Any="", e::Bool=true,
help::AbstractString="", showname::AbstractString="") = push!(INNER_PARAMETER,
                                                              parameter(position; name=name,
                                                                        inputformat=inputformat, default=default, e=e,
                                                                        help=help, showname=showname))
function clearinnerbuffer!()
    global INNER_ARGUMENT, INNER_PARAMETER
    INNER_ARGUMENT = Argument[]
    INNER_PARAMETER = Parameter[]
    return nothing
end

function parse(lines::Vector{T}) where {T<:AbstractString}
    return parse(lines, layer("UNKNOWN", INNER_ARGUMENT, INNER_PARAMETER))
end

end