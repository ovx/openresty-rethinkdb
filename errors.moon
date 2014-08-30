class RqlDriverError
    constructor: (msg) ->
        @name = @constructor.name
        @msg = msg
        @message = msg

class RqlServerError
    constructor: (msg, term, frames) ->
        @name = @constructor.name
        @msg = msg
        @frames = frames
        if term
            if msg[msg.length-1] == '.'
                @message = "#{msg.slice(0, msg.length-1)} in:\n#{RqlQueryPrinter::printQuery(term)}\n#{RqlQueryPrinter::printCarrots(term, frames)}"
            else
                @message = "#{msg} in:\n#{RqlQueryPrinter::printQuery(term)}\n#{RqlQueryPrinter::printCarrots(term, frames)}"
        else
            @message = "#{msg}"

class RqlRuntimeError extends RqlServerError

class RqlCompileError extends RqlServerError

class RqlClientError extends RqlServerError

class RqlQueryPrinter
    printQuery: (term) ->
        tree = composeTerm(term)
        joinTree tree

    composeTerm = (term) ->
        args = [composeTerm arg for arg in term.args]
        optargs = {}
        for key,arg in term.optargs
            optargs[key] = composeTerm(arg)
        term.compose(args, optargs)

    printCarrots: (term, frames) ->
        if frames.length == 0
            tree = {carrotify(composeTerm(term))}
        else
            tree = composeCarrots(term, frames)
        joinTree(tree).gsub('[^\^]', ' ')

    composeCarrots = (term, frames) ->
        frame = frames.shift()

        args = for arg,i in term.args
                    if frame == i
                        composeCarrots(arg, frames)
                    else
                        composeTerm(arg)

        optargs = {}
        for key,arg in term.optargs
            if frame == key
                optargs[key] = composeCarrots(arg, frames)
            else
                optargs[key] = composeTerm(arg)

        if frame
            term.compose(args, optargs)
        else
            carrotify(term.compose(args, optargs))

    carrotMarker = {}

    carrotify = (tree) -> {carrotMarker, tree}

    joinTree = (tree) ->
        str = ''
        for term in tree
            if Array.isArray term
                if term.length == 2 and term[0] == carrotMarker
                    str += joinTree(term[1]).gsub('.', '^')
                else
                    str += joinTree term
            else
                str += term
        return str

{RqlDriverError: RqlDriverError
RqlRuntimeError: RqlRuntimeError
RqlCompileError: RqlCompileError
RqlClientError: RqlClientError
printQuery: RqlQueryPrinter.printQuery}
