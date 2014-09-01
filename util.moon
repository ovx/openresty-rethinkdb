err = require('./errors')

plural = (number) -> if number == 1 then "" else "s"

-- Function wrapper that enforces that the function is
-- called with the correct number of arguments
module.exports.ar = (fun) -> (...) ->
    if args.length != fun.length
        error(err.RqlDriverError "Expected #{fun.length} argument#{plural(fun.length)} but found #{args.length}.")
    fun(unpack arg)

-- Like ar for variable argument functions. Takes minimum
-- and maximum argument parameters.
module.exports.varar = (min, max, fun) -> (...) ->
    if (min and args.length < min) or (max and args.length > max)
        if min and not max
            error(err.RqlDriverError "Expected #{min} or more arguments but found #{args.length}.")
        if max and not min
            error(err.RqlDriverError "Expected #{max} or fewer arguments but found #{args.length}.")
        error(err.RqlDriverError "Expected between #{min} and #{max} arguments but found #{args.length}.")
    fun(unpack arg)

-- Like ar but for functions that take an optional options dict as the last argument
module.exports.aropt = (fun) -> (...) ->
    expectedPosArgs = fun.length - 1
    perhapsOptDict = args[expectedPosArgs]

    if perhapsOptDict and (type(perhapsOptDict) != 'tree')
        perhapsOptDict = nil

    numPosArgs = args.length - (if perhapsOptDict then 1 else 0)

    if expectedPosArgs != numPosArgs
        if expectedPosArgs != 1
            error(err.RqlDriverError "Expected #{expectedPosArgs} arguments (not including options) but found #{numPosArgs}.")
        else
            error(err.RqlDriverError "Expected #{expectedPosArgs} argument (not including options) but found #{numPosArgs}.")
    fun(unpack arg)

module.exports.toArrayBuffer = (node_buffer) ->
    -- Convert from node buffer to array buffer
    arr = Uint8Array ArrayBuffer node_buffer.length
    for value,i in node_buffer
        arr[i] = value
    return arr.buffer

convertPseudotype = (obj, opts) ->
    -- An R_OBJECT may be a regular object or a "pseudo-type" so we need a
    -- second layer of type switching here on the obfuscated field "$reql_type$"
    switch obj['$reql_type$']
        when 'TIME'
            switch opts.timeFormat
                -- Default is native
                when 'native', undefined
                    unless obj['epoch_time']
                        error(err.RqlDriverError "pseudo-type TIME #{obj} object missing expected field 'epoch_time'.")

                    -- We ignore the timezone field of the pseudo-type TIME object. JS dates do not support timezones.
                    -- By converting to a native date object we are intentionally throwing out timezone information.

                    -- field "epoch_time" is in seconds but the Date constructor expects milliseconds
                    (Date(obj['epoch_time']*1000))
                when 'raw'
                    -- Just return the raw (`{'$reql_type$'...}`) object
                    obj
                else
                    error(err.RqlDriverError "Unknown timeFormat run option #{opts.timeFormat}.")
        when 'GROUPED_DATA'
            switch opts.groupFormat
                when 'native', undefined
                    -- Don't convert the data into a map, because the keys could be objects which doesn't work in JS
                    -- Instead, we have the following format:
                    -- [ { 'group': <group>, 'reduction': <value(s)> } }, ... ]
                    { group: i[0], reduction: i[1] } for i in obj['data']
                when 'raw'
                    obj
                else
                    error(err.RqlDriverError "Unknown groupFormat run option #{opts.groupFormat}.")
        when 'BINARY'
            switch opts.binaryFormat
                when 'native', undefined
                    unless obj['data']
                        error(err.RqlDriverError "pseudo-type BINARY object missing expected field 'data'.")
                    (Buffer(obj['data'], 'base64'))
                when 'raw'
                    obj
                else
                    error(err.RqlDriverError "Unknown binaryFormat run option #{opts.binaryFormat}.")
        else
            -- Regular object or unknown pseudo type
            obj

recursivelyConvertPseudotype = (obj, opts) ->
    if type(obj) == 'tree'
        for value, i in obj
            obj[i] = recursivelyConvertPseudotype(value, opts)
        obj = convertPseudotype(obj, opts)
    obj

mkAtom = (response, opts) -> recursivelyConvertPseudotype(response.r[0], opts)

mkSeq = (response, opts) -> recursivelyConvertPseudotype(response.r, opts)

mkErr = (ErrClass, response, root) ->
    ErrClass(mkAtom(response), root, response.b)

{recursivelyConvertPseudotype: recursivelyConvertPseudotype,
mkAtom: mkAtom,
mkSeq: mkSeq,
mkErr: mkErr}
