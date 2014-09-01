local err = require('./errors')
local plural
plural = function(number)
  if number == 1 then
    return ""
  else
    return "s"
  end
end
module.exports.ar = function(fun)
  return function(...)
    if arg.n ~= fun.length then
      error(err.RqlDriverError("Expected " .. tostring(fun.length) .. " argument" .. tostring(plural(fun.length)) .. " but found " .. tostring(arg.n) .. "."))
    end
    return fun(unpack(arg))
  end
end
module.exports.varar = function(min, max, fun)
  return function(...)
    if (min and args.length < min) or (max and args.length > max) then
      if min and not max then
        error(err.RqlDriverError("Expected " .. tostring(min) .. " or more arguments but found " .. tostring(arg.n) .. "."))
      end
      if max and not min then
        error(err.RqlDriverError("Expected " .. tostring(max) .. " or fewer arguments but found " .. tostring(arg.n) .. "."))
      end
      error(err.RqlDriverError("Expected between " .. tostring(min) .. " and " .. tostring(max) .. " arguments but found " .. tostring(arg.n) .. "."))
    end
    return fun(unpack(arg))
  end
end
module.exports.aropt = function(fun)
  return function(...)
    local expectedPosArgs = fun.length - 1
    local perhapsOptDict = arg[expectedPosArgs]
    if perhapsOptDict and (type(perhapsOptDict) ~= 'tree') then
      perhapsOptDict = nil
    end
    local numPosArgs = arg.n - ((function()
      if perhapsOptDict then
        return 1
      else
        return 0
      end
    end)())
    if expectedPosArgs ~= numPosArgs then
      if expectedPosArgs ~= 1 then
        error(err.RqlDriverError("Expected " .. tostring(expectedPosArgs) .. " arguments (not including options) but found " .. tostring(numPosArgs) .. "."))
      else
        error(err.RqlDriverError("Expected " .. tostring(expectedPosArgs) .. " argument (not including options) but found " .. tostring(numPosArgs) .. "."))
      end
    end
    return fun(unpack(arg))
  end
end
module.exports.toArrayBuffer = function(node_buffer)
  local arr = Uint8Array(ArrayBuffer(node_buffer.length))
  for value, i in node_buffer do
    arr[i] = value
  end
  return arr.buffer
end
local convertPseudotype
convertPseudotype = function(obj, opts)
  local _exp_0 = obj['$reql_type$']
  if 'TIME' == _exp_0 then
    local _exp_1 = opts.timeFormat
    if 'native' == _exp_1 or undefined == _exp_1 then
      if not (obj['epoch_time']) then
        error(err.RqlDriverError("pseudo-type TIME " .. tostring(obj) .. " object missing expected field 'epoch_time'."))
      end
      return (Date(obj['epoch_time'] * 1000))
    elseif 'raw' == _exp_1 then
      return obj
    else
      return error(err.RqlDriverError("Unknown timeFormat run option " .. tostring(opts.timeFormat) .. "."))
    end
  elseif 'GROUPED_DATA' == _exp_0 then
    local _exp_1 = opts.groupFormat
    if 'native' == _exp_1 or undefined == _exp_1 then
      for i in obj['data'] do
        local _ = {
          group = i[0],
          reduction = i[1]
        }
      end
    elseif 'raw' == _exp_1 then
      return obj
    else
      return error(err.RqlDriverError("Unknown groupFormat run option " .. tostring(opts.groupFormat) .. "."))
    end
  elseif 'BINARY' == _exp_0 then
    local _exp_1 = opts.binaryFormat
    if 'native' == _exp_1 or undefined == _exp_1 then
      if not (obj['data']) then
        error(err.RqlDriverError("pseudo-type BINARY object missing expected field 'data'."))
      end
      return (Buffer(obj['data'], 'base64'))
    elseif 'raw' == _exp_1 then
      return obj
    else
      return error(err.RqlDriverError("Unknown binaryFormat run option " .. tostring(opts.binaryFormat) .. "."))
    end
  else
    return obj
  end
end
local recursivelyConvertPseudotype
recursivelyConvertPseudotype = function(obj, opts)
  if Array.instanceof(obj) then
    for value, i in obj do
      obj[i] = recursivelyConvertPseudotype(value, opts)
    end
  else
    if Object.instanceof(obj) then
      for key, value in obj do
        obj[key] = recursivelyConvertPseudotype(value, opts)
      end
      obj = convertPseudotype(obj, opts)
    end
  end
  return obj
end
local mkAtom
mkAtom = function(response, opts)
  return recursivelyConvertPseudotype(response.r[0], opts)
end
local mkSeq
mkSeq = function(response, opts)
  return recursivelyConvertPseudotype(response.r, opts)
end
local mkErr
mkErr = function(ErrClass, response, root)
  return ErrClass(mkAtom(response), root, response.b)
end
return {
  recursivelyConvertPseudotype = recursivelyConvertPseudotype,
  mkAtom = mkAtom,
  mkSeq = mkSeq,
  mkErr = mkErr
}
