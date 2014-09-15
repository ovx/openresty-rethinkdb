local err = require('./errors')

local plural, toArrayBuffer, convertPseudotype, recursivelyConvertPseudotype
local mkAtom, mkSeq, mkErr

function plural(number)
  if number == 1 then
    return ""
  else
    return "s"
  end
end

function toArrayBuffer(node_buffer)
  -- Convert from node buffer to array buffer
  local arr = Uint8Array(ArrayBuffer(node_buffer.length))
  for value, i in ipairs(node_buffer) do
    arr[i] = value
  end
  return arr.buffer
end
function convertPseudotype(obj, opts)
  -- An R_OBJECT may be a regular object or a "pseudo-type" so we need a
  -- second layer of type switching here on the obfuscated field "$reql_type$"
  local _exp_0 = obj['$reql_type$']
  if 'TIME' == _exp_0 then
    local _exp_1 = opts.timeFormat
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['epoch_time']) then
        error(err.RqlDriverError("pseudo-type TIME " .. tostring(obj) .. " object missing expected field 'epoch_time'."))
      end

      -- We ignore the timezone field of the pseudo-type TIME object. JS dates do not support timezones.
      -- By converting to a native date object we are intentionally throwing out timezone information.

      -- field "epoch_time" is in seconds but the Date constructor expects milliseconds
      return (Date(obj['epoch_time'] * 1000))
    elseif 'raw' == _exp_1 then
      -- Just return the raw (`{'$reql_type$'...}`) object
      return obj
    else
      error(err.RqlDriverError("Unknown timeFormat run option " .. tostring(opts.timeFormat) .. "."))
    end
  elseif 'GROUPED_DATA' == _exp_0 then
    local _exp_1 = opts.groupFormat
    if 'native' == _exp_1 or not _exp_1 then
      -- Don't convert the data into a map, because the keys could be objects which doesn't work in JS
      -- Instead, we have the following format:
      -- [ { 'group': <group>, 'reduction': <value(s)> } }, ... ]
      res = {}
      j = 1
      for i, v in ipairs(obj['data']) do
        res[j] = {
          group = i,
          reduction = v
        }
        j = j + 1
      end
      obj = res
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.RqlDriverError("Unknown groupFormat run option " .. tostring(opts.groupFormat) .. "."))
    end
  elseif 'BINARY' == _exp_0 then
    local _exp_1 = opts.binaryFormat
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['data']) then
        error(err.RqlDriverError("pseudo-type BINARY object missing expected field 'data'."))
      end
      return (Buffer(obj['data'], 'base64'))
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.RqlDriverError("Unknown binaryFormat run option " .. tostring(opts.binaryFormat) .. "."))
    end
  else
    -- Regular object or unknown pseudo type
    return obj
  end
end
function recursivelyConvertPseudotype(obj, opts)
  if type(obj) == 'tree' then
    for key, value in ipairs(obj) do
      obj[key] = recursivelyConvertPseudotype(value, opts)
    end
    obj = convertPseudotype(obj, opts)
  end
  return obj
end
function mkAtom(response, opts)
  return recursivelyConvertPseudotype(response.r[0], opts)
end
function mkSeq(response, opts)
  return recursivelyConvertPseudotype(response.r, opts)
end
function mkErr(ErrClass, response, root)
  return ErrClass(mkAtom(response), root, response.b)
end

function isinstance(class, obj)
  if not obj then return false end
  local obj_cls = obj.__class
  while obj_cls do
    if obj_cls == class then
      return true
    end
    obj_cls = obj_cls.__parent
  end
  return false
end

return {
  isinstance = isinstance,
  toArrayBuffer = toArrayBuffer,
  recursivelyConvertPseudotype = recursivelyConvertPseudotype,
  mkAtom = mkAtom,
  mkSeq = mkSeq,
  mkErr = mkErr
}
