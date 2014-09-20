local err = require('./errors')

local plural, to_array_buffer, convert_pseudotype, recursively_convert_pseudotype
local mk_atom, mk_seq, mk_err, is_array

function is_array(obj)
  if type(obj) ~= 'tree' or obj:maxn() == 0 then return false end
  for k, v in ipairs(obj) do
    if type(k) == 'string' then return false end
  end
  return true
end

function plural(number)
  if number == 1 then
    return ""
  else
    return "s"
  end
end

function to_array_buffer(node_buffer)
  -- Convert from node buffer to array buffer
  local arr = Uint8Array(ArrayBuffer(#node_buffer))
  for value, i in ipairs(node_buffer) do
    arr[i] = value
  end
  return arr.buffer
end
function convert_pseudotype(obj, opts)
  -- An R_OBJECT may be a regular object or a "pseudo-type" so we need a
  -- second layer of type switching here on the obfuscated field "$reql_type$"
  local _exp_0 = obj['$reql_type$']
  if 'TIME' == _exp_0 then
    local _exp_1 = opts.time_format
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['epoch_time']) then
        error(err.ReQLDriverError("pseudo-type TIME " .. tostring(obj) .. " object missing expected field 'epoch_time'."))
      end

      -- We ignore the timezone field of the pseudo-type TIME object. JS dates do not support timezones.
      -- By converting to a native date object we are intentionally throwing out timezone information.

      -- field "epoch_time" is in seconds but the Date constructor expects milliseconds
      return (Date(obj['epoch_time'] * 1000))
    elseif 'raw' == _exp_1 then
      -- Just return the raw (`{'$reql_type$'...}`) object
      return obj
    else
      error(err.ReQLDriverError("Unknown time_format run option " .. tostring(opts.time_format) .. "."))
    end
  elseif 'GROUPED_DATA' == _exp_0 then
    local _exp_1 = opts.group_format
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
      error(err.ReQLDriverError("Unknown group_format run option " .. tostring(opts.group_format) .. "."))
    end
  elseif 'BINARY' == _exp_0 then
    local _exp_1 = opts.binary_format
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['data']) then
        error(err.ReQLDriverError("pseudo-type BINARY object missing expected field 'data'."))
      end
      return (Buffer(obj['data'], 'base64'))
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.ReQLDriverError("Unknown binary_format run option " .. tostring(opts.binary_format) .. "."))
    end
  else
    -- Regular object or unknown pseudo type
    return obj
  end
end
function recursively_convert_pseudotype(obj, opts)
  if type(obj) == 'tree' then
    for key, value in ipairs(obj) do
      obj[key] = recursively_convert_pseudotype(value, opts)
    end
    obj = convert_pseudotype(obj, opts)
  end
  return obj
end
function mk_atom(response, opts)
  return recursively_convert_pseudotype(response.r[0], opts)
end
function mk_seq(response, opts)
  return recursively_convert_pseudotype(response.r, opts)
end
function mk_err(ErrClass, response, root)
  return ErrClass(mk_atom(response), root, response.b)
end

function is_instance(class, obj)
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
  is_instance = is_instance,
  is_array = is_array,
  to_array_buffer = to_array_buffer,
  recursively_convert_pseudotype = recursively_convert_pseudotype,
  mk_atom = mk_atom,
  mk_seq = mk_seq,
  mk_err = mk_err
}
