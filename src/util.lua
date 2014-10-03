return {
  class = function(name, parent, base)
    local index, init

    if base == nil then
      base = parent
      parent = nil
    end

    if type(base) == 'function' then
      base = {__init = base}
    end

    if parent and parent.__base then
      setmetatable(base, parent.__base)
    else
      index = base
    end

    init = base.__init
    base.__init = nil
    base.__index = base

    local _class_0 = setmetatable({
      __name = name,
      __init = init,
      __base = base,
      __parent = parent
    }, {
      __index = index or function(cls, name)
        local val = rawget(base, name)
        if val == nil then
          return parent[name]
        else
          return val
        end
      end,
      __call = function(cls, ...)
        local self = setmetatable({}, cls.__base)
        cls.__init(self, ...)
        return self
      end
    })
    base.__class = _class_0

    if parent and parent.__inherited then
      parent.__inherited(parent, _class_0)
    end

    return _class_0
  end,
  is_instance = function(class, obj)
    if type(obj) ~= 'table' then return false end

    local obj_cls = obj.__class
    while obj_cls do
      if obj_cls.__name == class.__name then
        return true
      end
      obj_cls = obj_cls.__parent
    end

    return false
  end
}
