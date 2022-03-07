function disocciate_array(array)
  a = {}

  for k, v in pairs(array) do
    table.insert(a, v)
  end

  return a
end

data = load(io.open(arg[1], 'r'):read('*all'))()

local cjson = require 'cjson'.new()

if type(data[pairs(data)(data)]) == 'table' then
  data = disocciate_array(data)
else
  cjson.encode_sparse_array(true)
end

io.open(arg[2], 'w'):write(cjson.encode(data))
