-- lua/zfogg/util.lua

-- INFO: https://github.com/nanotee/nvim-lua-guide#tips-3
function _G.put(...)
  local objects = {}
  for i = 1, select('#', ...) do
    local v = select(i, ...)
    table.insert(objects, vim.inspect(v))
  end

  print(table.concat(objects, '\n'))
  return ...
end

function _G.PHas(plugin)
  local pp = _G.packer_plugins[plugin]
  return pp and pp.loaded
end

function _G.PExe(exe)
  local _pexe = function()
    return vim.fn.executable(exe)
  end
  return _pexe
end

function _G.ccat(a, b)
  local ab = {}
  table.foreach(a, function(k, v) table.insert(ab, v) end)
  table.foreach(b, function(k, v) table.insert(ab, v) end)
  return ab
end
