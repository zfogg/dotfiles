-- lua/zfogg/util.lua
local cmd = vim.cmd
local o_s = vim.o
local map_key = vim.api.nvim_set_keymap

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

function _G.vimreload()
  for k,_ in pairs(package.loaded) do
    print(k)
    local p = nil
    if     string.match(k, "^zfogg.") then p = k
    elseif string.match(k, "^rc.")    then p = k end
    if p then
      print(p)
      package.loaded[k] = nil
    end
  end
end

function _G.dump(o)
  if type(o) == 'table' then
    local s = '{ '
    for k,v in pairs(o) do
      if type(k) ~= 'number' then k = '"'..k..'"' end
      s = s .. '['..k..'] = ' .. dump(v) .. ','
    end
    return s .. '} '
  else
    return tostring(o)
  end
end

function _G.pp(o)
  --print(_G.dump(o))
  print(vim.inspect(o))
end

local function opt(o, v, scopes)
  scopes = scopes or {o_s}
  for _, s in ipairs(scopes) do s[o] = v end
end

local function autocmd(group, cmds, clear)
  clear = clear == nil and false or clear
  if type(cmds) == 'string' then cmds = {cmds} end
  cmd('augroup ' .. group)
  if clear then cmd [[au!]] end
  for _, c in ipairs(cmds) do cmd('autocmd ' .. c) end
  cmd [[augroup END]]
end

local function map(modes, lhs, rhs, opts)
  opts = opts or {}
  opts.noremap = opts.noremap == nil and true or opts.noremap
  if type(modes) == 'string' then modes = {modes} end
  for _, mode in ipairs(modes) do map_key(mode, lhs, rhs, opts) end
end

local function tmerge(t1, t2)
  for k,v in pairs(t2) do
    if type(v) == "table" then
      if type(t1[k] or false) == "table" then
        tableMerge(t1[k] or {}, t2[k] or {})
      else
        t1[k] = v
      end
    else
      t1[k] = v
    end
  end
  return t1
end

local function extend(t1, t2)
  return table.move(t2, 1, #t2, #t1 + 1, t1)
end


return {
  opt = opt,
  autocmd = autocmd,
  map = map,
  tmerge = tmerge,
  extend = extend,
}
