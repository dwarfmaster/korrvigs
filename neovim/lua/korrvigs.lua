
local m = {}

local pickers = require "telescope.pickers"
local finders = require "telescope.finders"
local conf = require("telescope.config").values
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"
local previewers = require "telescope.previewers"
local prevutils = require "telescope.previewers.utils"
local telstate = require "telescope.state"
local ts_utils = require "nvim-treesitter.ts_utils"

function m.setup()
  vim.g.korrvigs_root = os.execute('korr config root')
end

function m.jump_to_note(opts)
  local opts = opts or {}
  pickers.new(opts, {
    prompt_title = "Korr note",
    finder = finders.new_oneshot_job(
      { "korr", "query", "--json", "--kind=note" },
      {
        entry_maker = function(entry)
          local json = vim.json.decode(entry)
          return {
            value = entry,
            display = "[" .. json.name .. "] " .. json.title,
            ordinal = json.name .. json.title,
            path = json.path
          }
        end
      }
    ),
    sorter = conf.generic_sorter(opts),
    previewer = previewers.new_buffer_previewer({
      define_preview = function(self, entry, status)
        conf.buffer_previewer_maker(entry.path, self.state.bufnr, {})
      end,
    }),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        local prompt = 
          string.sub(vim.api.nvim_buf_get_lines(prompt_bufnr, 0, 1, true)[1], 3, -1)
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        if selection then
          vim.cmd.edit(selection.path)
        else
          vim.ui.select(
            { "Create", "Cancel" },
            { prompt = 'Create note "' .. prompt .. '"?' },
            function(choice)
              if choice == 'Create' then
                local hid = io.popen("korr note new \"" .. prompt .. "\"")
                local id = string.gsub(hid:read("*a"), '^%s*(.-)%s*$', '%1')
                hid:close()
                local hinfo = io.popen("korr info --json " .. id)
                local json = vim.json.decode(hinfo:read("*a"))
                hinfo:close()
                vim.cmd.edit(json.path)
              end
            end)
        end
      end)
      return true
    end
  }):find()
end

local function extract_buffer_text(srow,scol,erow,ecol)
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, srow, erow+1, true)
  if srow == erow then
    lines[1] = string.sub(lines[1],scol+1,ecol)
  else
    lines[1] = string.sub(lines[1],scol+1,-1)
    lines[#lines] = string.sub(lines[#lines], 1, ecol)
  end
  return table.concat(lines, " ")
end

local function extract_link_at_cursor()
  local node = ts_utils.get_node_at_cursor()
  while node and node:type() ~= "inline_link" do
    node = node:parent()
  end
  if not node then
    return nil, nil, nil
  end
  local linkName = nil
  local linkDest = nil
  for child in node:iter_children() do
    if child:type() == "link_text" then
      local srow,scol,erow,ecol = child:range(false)
      linkName = extract_buffer_text(srow,scol,erow,ecol)
    end
    if child:type() == "link_destination" then
      local srow,scol,erow,ecol = child:range(false)
      linkDest = extract_buffer_text(srow,scol,erow,ecol)
    end
  end
  local range = {}
  local srow,scol,erow,ecol = node:range(false)
  range.srow = srow
  range.scol = scol
  range.erow = erow
  range.ecol = ecol
  return range, linkName, linkDest
end

function m.insert_link(opts)
  local opts = opts or {}
  local range, linkName, linkValue = extract_link_at_cursor()
  vim.ui.input(
    { prompt = "Link name: ", default = linkName },
    function(link)
      if link == nil then
        return
      end
      pickers.new(opts, {
        default_text = linkValue,
        prompt_title = "Link to entry",
        finder = finders.new_oneshot_job(
          { "korr", "query", "--json" },
          {
            entry_maker = function(entry)
              local json = vim.json.decode(entry)
              local title = json.title or json.path or ""
              return {
                value = json,
                display = "<" .. json.kind .. "> [" .. json.name .. "] " .. title,
                ordinal = json.name .. title,
              }
            end
          }
        ),
        sorter = conf.generic_sorter(opts),
        previewer = previewers.new_termopen_previewer({
          get_command = function(entry, status)
            return { "korr", "info", entry.value.name }
          end
        }),
        attach_mappings = function(prompt_bufnr, map)
          actions.select_default:replace(function()
            actions.close(prompt_bufnr)
            local selection = action_state.get_selected_entry()
            local link = "[" .. link .. "](" .. selection.value.name .. ")"
            local bufnr = vim.api.nvim_get_current_buf()
            if range then
              vim.api.nvim_buf_set_text(bufnr, range.srow, range.scol, range.erow, range.ecol, { link })
            else
              local row, col = unpack(vim.api.nvim_win_get_cursor(0))
              vim.api.nvim_buf_set_text(bufnr, row - 1, col, row - 1, col, { link })
            end
          end)
          return true
        end
      }):find()
    end)
end

return m
