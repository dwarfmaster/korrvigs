
local m = {}

local pickers = require "telescope.pickers"
local finders = require "telescope.finders"
local conf = require("telescope.config").values
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"
local previewers = require "telescope.previewers"
local prevutils = require "telescope.previewers.utils"
local telstate = require "telescope.state"

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

return m
