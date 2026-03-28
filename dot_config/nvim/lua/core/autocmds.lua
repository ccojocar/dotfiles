local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Highlight yanked text briefly
autocmd("TextYankPost", {
  group = augroup("highlight_yank", { clear = true }),
  callback = function() vim.highlight.on_yank() end,
})

-- Go uses real tabs
autocmd("FileType", {
  group = augroup("go_settings", { clear = true }),
  pattern = "go",
  callback = function()
    vim.opt_local.expandtab = false
    vim.opt_local.tabstop = 4
    vim.opt_local.shiftwidth = 4
  end,
})

-- Remove trailing whitespace on save
autocmd("BufWritePre", {
  group = augroup("trim_whitespace", { clear = true }),
  pattern = { "*.c", "*.h", "*.go", "*.rs", "*.lua" },
  callback = function()
    local pos = vim.api.nvim_win_get_cursor(0)
    vim.cmd([[%s/\s\+$//e]])
    vim.api.nvim_win_set_cursor(0, pos)
  end,
})

-- Spell checking for Markdown
autocmd("FileType", {
  group = augroup("markdown_settings", { clear = true }),
  pattern = "markdown",
  callback = function()
    vim.opt_local.spell = true
    vim.opt_local.spelllang = "en_us"
  end,
})

-- Restore last session buffers
vim.o.sessionoptions = "buffers,curdir,tabpages,winsize"

local session_file = vim.fn.stdpath("data") .. "/session.vim"

autocmd("VimLeavePre", {
  group = augroup("save_session", { clear = true }),
  callback = function()
    vim.cmd("mksession! " .. session_file)
  end,
})

autocmd("VimEnter", {
  group = augroup("restore_session", { clear = true }),
  callback = function()
    if vim.fn.argc() == 0
      and vim.fn.filereadable(session_file) == 1
    then
      vim.cmd("source " .. session_file)
    end
  end,
})
