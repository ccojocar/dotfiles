-- Space as leader (SpaceVim style)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = vim.keymap.set

-- Files
map("n", "<Space>fs", "<cmd>w<CR>",  { desc = "Save file" })
map("n", "<Space>qq", "<cmd>qa<CR>", { desc = "Quit all" })

-- Windows (Space-w prefix)
map("n", "<Space>wh", "<C-w>h",      { desc = "Window left" })
map("n", "<Space>wj", "<C-w>j",      { desc = "Window down" })
map("n", "<Space>wk", "<C-w>k",      { desc = "Window up" })
map("n", "<Space>wl", "<C-w>l",      { desc = "Window right" })
map("n", "<Space>wv", "<C-w>v",      { desc = "Split vertical" })
map("n", "<Space>ws", "<C-w>s",      { desc = "Split horizontal" })
map("n", "<Space>wq", "<C-w>q",      { desc = "Close window" })

-- Buffers (Space-b prefix)
map("n", "<Space>bn", "<cmd>bnext<CR>",   { desc = "Next buffer" })
map("n", "<Space>bp", "<cmd>bprev<CR>",   { desc = "Prev buffer" })
map("n", "<Space>bd", "<cmd>bdelete<CR>", { desc = "Delete buffer" })

-- Copy file path to clipboard
map("n", "<Space>cp", function()
  vim.fn.setreg("+", vim.fn.expand("%:p"))
end, { desc = "Copy absolute path" })

-- Move selected lines in visual mode
map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Paste without overwriting clipboard
map("x", "<Space>p", '"_dP', { desc = "Paste without yank" })

-- Switch windows from terminal mode
map("t", "<C-w>h", "<C-\\><C-n><C-w>h", { desc = "Window left (terminal)" })
map("t", "<C-w>j", "<C-\\><C-n><C-w>j", { desc = "Window down (terminal)" })
map("t", "<C-w>k", "<C-\\><C-n><C-w>k", { desc = "Window up (terminal)" })
map("t", "<C-w>l", "<C-\\><C-n><C-w>l", { desc = "Window right (terminal)" })

-- Clear search highlight
map("n", "<Esc>", "<cmd>nohlsearch<CR>", { silent = true })
