-- Editor layer: syntax highlighting, fuzzy search, and AI assistance
return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").setup({
        ensure_installed = {
          "c", "lua", "rust", "go", "gomod", "gosum",
          "toml", "json", "yaml", "markdown",
        },
      })
    end,
  },

  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local t = require("telescope.builtin")
      vim.keymap.set("n", "<Space>ff", t.find_files,  { desc = "Find files" })
      vim.keymap.set("n", "<Space>fg", t.live_grep,   { desc = "Live grep" })
      vim.keymap.set("n", "<Space>fb", t.buffers,     { desc = "Buffers" })
      vim.keymap.set("n", "<Space>fh", t.help_tags,   { desc = "Help tags" })
      vim.keymap.set("n", "<Space>fr", t.oldfiles,    { desc = "Recent files" })
      vim.keymap.set("n", "<Space>fd", t.diagnostics, { desc = "Diagnostics" })
      vim.keymap.set("n", "<Space>fk", t.keymaps,     { desc = "Keymaps" })
    end,
  },

  {
    "kdheepak/lazygit.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      vim.keymap.set("n", "<Space>gg", "<cmd>LazyGit<CR>", { desc = "LazyGit" })
    end,
  },

  {
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup({
        view_options = { show_hidden = true },
      })
      vim.keymap.set("n", "<Space>fe", "<cmd>Oil<CR>", { desc = "File explorer" })
    end,
  },

  -- Claude Code integration (requires `claude` CLI in PATH)
  {
    "greggh/claude-code.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("claude-code").setup({
        window = {
          position   = "rightbelow vsplit",
          split_ratio = 0.35,
        },
      })
      -- Space-a prefix for AI
      vim.keymap.set("n", "<Space>ac", "<cmd>ClaudeCode<CR>",       { desc = "Claude Code" })
      vim.keymap.set("n", "<Space>at", "<cmd>ClaudeCodeToggle<CR>", { desc = "Claude Code toggle" })
    end,
  },
}
