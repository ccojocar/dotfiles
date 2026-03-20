-- Language layer: Rust and Go enhancements
return {
  -- Rust: manages rust-analyzer directly; adds inlay hints,
  -- hover actions, cargo commands, and Clippy integration.
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    ft = "rust",
    init = function()
      vim.g.rustaceanvim = {
        server = {
          settings = {
            ["rust-analyzer"] = {
              checkOnSave = { command = "clippy" },
              cargo       = { allFeatures = true },
              inlayHints  = { enable = true },
              procMacro   = { enable = true },
            },
          },
        },
      }
    end,
  },

  -- Go: :GoTest, :GoRun, :GoAddTag, :GoFillStruct, etc.
  -- gopls (configured in lsp layer) handles completion/diagnostics.
  {
    "ray-x/go.nvim",
    ft           = { "go", "gomod", "gosum" },
    dependencies = { "ray-x/guihua.lua" },
    build        = ':lua require("go.install").update_all_sync()',
    config = function()
      require("go").setup({
        lsp_cfg       = false,
        lsp_on_attach = false,
        lsp_gofumpt   = true,
        dap_debug     = false,
      })

      local map = vim.keymap.set
      map("n", "<Space>gt",  "<cmd>GoTest<CR>",       { desc = "Go: run tests" })
      map("n", "<Space>gr",  "<cmd>GoRun<CR>",        { desc = "Go: run" })
      map("n", "<Space>gat", "<cmd>GoAddTag<CR>",     { desc = "Go: add struct tags" })
      map("n", "<Space>gfs", "<cmd>GoFillStruct<CR>", { desc = "Go: fill struct" })
    end,
  },
}
