-- LSP layer: native vim.lsp.config (neovim 0.11+), mason, completion, formatting
return {
  -- LSP installer
  { "williamboman/mason.nvim", config = true },

  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "williamboman/mason.nvim" },
    opts = {
      -- rust_analyzer omitted: managed by rustaceanvim
      ensure_installed = { "gopls", "lua_ls" },
      automatic_enable = true, -- calls vim.lsp.enable() for installed servers
    },
  },

  -- Completion (provides capabilities used below)
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      local cmp     = require("cmp")
      local luasnip = require("luasnip")

      -- Share capabilities with all LSP servers
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      vim.lsp.config("*", { capabilities = capabilities })

      -- Go
      vim.lsp.config("gopls", {
        cmd        = { "gopls" },
        filetypes  = { "go", "gomod", "gowork", "gotmpl" },
        root_markers = { "go.work", "go.mod", ".git" },
        settings = {
          gopls = {
            analyses      = { unusedparams = true, shadow = true },
            staticcheck   = true,
            gofumpt       = true,
            codelenses    = { gc_details = true, run_govulncheck = true },
            hints = {
              assignVariableTypes    = true,
              compositeLiteralFields = true,
              functionTypeParameters = true,
              parameterNames         = true,
              rangeVariableTypes     = true,
            },
          },
        },
      })

      -- Lua (for editing this config)
      vim.lsp.config("lua_ls", {
        cmd        = { "lua-language-server" },
        filetypes  = { "lua" },
        root_markers = { ".luarc.json", ".git" },
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace   = { checkThirdParty = false },
            telemetry   = { enable = false },
          },
        },
      })

      -- LSP keymaps on attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("lsp_keymaps", { clear = true }),
        callback = function(args)
          local buf = args.buf
          local map = function(keys, fn, desc)
            vim.keymap.set("n", keys, fn, { buffer = buf, desc = desc })
          end

          map("gd",        vim.lsp.buf.definition,     "Go to definition")
          map("gD",        vim.lsp.buf.declaration,    "Go to declaration")
          map("gi",        vim.lsp.buf.implementation, "Go to implementation")
          map("gr",        vim.lsp.buf.references,     "References")
          map("K",         vim.lsp.buf.hover,          "Hover docs")
          map("<Space>ca", vim.lsp.buf.code_action,    "Code action")
          map("<Space>rn", vim.lsp.buf.rename,         "Rename symbol")
          map("<Space>ld", vim.diagnostic.open_float,  "Line diagnostics")
          map("[d",        vim.diagnostic.goto_prev,   "Prev diagnostic")
          map("]d",        vim.diagnostic.goto_next,   "Next diagnostic")
        end,
      })

      -- nvim-cmp setup
      cmp.setup({
        snippet = {
          expand = function(args) luasnip.lsp_expand(args.body) end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"]     = cmp.mapping.scroll_docs(-4),
          ["<C-f>"]     = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"]     = cmp.mapping.abort(),
          ["<CR>"]      = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "buffer" },
          { name = "path" },
        }),
      })
    end,
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts = {
      formatters_by_ft = {
        go   = { "gofumpt", "goimports" },
        rust = { "rustfmt" },
        lua  = { "stylua" },
      },
      format_on_save = { timeout_ms = 500, lsp_fallback = true },
    },
  },
}
