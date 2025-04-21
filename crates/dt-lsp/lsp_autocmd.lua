-- nvim --cmd 'luafile lsp_autocmd.lua' a.dts
local augroup_id = vim.api.nvim_create_augroup("DtsLSP", {
    clear = true
});

-- TODO: https://github.com/neovim/nvim-lspconfig/blob/9c762dcd457d2ab99edb3f3433cea9865ded47ad/lua/lspconfig/configs.lua#L139-L154
-- TODO: https://github.com/neovim/nvim-lspconfig/blob/9c762dcd457d2ab99edb3f3433cea9865ded47ad/lua/lspconfig/manager.lua#L244

vim.api.nvim_create_autocmd("FileType", {
  group = augroup_id,
  pattern = {"dts", "dtsi", "dtso"},
  callback = function(args)
    print("Launching LSP")
    vim.lsp.start({
      name = 'DeviceTreeLSP',
      --cmd = {'../../target/debug/dt-lsp'},
      --cmd = {'cargo', 'run'},
      cmd = vim.lsp.rpc.connect('127.0.0.1', 9257),
      root_dir = vim.fs.root(args.buf, {'.git'}),
    })
  end
});

vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  group = augroup_id,
  pattern = {"*.h"}, -- TODO: only make .h match for included files
  callback = function(args)
    print("Launching LSP")
    vim.lsp.start({
      name = 'DeviceTreeLSP',
      --cmd = {'../../target/debug/dt-lsp'},
      --cmd = {'cargo', 'run'},
      cmd = vim.lsp.rpc.connect('127.0.0.1', 9257),
      root_dir = vim.fs.root(args.buf, {'.git'}),
    })
  end
});
